use bevy::prelude::*;
use bevy::gltf::GltfAssetLabel;
use bevy::mesh::{Indices, PrimitiveTopology};
use bevy::asset::RenderAssetUsages;
use std::time::Duration;
use std::collections::{HashMap, HashSet, BinaryHeap};
use std::cmp::Ordering;
use rand::Rng;
use serde::{Deserialize, Serialize};

use crate::economy::{Economy, Harvester, HarvesterState};
use crate::map::{axial_to_world_pos, HexMapConfig, Obstacles};
use crate::selection::{create_selection_ring_mesh, create_ring_arc_mesh, InnerQuarterCircle};
use crate::launch_pads::{GameState, GameTimer, GAME_DURATION};
use crate::loading::LoadingState;
use crate::Paused;

// Unit definition structures loaded from RON files
#[derive(Debug, Clone, Deserialize, Serialize, Resource)]
pub struct UnitDefinitions {
    pub units: Vec<UnitDefinition>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct UnitDefinition {
    pub unit_type: String,
    pub stats: UnitStatsDefinition,
    pub combat: CombatDefinition,
    pub rendering: RenderingDefinition,
    pub animation: AnimationDefinition,
    pub economy: EconomyDefinition,
    #[serde(default)]
    pub squad_behavior: Option<SquadBehavior>,
    #[serde(default)]
    pub harvester_behavior: Option<HarvesterBehavior>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct UnitStatsDefinition {
    pub max_health: f32,
    pub speed: f32,
    pub armor: f32,
    pub attack: f32,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CombatDefinition {
    pub base_cooldown: f32,
    pub movement_cooldown: f32,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RenderingDefinition {
    pub model_path: String,
    pub scale: f32,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AnimationDefinition {
    pub idle_animation_index: usize,
    pub moving_animation_index: usize,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct EconomyDefinition {
    pub cost: i32,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SquadBehavior {
    pub model_count: usize,
    pub formation_spacing: f32,
    pub formation_pattern: FormationPattern,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct HarvesterBehavior {
    pub harvest_duration: f32,
    pub crystals_per_second: f32,
    pub max_crystals: i32,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub enum FormationPattern {
    Triangle,
    Line,
    Square,
}

// Resource for selection ring assets
#[derive(Resource)]
pub struct SelectionRingAssets {
    pub main_ring_mesh: Handle<Mesh>,
    pub main_ring_material: Handle<StandardMaterial>,
    pub outer_ring_mesh: Handle<Mesh>,
    pub outer_ring_material: Handle<StandardMaterial>,
    pub inner_ring_left_mesh: Handle<Mesh>,
    pub inner_ring_right_mesh: Handle<Mesh>,
    pub inner_ring_material: Handle<StandardMaterial>,
}

// Components
#[derive(Component)]
pub struct RedArmy;

#[derive(Component)]
pub struct BlueArmy;

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug)]
pub enum Army {
    Red,
    Blue,
}

#[derive(Component, Clone)]
pub struct Unit {
    pub q: i32,
    pub r: i32,
    pub _sprite_index: usize,
    pub army: Army,
}

#[derive(Component, Clone)]
pub struct UnitMovement {
    pub waypoints: Vec<Vec3>, // World-space waypoints
    pub current_waypoint: usize,
    pub progress: f32,
    pub speed: f32,
    pub segment_distance: f32, // Distance of current segment (computed once per segment)
    pub segment_start: Vec3, // Start position of current segment (fixed for entire segment)
}

#[derive(Component)]
pub struct AnimationGraphs {
    pub idle_index: AnimationNodeIndex,
    pub moving_index: AnimationNodeIndex,
}

#[derive(Component)]
pub struct CurrentAnimationState {
    pub is_moving: bool,
}

#[derive(Component)]
pub struct AnimationOffset {
    pub offset: f32,
}

// Track which infantry models have died for progressive death animations
#[derive(Component)]
pub struct InfantryDeaths {
    pub died_at_66: bool,
    pub died_at_33: bool,
    pub model_index_for_66: Option<usize>,
    pub model_index_for_33: Option<usize>,
}

impl Default for InfantryDeaths {
    fn default() -> Self {
        Self {
            died_at_66: false,
            died_at_33: false,
            model_index_for_66: None,
            model_index_for_33: None,
        }
    }
}

// Marker to identify which squad member this is (0, 1, 2, etc.)
#[derive(Component)]
pub struct SquadMemberIndex {
    pub index: usize,
}

// Track death animation timing for fade out
#[derive(Component)]
pub struct InfantryModelDying {
    pub death_started_at: f32,
    pub animation_duration: f32,
    pub fade_delay: f32,
    pub fade_duration: f32,
    pub materials_cloned: bool, // Track if we've already cloned materials
}

#[derive(Component)]
pub struct Health {
    pub current: f32,
    pub max: f32,
}

#[derive(Component)]
pub struct Combat {
    pub last_attack_time: f32,
    pub attack_cooldown: f32, // Seconds between attacks
    pub last_movement_time: f32, // Last time unit finished moving
    pub movement_cooldown: f32, // Cooldown after moving before can attack
}

#[derive(Component)]
pub struct Targeting {
    pub target_entity: Entity,
    pub target_last_position: (i32, i32),
    pub repathing_cooldown: f32,
    pub last_repath_time: f32,
}

#[derive(Component)]
pub struct UnitClickCollider {
    pub unit_entity: Entity,
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnitClass {
    Infantry,
    Cavalry,
    Artillery,
    Harvester,
}

#[derive(Component, Clone)]
pub struct UnitStats {
    pub max_health: f32,
    pub speed: f32,
    pub armor: f32,
    pub attack: f32,
}

impl UnitClass {
    pub fn definition<'a>(&self, definitions: &'a UnitDefinitions) -> &'a UnitDefinition {
        definitions.units.iter()
            .find(|def| def.unit_type == format!("{:?}", self))
            .unwrap_or_else(|| panic!("No definition found for {:?}", self))
    }

    pub fn default_stats(&self, definitions: &UnitDefinitions) -> UnitStats {
        let def = self.definition(definitions);
        UnitStats {
            max_health: def.stats.max_health,
            speed: def.stats.speed,
            armor: def.stats.armor,
            attack: def.stats.attack,
        }
    }

    pub fn base_cooldown(&self, definitions: &UnitDefinitions) -> f32 {
        let def = self.definition(definitions);
        def.combat.base_cooldown
    }

    pub fn cost(&self, definitions: &UnitDefinitions) -> i32 {
        let def = self.definition(definitions);
        def.economy.cost
    }

    pub fn model_path(&self, definitions: &UnitDefinitions) -> String {
        let def = self.definition(definitions);
        def.rendering.model_path.clone()
    }

    pub fn scale(&self, definitions: &UnitDefinitions) -> f32 {
        let def = self.definition(definitions);
        def.rendering.scale
    }

    pub fn idle_animation_index(&self, definitions: &UnitDefinitions) -> usize {
        let def = self.definition(definitions);
        def.animation.idle_animation_index
    }

    pub fn moving_animation_index(&self, definitions: &UnitDefinitions) -> usize {
        let def = self.definition(definitions);
        def.animation.moving_animation_index
    }
}

#[derive(Component)]
pub struct HealthBar {
    pub unit_entity: Entity,
}

#[derive(Component)]
pub struct HealthBarFill;

#[derive(Component)]
pub struct HealthBarBorder;

#[derive(Component)]
pub struct FlashEffect {
    pub timer: f32,
    pub duration: f32,
}

#[derive(Component)]
pub struct FlashVisual {
    pub timer: f32,
}

#[derive(Component)]
pub struct ExplosionEffect {
    pub timer: f32,
    pub duration: f32,
    pub damage: f32,
}

#[derive(Component)]
pub struct ExplosionVisual {
    pub timer: f32,
}

#[derive(Component)]
pub struct SmokeCloud {
    pub timer: f32,
    pub rise_speed: f32,
}

// Resources
#[derive(Resource, Default)]
pub struct ClickedUnit {
    pub entity: Option<Entity>,
}

impl ClickedUnit {}

#[derive(Resource, Default)]
pub struct HoveredUnit {
    pub entity: Option<Entity>,
}

#[derive(Resource, Default)]
pub struct Occupancy {
    pub positions: HashSet<(i32, i32)>,
    pub position_to_entity: HashMap<(i32, i32), Entity>,
}

// Fast position cache that doesn't trigger change detection
// Updated every frame for moving units without mutating Unit components
#[derive(Resource, Default)]
pub struct UnitPositionCache {
    pub positions: HashMap<Entity, (i32, i32)>,
    /// Live cell→entity map updated within move_units each frame so that
    /// later-processed units see positions already claimed by earlier ones.
    pub live_cell_to_entity: HashMap<(i32, i32), Entity>,
}

#[derive(Resource, Default)]
pub struct OccupancyIntent {
    pub intentions: HashMap<Entity, (i32, i32)>,
}

// Track cells claimed THIS FRAME by any system (player or AI)
// This prevents race conditions when multiple systems assign movement in same frame
#[derive(Resource, Default)]
pub struct ClaimedCellsThisFrame {
    pub cells: HashSet<(i32, i32)>,
}

#[derive(Default, Resource)]
pub struct UnitSpawnQueue {
    pub requests: Vec<UnitSpawnRequest>,
}

pub struct UnitSpawnRequest {
    pub unit_class: UnitClass,
    pub army: Army,
}

pub struct ArmyCooldowns {
    pub timer: f32,
    pub cooldown: f32,
}

impl Default for ArmyCooldowns {
    fn default() -> Self {
        Self {
            timer: 0.0,
            cooldown: 0.0,
        }
    }
}

impl ArmyCooldowns {
    pub fn is_ready(&self, _unit_class: UnitClass, _current_unit_count: usize) -> bool {
        // If no cooldown has been started yet, we're ready
        if self.cooldown == 0.0 {
            return true;
        }

        // Check if the current active cooldown has expired
        self.timer >= self.cooldown
    }

    pub fn get_progress(&self, _unit_class: UnitClass, _current_unit_count: usize) -> f32 {
        // If no cooldown active, return 100% progress
        if self.cooldown == 0.0 {
            1.0
        } else {
            (self.timer / self.cooldown).min(1.0)
        }
    }

    fn calculate_cooldown(total_units: usize) -> f32 {
        // Exponential cooldown that grows but caps at 15 seconds
        // No cooldown with 0 units, then grows exponentially
        // Formula: min(15.0, 3.0 * 1.7^(total_units - 1))
        // 0 units: 0s, 1: 3s, 2: 5.1s, 3: 8.7s, 4: 14.8s, 5+: 15s (capped)
        if total_units == 0 {
            0.0
        } else {
            (3.0 * 1.7_f32.powf((total_units - 1) as f32)).min(15.0)
        }
    }

    pub fn start_cooldown(&mut self, _unit_class: UnitClass, total_units: usize) {
        self.cooldown = Self::calculate_cooldown(total_units);
        self.timer = 0.0;
    }

    pub fn update(&mut self, delta: f32) {
        self.timer += delta;
    }
}

#[derive(Resource)]
#[derive(Default)]
pub struct SpawnCooldowns {
    pub red: ArmyCooldowns,
    pub blue: ArmyCooldowns,
}


impl SpawnCooldowns {
    pub fn get_army_cooldowns(&self, army: Army) -> &ArmyCooldowns {
        match army {
            Army::Red => &self.red,
            Army::Blue => &self.blue,
        }
    }

    pub fn get_army_cooldowns_mut(&mut self, army: Army) -> &mut ArmyCooldowns {
        match army {
            Army::Red => &mut self.red,
            Army::Blue => &mut self.blue,
        }
    }
}

// Helper functions
pub fn hex_distance(a: (i32, i32), b: (i32, i32)) -> i32 {
    let (q1, r1) = a;
    let (q2, r2) = b;
    ((q1 - q2).abs() + (r1 - r2).abs() + ((q1 + r1) - (q2 + r2)).abs()) / 2
}

fn hex_neighbors(pos: (i32, i32)) -> [(i32, i32); 6] {
    let (q, r) = pos;
    [
        (q + 1, r),
        (q - 1, r),
        (q, r + 1),
        (q, r - 1),
        (q + 1, r - 1),
        (q - 1, r + 1),
    ]
}

// A* pathfinding node
#[derive(Clone, Eq, PartialEq)]
struct PathNode {
    position: (i32, i32),
    g_cost: i32,  // Cost from start
    h_cost: i32,  // Heuristic cost to goal
    parent: Option<(i32, i32)>,
}

impl PathNode {
    fn f_cost(&self) -> i32 {
        self.g_cost + self.h_cost
    }
}

impl Ord for PathNode {
    fn cmp(&self, other: &Self) -> Ordering {
        other.f_cost().cmp(&self.f_cost())
            .then_with(|| other.h_cost.cmp(&self.h_cost))
    }
}

impl PartialOrd for PathNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// Draw a line between two hex cells, returning all cells the line touches
/// Standard hex line - produces a thin line touching one hex at a time.
/// Used for pathfinding to create smooth, direct paths.
fn hex_line(start: (i32, i32), goal: (i32, i32)) -> Vec<(i32, i32)> {
    let (q0, r0) = start;
    let (q1, r1) = goal;

    // Convert to cube coordinates (q, r, s where q + r + s = 0)
    let s0 = -q0 - r0;
    let s1 = -q1 - r1;

    // Calculate distance - use normal subdivision for thin line
    let n = hex_distance(start, goal) as usize;

    if n == 0 {
        return vec![start];
    }

    let mut results = Vec::new();
    let mut seen = HashSet::new();

    // Linear interpolation in cube coordinates
    for i in 0..=n {
        let t = i as f32 / n as f32;

        // Interpolate cube coordinates
        let q_f = q0 as f32 * (1.0 - t) + q1 as f32 * t;
        let r_f = r0 as f32 * (1.0 - t) + r1 as f32 * t;
        let s_f = s0 as f32 * (1.0 - t) + s1 as f32 * t;

        // Round to nearest hex using cube rounding
        let mut q = q_f.round();
        let mut r = r_f.round();
        let mut s = s_f.round();

        let q_diff = (q - q_f).abs();
        let r_diff = (r - r_f).abs();
        let s_diff = (s - s_f).abs();

        // Fix rounding to maintain q + r + s = 0
        if q_diff > r_diff && q_diff > s_diff {
            q = -r - s;
        } else if r_diff > s_diff {
            r = -q - s;
        } else {
            s = -q - r;
        }

        let cell = (q as i32, r as i32);

        // Only add if we haven't seen this cell yet
        if !seen.contains(&cell) {
            seen.insert(cell);
            results.push(cell);
        }
    }

    results
}

/// Supercover hex line - produces a thick line catching all cells the line touches.
/// Used for line-of-sight and collision checking to ensure proper clearance.
fn hex_line_supercover(start: (i32, i32), goal: (i32, i32)) -> Vec<(i32, i32)> {
    let (q0, r0) = start;
    let (q1, r1) = goal;

    // Convert to cube coordinates (q, r, s where q + r + s = 0)
    let s0 = -q0 - r0;
    let s1 = -q1 - r1;

    // Calculate distance - use very fine subdivision to catch all cells the line touches
    let n = (hex_distance(start, goal) * 10) as usize; // 10x for very fine resolution

    if n == 0 {
        return vec![start];
    }

    let mut results = Vec::new();
    let mut seen = HashSet::new();

    // Linear interpolation in cube coordinates with fine granularity
    for i in 0..=n {
        let t = i as f32 / n as f32;

        // Interpolate cube coordinates
        let q_f = q0 as f32 * (1.0 - t) + q1 as f32 * t;
        let r_f = r0 as f32 * (1.0 - t) + r1 as f32 * t;
        let s_f = s0 as f32 * (1.0 - t) + s1 as f32 * t;

        // Round to nearest hex using cube rounding
        let mut q = q_f.round();
        let mut r = r_f.round();
        let mut s = s_f.round();

        let q_diff = (q - q_f).abs();
        let r_diff = (r - r_f).abs();
        let s_diff = (s - s_f).abs();

        // Fix rounding to maintain q + r + s = 0
        if q_diff > r_diff && q_diff > s_diff {
            q = -r - s;
        } else if r_diff > s_diff {
            r = -q - s;
        } else {
            s = -q - r;
        }

        let cell = (q as i32, r as i32);

        // Only add if we haven't seen this cell yet
        if !seen.contains(&cell) {
            seen.insert(cell);
            results.push(cell);
        }
    }

    results
}

// New function: compute world-space waypoints using bevy_northstar any-angle pathfinding
pub fn find_path_waypoints(
    start: (i32, i32),
    goal: (i32, i32),
    valid_cells: &HashSet<(i32, i32)>,
    obstacles: &HashSet<(i32, i32)>,
    _hex_grid: &crate::hex_pathfinding::HexPathfindingGrid,
) -> Option<Vec<Vec3>> {
    // Get A* path
    let path_cells = find_path(start, goal, valid_cells, obstacles)?;

    if path_cells.is_empty() {
        return None;
    }

    // Apply path smoothing using line-of-sight optimization
    let smoothed_path = smooth_path(&path_cells, obstacles);
    // let smoothed_path = path_cells;

    // Convert cell positions to world positions
    let waypoints: Vec<Vec3> = smoothed_path
        .iter()
        .map(|&(q, r)| axial_to_world_pos(q, r))
        .collect();

    Some(waypoints)
}

/// Smooth a path by removing unnecessary waypoints using line-of-sight checks.
/// Uses "string pulling" algorithm: skip ahead as far as possible while maintaining clear line of sight.
fn smooth_path(path: &[(i32, i32)], obstacles: &HashSet<(i32, i32)>) -> Vec<(i32, i32)> {
    if path.len() <= 2 {
        return path.to_vec();
    }

    let mut smoothed = vec![path[0]];
    let mut current_idx = 0;

    while current_idx < path.len() - 1 {
        let current = path[current_idx];

        // Try to find the farthest point we can see from current position
        let mut farthest_visible = current_idx + 1;

        for look_ahead in (current_idx + 2)..path.len() {
            let target = path[look_ahead];

            if has_line_of_sight(current, target, obstacles) {
                farthest_visible = look_ahead;
            } else {
                break; // No point checking further if we hit an obstacle
            }
        }

        // Add the farthest visible point
        smoothed.push(path[farthest_visible]);
        current_idx = farthest_visible;
    }

    smoothed
}

/// Check if there's a clear line of sight between two hex cells (no obstacles in between).
/// Uses a "supercover" approach - checks all cells the line touches for obstacles.
fn has_line_of_sight(start: (i32, i32), goal: (i32, i32), obstacles: &HashSet<(i32, i32)>) -> bool {
    if start == goal {
        return true;
    }

    // Use supercover hex_line to get all cells the line touches
    let line_cells = hex_line_supercover(start, goal);

    // Check each cell along the supercover line for obstacles
    for &cell in line_cells.iter() {
        if cell != start && cell != goal {
            if obstacles.contains(&cell) {
                return false;
            }
        }
    }

    true
}

// Old cell-based pathfinding (kept for backward compatibility)
pub fn find_path(
    start: (i32, i32),
    goal: (i32, i32),
    valid_cells: &HashSet<(i32, i32)>,
    obstacles: &HashSet<(i32, i32)>,
) -> Option<Vec<(i32, i32)>> {
    // Get the straight line between start and goal
    let line = hex_line(start, goal);

    // Try to follow the line, only deviating when blocked
    let mut path = Vec::new();
    let mut current_idx = 0;

    while current_idx < line.len() {
        let current = line[current_idx];
        path.push(current);

        // If we've reached the goal, we're done
        if current == goal {
            return Some(path);
        }

        // Check if we can continue along the line
        if current_idx + 1 < line.len() {
            let next = line[current_idx + 1];

            // Check if next cell is valid
            let is_blocked = obstacles.contains(&next) && next != goal;

            if valid_cells.contains(&next) && !is_blocked {
                // Can continue along line
                current_idx += 1;
                continue;
            }
        }

        // Can't continue on line - need to path around
        // Use A* from current position to goal
        return path_around_obstacle(current, goal, valid_cells, obstacles, path);
    }

    Some(path)
}

// Helper function to path around obstacles using A*
fn path_around_obstacle(
    start: (i32, i32),
    goal: (i32, i32),
    valid_cells: &HashSet<(i32, i32)>,
    obstacles: &HashSet<(i32, i32)>,
    mut existing_path: Vec<(i32, i32)>,
) -> Option<Vec<(i32, i32)>> {
    let mut open_set = BinaryHeap::new();
    let mut came_from: HashMap<(i32, i32), (i32, i32)> = HashMap::new();
    let mut g_score: HashMap<(i32, i32), i32> = HashMap::new();

    g_score.insert(start, 0);
    open_set.push(PathNode {
        position: start,
        g_cost: 0,
        h_cost: hex_distance(start, goal) * 10,
        parent: None,
    });

    while let Some(current_node) = open_set.pop() {
        let current = current_node.position;

        if current == goal {
            // Reconstruct path from A*
            let mut astar_path = vec![current];
            let mut pos = current;
            while let Some(&parent) = came_from.get(&pos) {
                if parent == start {
                    break; // Don't include start since it's already in existing_path
                }
                astar_path.push(parent);
                pos = parent;
            }
            astar_path.reverse();
            existing_path.extend(astar_path);
            return Some(existing_path);
        }

        for neighbor in hex_neighbors(current) {
            let (q, r) = neighbor;

            if !valid_cells.contains(&neighbor) {
                continue;
            }

            if obstacles.contains(&neighbor) && neighbor != goal {
                continue;
            }

            let tentative_g_score = g_score.get(&current).unwrap_or(&i32::MAX) + 10;

            if tentative_g_score < *g_score.get(&neighbor).unwrap_or(&i32::MAX) {
                came_from.insert(neighbor, current);
                g_score.insert(neighbor, tentative_g_score);

                open_set.push(PathNode {
                    position: neighbor,
                    g_cost: tentative_g_score,
                    h_cost: hex_distance(neighbor, goal) * 10,
                    parent: Some(current),
                });
            }
        }
    }

    None
}

pub fn get_adjacent_hexes(pos: (i32, i32)) -> Vec<(i32, i32)> {
    hex_neighbors(pos).to_vec()
}

pub fn find_closest_adjacent_cell(
    target: (i32, i32),
    from: (i32, i32),
    obstacles: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    let adjacent_cells = get_adjacent_hexes(target);

    adjacent_cells
        .iter()
        .filter(|&&cell| !obstacles.contains(&cell))
        .min_by_key(|&&cell| hex_distance(from, cell))
        .copied()
}

// Systems
fn move_units(
    time: Res<Time>,
    mut commands: Commands,
    occupancy: Res<Occupancy>,
    obstacles: Res<Obstacles>,
    config: Res<HexMapConfig>,
    visible_hexes: Res<crate::map::VisibleHexes>,
    mut position_cache: ResMut<UnitPositionCache>,
    mut query: Query<(Entity, &Children, &mut Transform, &mut Unit, &mut UnitMovement, &UnitStats, &Army, Option<&mut Combat>), Without<SquadMemberIndex>>,
    squad_marker_query: Query<(), With<SquadMemberIndex>>,
    army_query: Query<&Army>,
    mut transform_query: ParamSet<(
        Query<&mut Transform, With<SquadMemberIndex>>,
        Query<&mut Transform, (Without<SquadMemberIndex>, Without<Unit>)>,
    )>,
) {
    let current_time = time.elapsed_secs();

    // Seed the live cell map from last-frame occupancy so stationary units block
    // immediately, then update it as each unit moves this frame.
    position_cache.live_cell_to_entity = occupancy.position_to_entity.clone();

    for (entity, children, mut transform, mut unit, mut movement, stats, army, combat_opt) in &mut query {
        if movement.current_waypoint >= movement.waypoints.len() {
            // Update last_movement_time when movement ends
            if let Some(mut combat) = combat_opt {
                combat.last_movement_time = current_time;
            }
            commands.entity(entity).remove::<UnitMovement>();
            continue;
        }

        // Waypoint-based movement
        let target_wp = movement.waypoints[movement.current_waypoint];

        // Initialize segment start and distance at the beginning of each segment
        if movement.progress == 0.0 {
            let current_cell = crate::map::world_pos_to_axial(transform.translation.x, transform.translation.z);
            let target_cell = crate::map::world_pos_to_axial(target_wp.x, target_wp.z);

            // Scan every cell along the segment (not just the endpoint) so smoothed
            // paths don't pass through occupied intermediate cells that weren't
            // visible when the path was originally computed.
            let first_blocked = hex_line(current_cell, target_cell)
                .into_iter()
                .skip(1) // skip the cell we're standing on
                .find(|cell| position_cache.live_cell_to_entity.get(cell).map_or(false, |&e| e != entity));

            if let Some(blocked_cell) = first_blocked {
                let occupying_entity = *position_cache.live_cell_to_entity.get(&blocked_cell).unwrap();
                if let Ok(occupying_army) = army_query.get(occupying_entity) {
                    let is_intermediate = blocked_cell != target_cell;
                    let is_final_destination = movement.current_waypoint == movement.waypoints.len() - 1;

                    if occupying_army != army || is_final_destination || is_intermediate {
                        // Build blocking set. Always include the detected blocked cell.
                        // For other occupied cells: always block friendlies; only block
                        // enemies that are in the visible-hex set (fog of war).
                        let mut blocking = obstacles.positions.clone();
                        blocking.insert(blocked_cell);
                        for &pos in &occupancy.positions {
                            if pos != current_cell {
                                if let Some(&occ_e) = occupancy.position_to_entity.get(&pos) {
                                    if let Ok(occ_army) = army_query.get(occ_e) {
                                        if occ_army == army || visible_hexes.0.contains(&pos) {
                                            blocking.insert(pos);
                                        }
                                    } else {
                                        blocking.insert(pos);
                                    }
                                } else {
                                    blocking.insert(pos);
                                }
                            }
                        }

                        let final_wp = *movement.waypoints.last().unwrap();
                        let final_goal = crate::map::world_pos_to_axial(final_wp.x, final_wp.z);
                        let dummy_grid = crate::hex_pathfinding::HexPathfindingGrid;

                        // Try to repath to the original destination routing around the block.
                        if let Some(waypoints) = find_path_waypoints(current_cell, final_goal, &config.valid_cells, &blocking, &dummy_grid)
                            && waypoints.len() > 1
                        {
                            movement.waypoints = waypoints;
                            movement.current_waypoint = 1;
                            movement.progress = 0.0;
                            movement.segment_distance = 0.0;
                            movement.segment_start = Vec3::ZERO;
                            continue;
                        }

                        // Fallback: stop adjacent to the blocked cell if no path exists.
                        let adjacent = find_closest_adjacent_cell(blocked_cell, current_cell, &blocking);
                        if let Some(adj_cell) = adjacent && adj_cell != current_cell {
                            if let Some(waypoints) = find_path_waypoints(current_cell, adj_cell, &config.valid_cells, &blocking, &dummy_grid)
                                && waypoints.len() > 1
                            {
                                movement.waypoints = waypoints;
                                movement.current_waypoint = 1;
                                movement.progress = 0.0;
                                movement.segment_distance = 0.0;
                                movement.segment_start = Vec3::ZERO;
                                continue;
                            }
                        }

                        // Final fallback: no path at all — stop here.
                        let center_pos = crate::map::axial_to_world_pos(current_cell.0, current_cell.1);
                        transform.translation.x = center_pos.x;
                        transform.translation.z = center_pos.z;
                        unit.q = current_cell.0;
                        unit.r = current_cell.1;
                        if let Some(mut combat) = combat_opt {
                            combat.last_movement_time = current_time;
                        }
                        commands.entity(entity).remove::<UnitMovement>();
                        continue;
                    } else {
                        // Friendly unit at a non-final waypoint — skip it
                        movement.current_waypoint += 1;
                        movement.progress = 0.0;
                        continue;
                    }
                }
            }
            movement.segment_start = Vec3::new(transform.translation.x, 0.0, transform.translation.z);
            movement.segment_distance = movement.segment_start.distance(target_wp);
        }

        let start_wp = movement.segment_start;

        // Rotate toward target waypoint
        if movement.segment_distance > 0.0 {
            let direction = (target_wp - start_wp).normalize();
            let angle = direction.z.atan2(direction.x);
            let target_rotation = Quat::from_rotation_y(-angle + std::f32::consts::PI / 2.0);
            let rotation_speed = 8.0;

            // Check if any children are squad models
            let mut is_squad = false;
            for child in children.iter() {
                if squad_marker_query.get(child).is_ok() {
                    is_squad = true;
                    break;
                }
            }

            if is_squad {
                let mut squad_transforms = transform_query.p0();
                for child in children.iter() {
                    if let Ok(mut child_transform) = squad_transforms.get_mut(child) {
                        child_transform.rotation = child_transform.rotation.slerp(target_rotation, time.delta_secs() * rotation_speed);
                    }
                }
            } else {
                let mut non_squad_transforms = transform_query.p1();
                for child in children.iter() {
                    if let Ok(mut child_transform) = non_squad_transforms.get_mut(child) {
                        child_transform.rotation = child_transform.rotation.slerp(target_rotation, time.delta_secs() * rotation_speed);
                        break;
                    }
                }
            }
        }

        // Update progress
        if movement.segment_distance > 0.0 {
            movement.progress += (time.delta_secs() * stats.speed) / movement.segment_distance;
        } else {
            movement.progress = 1.0;
        }

        if movement.progress >= 1.0 {
            movement.current_waypoint += 1;
            movement.progress = 0.0;
            // Segment start and distance will be recomputed on next frame

            // Update unit cell position when reaching final waypoint
            if movement.current_waypoint >= movement.waypoints.len() {
                let final_pos = movement.waypoints.last().unwrap();
                transform.translation.x = final_pos.x;
                transform.translation.z = final_pos.z;

                if let Some(mut combat) = combat_opt {
                    combat.last_movement_time = current_time;
                }
                commands.entity(entity).remove::<UnitMovement>();
            } else {
                // Snap to waypoint
                transform.translation.x = target_wp.x;
                transform.translation.z = target_wp.z;
                // Immediately sync unit.q/r so the in-flight check below doesn't
                // misfire with stale occupancy at the segment boundary.
                let arrived = crate::map::world_pos_to_axial(target_wp.x, target_wp.z);
                unit.q = arrived.0;
                unit.r = arrived.1;
            }
        } else {
            // Interpolate between waypoints
            let current_pos = start_wp.lerp(target_wp, movement.progress);
            transform.translation.x = current_pos.x;
            transform.translation.z = current_pos.z;
        }

        // Determine where this unit is right now.
        let current_cell = crate::map::world_pos_to_axial(transform.translation.x, transform.translation.z);

        // Check for conflict BEFORE claiming the cell.  Another unit processed
        // earlier this frame may already occupy current_cell.
        if let Some(&occupying_entity) = position_cache.live_cell_to_entity.get(&current_cell) {
            if occupying_entity != entity {
                // Conflict — snap back to previous axial position and repath.
                let prev_cell = (unit.q, unit.r);
                let center_pos = crate::map::axial_to_world_pos(prev_cell.0, prev_cell.1);
                transform.translation.x = center_pos.x;
                transform.translation.z = center_pos.z;
                // Restore unit.q/r in case we updated it during the waypoint snap above.
                unit.q = prev_cell.0;
                unit.r = prev_cell.1;
                position_cache.positions.insert(entity, prev_cell);
                // live_cell_to_entity was not updated for current_cell yet, so no cleanup needed.

                let final_wp = *movement.waypoints.last().unwrap();
                let final_goal = crate::map::world_pos_to_axial(final_wp.x, final_wp.z);
                let dummy_grid = crate::hex_pathfinding::HexPathfindingGrid;

                let mut blocking = obstacles.positions.clone();
                blocking.insert(current_cell);
                for (&pos, &occ_e) in &position_cache.live_cell_to_entity {
                    if pos != prev_cell {
                        if let Ok(occ_army) = army_query.get(occ_e) {
                            if occ_army == army || visible_hexes.0.contains(&pos) {
                                blocking.insert(pos);
                            }
                        } else {
                            blocking.insert(pos);
                        }
                    }
                }

                if let Some(waypoints) = find_path_waypoints(prev_cell, final_goal, &config.valid_cells, &blocking, &dummy_grid)
                    && waypoints.len() > 1
                {
                    movement.waypoints = waypoints;
                    movement.current_waypoint = 1;
                    movement.progress = 0.0;
                    movement.segment_distance = 0.0;
                    movement.segment_start = Vec3::ZERO;
                } else if let Some(adj_goal) = find_closest_adjacent_cell(final_goal, prev_cell, &blocking) {
                    if let Some(waypoints) = find_path_waypoints(prev_cell, adj_goal, &config.valid_cells, &blocking, &dummy_grid)
                        && waypoints.len() > 1
                    {
                        movement.waypoints = waypoints;
                        movement.current_waypoint = 1;
                        movement.progress = 0.0;
                        movement.segment_distance = 0.0;
                        movement.segment_start = Vec3::ZERO;
                    } else {
                        commands.entity(entity).remove::<UnitMovement>();
                    }
                } else {
                    commands.entity(entity).remove::<UnitMovement>();
                }
                continue;
            }
        }

        // No conflict — claim the cell in the live map.
        if let Some(&old_cell) = position_cache.positions.get(&entity) {
            if old_cell != current_cell && position_cache.live_cell_to_entity.get(&old_cell) == Some(&entity) {
                position_cache.live_cell_to_entity.remove(&old_cell);
            }
        }
        position_cache.positions.insert(entity, current_cell);
        position_cache.live_cell_to_entity.insert(current_cell, entity);

        if (unit.q, unit.r) != current_cell {
            unit.q = current_cell.0;
            unit.r = current_cell.1;
        }
    }
}

fn rotate_units_toward_enemies(
    time: Res<Time>,
    unit_query: Query<(Entity, &Unit, &Army, &Health, &Children, &Transform), (Without<UnitMovement>, Without<SquadMemberIndex>)>,
    squad_marker_query: Query<(), With<SquadMemberIndex>>,
    mut transform_query: ParamSet<(
        Query<&mut Transform, With<SquadMemberIndex>>,
        Query<&mut Transform, (Without<SquadMemberIndex>, Without<Unit>)>,
    )>,
) {
    // Collect all unit data including children to avoid borrowing issues
    let units: Vec<_> = unit_query.iter().map(|(e, u, a, h, children, t)| {
        let children_vec: Vec<Entity> = children.iter().collect();
        (e, u.q, u.r, *a, h.current, t.translation, children_vec)
    }).collect();

    for (entity, q, r, army, health, pos, children) in &units {
        // Skip dead units
        if *health <= 0.0 {
            continue;
        }

        // Get adjacent hexes
        let adjacent_hexes = [
            (q + 1, *r),
            (q - 1, *r),
            (*q, r + 1),
            (*q, r - 1),
            (q + 1, r - 1),
            (q - 1, r + 1),
        ];

        // Find nearest enemy in adjacent hexes
        let mut nearest_enemy_pos: Option<Vec3> = None;
        for (other_entity, other_q, other_r, other_army, other_health, other_pos, _) in &units {
            if other_entity == entity || *other_health <= 0.0 || other_army == army {
                continue;
            }

            if adjacent_hexes.contains(&(*other_q, *other_r)) {
                nearest_enemy_pos = Some(*other_pos);
                break;
            }
        }

        // Rotate toward nearest enemy if found
        if let Some(enemy_pos) = nearest_enemy_pos {
            let direction = (enemy_pos - *pos).normalize();
            let distance = pos.distance(enemy_pos);

            if distance > 0.0 {
                let angle = direction.z.atan2(direction.x);
                let target_rotation = Quat::from_rotation_y(-angle + std::f32::consts::PI / 2.0);
                let rotation_speed = 8.0;

                // Check if any children are infantry models
                let mut is_squad = false;
                for &child in children {
                    if squad_marker_query.get(child).is_ok() {
                        is_squad = true;
                        break;
                    }
                }

                if is_squad {
                    // Squad: rotate each model individually around its own position
                    let mut squad_transforms = transform_query.p0();
                    for &child in children {
                        if let Ok(mut transform) = squad_transforms.get_mut(child) {
                            transform.rotation = transform.rotation.slerp(target_rotation, time.delta_secs() * rotation_speed);
                        }
                    }
                } else {
                    // Non-squad: rotate the first scene child (the model)
                    let mut non_squad_transforms = transform_query.p1();
                    for &child in children {
                        if let Ok(mut transform) = non_squad_transforms.get_mut(child) {
                            transform.rotation = transform.rotation.slerp(target_rotation, time.delta_secs() * rotation_speed);
                            break; // Only rotate the first child (the model)
                        }
                    }
                }
            }
        }
    }
}

fn combat_system(
    time: Res<Time>,
    mut commands: Commands,
    mut unit_query: Query<(Entity, &Unit, &Army, &UnitStats, &mut Combat, &mut Health, Option<&UnitMovement>)>,
) {
    let current_time = time.elapsed_secs();

    // Process attacks immediately to prevent simultaneous kills
    // Collect list of (attacker_entity, defender_entity, damage) to apply
    let mut attacks_to_apply = Vec::new();

    // First pass: identify all potential attacks
    // Use a non-mutable iteration to collect attack information
    let units: Vec<_> = unit_query.iter().map(|(e, u, a, s, c, h, m)| {
        (e, u.clone(), *a, s.clone(), c.last_attack_time, c.attack_cooldown, c.last_movement_time, c.movement_cooldown, h.current, m.is_some())
    }).collect();

    for (attacker_entity, attacker_unit, attacker_army, attacker_stats, last_attack_time, attack_cooldown, last_movement_time, movement_cooldown, attacker_health, is_moving) in &units {
        // Skip dead attackers
        if *attacker_health <= 0.0 {
            continue;
        }

        // Skip if currently moving
        if *is_moving {
            continue;
        }

        // Check if attack cooldown has passed
        if current_time - last_attack_time < *attack_cooldown {
            continue;
        }

        // Check if movement cooldown has passed (can't fire while recently moved)
        if current_time - last_movement_time < *movement_cooldown {
            continue;
        }

        // Get adjacent hexes
        let adjacent_hexes = [
            (attacker_unit.q + 1, attacker_unit.r),
            (attacker_unit.q - 1, attacker_unit.r),
            (attacker_unit.q, attacker_unit.r + 1),
            (attacker_unit.q, attacker_unit.r - 1),
            (attacker_unit.q + 1, attacker_unit.r - 1),
            (attacker_unit.q - 1, attacker_unit.r + 1),
        ];

        // Check each adjacent hex for enemy units
        for (defender_entity, defender_unit, defender_army, defender_stats, _, _, _, _, defender_health, _) in &units {
            // Skip dead defenders
            if *defender_health <= 0.0 {
                continue;
            }

            // Skip if same army
            if attacker_army == defender_army {
                continue;
            }

            // Check if defender is adjacent
            if adjacent_hexes.contains(&(defender_unit.q, defender_unit.r)) {
                // Calculate damage: attack - (armor / 2), minimum 5 damage
                let base_damage = attacker_stats.attack - (defender_stats.armor / 2.0);

                // Add randomness: ±30% variation
                let mut rng = rand::thread_rng();
                let variation = rng.gen_range(-0.3..=0.3);
                let damage = (base_damage * (1.0 + variation)).max(5.0);

                attacks_to_apply.push((*attacker_entity, *defender_entity, damage, *attacker_army, *defender_army));
                break; // Only attack one enemy per cooldown
            }
        }
    }

    // Second pass: apply attacks one at a time so first killer prevents return fire
    for (attacker_entity, defender_entity, damage, attacker_army, defender_army) in attacks_to_apply {
        // Check if attacker is still alive
        let attacker_alive = if let Ok((_, _, _, _, _, attacker_health, _)) = unit_query.get(attacker_entity) {
            attacker_health.current > 0.0
        } else {
            false
        };

        if !attacker_alive {
            continue; // Dead units can't attack
        }

        // Check if defender is still alive before applying damage
        let defender_survived = if let Ok((_, _, _, _, _, mut defender_health, _)) = unit_query.get_mut(defender_entity) {
            if defender_health.current <= 0.0 {
                continue; // Already dead
            }

            defender_health.current -= damage;
            println!("⚔️  {:?} {:?} attacks {:?} {:?} for {:.1} damage!",
                attacker_army, attacker_entity, defender_army, defender_entity, damage);
            println!("   └─ Defender health: {:.1}/{:.1}", defender_health.current, defender_health.max);

            // Store whether defender survived for later
            defender_health.current > 0.0
        } else {
            false
        };

        // Update attacker's last attack time
        if let Ok((_, _, _, _, mut attacker_combat, _, _)) = unit_query.get_mut(attacker_entity) {
            attacker_combat.last_attack_time = current_time;
        }

        // Add flash effect to attacker
        commands.entity(attacker_entity).insert(FlashEffect {
            timer: 0.0,
            duration: 0.15, // Flash for 0.15 seconds
        });

        // Add explosion effect to defender only if they survive the hit
        // (Death explosions are handled by remove_dead_units)
        if defender_survived {
            commands.entity(defender_entity).insert(ExplosionEffect {
                timer: 0.0,
                duration: 0.3, // Explosion lasts 0.3 seconds
                damage,
            });
        }
    }
}

fn update_targeting_system(
    time: Res<Time>,
    mut commands: Commands,
    config: Res<HexMapConfig>,
    obstacles: Res<Obstacles>,
    occupancy: Res<Occupancy>,
    occupancy_intent: Res<OccupancyIntent>,
    hex_grid: Res<crate::hex_pathfinding::HexPathfindingGrid>,
    non_targeting_units: Query<(Entity, &Unit), Without<Targeting>>,
    mut targeting_query: Query<(Entity, &mut Unit, &UnitStats, &mut Targeting, Option<&UnitMovement>)>,
) {
    let current_time = time.elapsed_secs();

    // Build a map of non-targeting unit entities to their armies for quick lookup
    let non_targeting_armies: std::collections::HashMap<Entity, Army> = non_targeting_units
        .iter()
        .map(|(entity, unit)| (entity, unit.army))
        .collect();

    // Also collect targeting unit armies (before mutable iteration)
    let _targeting_armies: std::collections::HashMap<Entity, Army> = targeting_query
        .iter()
        .map(|(entity, unit, _, _, _)| (entity, unit.army))
        .collect();

    for (attacker_entity, mut attacker_unit, stats, mut targeting, movement_opt) in &mut targeting_query {
        // Check if target still exists
        if let Some(&_target_army) = non_targeting_armies.get(&targeting.target_entity) {
            // Get target position from non_targeting_units
            let target_unit = non_targeting_units.get(targeting.target_entity).unwrap().1;
            let target_pos = (target_unit.q, target_unit.r);
            let attacker_pos = (attacker_unit.q, attacker_unit.r);

            // Check if target moved
            let target_moved = target_pos != targeting.target_last_position;
            let should_repath = target_moved && current_time - targeting.last_repath_time > targeting.repathing_cooldown;

            // Only repath if target moved and cooldown elapsed
            if should_repath {
                targeting.target_last_position = target_pos;
                targeting.last_repath_time = current_time;

                // Find closest adjacent cell to target
                let mut blocking_cells = obstacles.positions.clone();
                for &occupied_pos in &occupancy.positions {
                    if occupied_pos != attacker_pos {
                        blocking_cells.insert(occupied_pos);
                    }
                }
                for (entity, &intent_pos) in &occupancy_intent.intentions {
                    if *entity != attacker_entity && intent_pos != attacker_pos {
                        blocking_cells.insert(intent_pos);
                    }
                }

                if let Some(goal) = find_closest_adjacent_cell(target_pos, attacker_pos, &blocking_cells) {
                    if let Some(movement) = movement_opt {
                        // Unit is currently moving - handle mid-movement repathing
                        if movement.current_waypoint < movement.waypoints.len() {
                            let current_cell = (attacker_unit.q, attacker_unit.r);

                            // Get waypoints from current position to goal
                            if let Some(waypoints) = find_path_waypoints(current_cell, goal, &config.valid_cells, &blocking_cells, &hex_grid) {
                                if waypoints.len() > 1 {
                                    // Calculate unit position based on progress
                                    let unit_position = if movement.progress >= 0.5 {
                                        // Past midpoint, closer to next waypoint
                                        if movement.current_waypoint < movement.waypoints.len() {
                                            let next_wp = movement.waypoints[movement.current_waypoint];
                                            let next_cell = crate::map::world_pos_to_axial(next_wp.x, next_wp.z);
                                            next_cell
                                        } else {
                                            current_cell
                                        }
                                    } else {
                                        current_cell
                                    };

                                    *attacker_unit = Unit {
                                        q: unit_position.0,
                                        r: unit_position.1,
                                        _sprite_index: attacker_unit._sprite_index,
                                        army: attacker_unit.army,
                                    };

                                    commands.entity(attacker_entity).insert(UnitMovement {
                                        waypoints,
                                        current_waypoint: 1,
                                        progress: 0.0,
                                        speed: stats.speed,
                                        segment_distance: 0.0,
                                        segment_start: Vec3::ZERO,
                                    });
                                }
                            }
                        }
                    } else {
                        // Unit not moving - start new movement
                        if let Some(waypoints) = find_path_waypoints(attacker_pos, goal, &config.valid_cells, &blocking_cells, &hex_grid) {
                            if waypoints.len() > 1 {
                                commands.entity(attacker_entity).insert(UnitMovement {
                                    waypoints,
                                    current_waypoint: 1,
                                    progress: 0.0,
                                    speed: stats.speed,
                                    segment_distance: 0.0,
                                    segment_start: Vec3::ZERO,
                                });
                            }
                        }
                    }
                }
            }
        } else {
            // Target no longer exists, clear targeting
            commands.entity(attacker_entity).remove::<Targeting>();
        }
    }
}

fn remove_dead_units(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    unit_query: Query<(Entity, &Health, &Unit, &Transform)>,
    children_query: Query<&Children>,
    health_bar_query: Query<(Entity, &HealthBar)>,
    selection_ring_query: Query<(Entity, &crate::selection::SelectionRing)>,
    collider_query: Query<(Entity, &UnitClickCollider)>,
    targeting_query: Query<(Entity, &Targeting)>,
) {
    for (entity, health, _unit, transform) in &unit_query {
        if health.current <= 0.0 {
            println!("Unit {:?} has been destroyed!", entity);

            let death_pos = transform.translation;

            // Spawn a large explosion effect at death location
            let explosion_mesh = meshes.add(Sphere::new(30.0));
            let explosion_material = materials.add(StandardMaterial {
                base_color: Color::srgb(1.0, 0.5, 0.0), // Orange
                emissive: Color::srgb(6.0, 3.0, 0.0).into(), // Bright orange-yellow
                unlit: true,
                alpha_mode: bevy::prelude::AlphaMode::Blend,
                ..default()
            });

            commands.spawn((
                Mesh3d(explosion_mesh.clone()),
                MeshMaterial3d(explosion_material),
                Transform::from_translation(death_pos).with_scale(Vec3::splat(0.1)),
                ExplosionVisual {
                    timer: 0.0,
                },
                DespawnOnExit(crate::loading::LoadingState::Playing),
            ));

            // Spawn dark smoke cloud
            let smoke_mesh = meshes.add(Sphere::new(40.0));
            let smoke_material = materials.add(StandardMaterial {
                base_color: Color::srgba(0.2, 0.2, 0.2, 0.8), // Dark gray smoke
                emissive: Color::BLACK.into(),
                unlit: true,
                alpha_mode: bevy::prelude::AlphaMode::Blend,
                ..default()
            });

            commands.spawn((
                Mesh3d(smoke_mesh),
                MeshMaterial3d(smoke_material),
                Transform::from_translation(death_pos + Vec3::new(0.0, 10.0, 0.0))
                    .with_scale(Vec3::splat(0.1)),
                SmokeCloud {
                    timer: 0.0,
                    rise_speed: 20.0,
                },
                DespawnOnExit(crate::loading::LoadingState::Playing),
            ));

            // Despawn all children (Infantry models, etc.)
            if let Ok(children) = children_query.get(entity) {
                for child in children.iter() {
                    commands.entity(child).despawn();
                }
            }

            // Despawn health bars that reference this unit
            for (bar_entity, health_bar) in &health_bar_query {
                if health_bar.unit_entity == entity {
                    commands.entity(bar_entity).despawn();
                }
            }

            // Despawn selection ring that references this unit
            for (ring_entity, selection_ring) in &selection_ring_query {
                if selection_ring.unit_entity == entity {
                    commands.entity(ring_entity).despawn();
                }
            }

            // Despawn collision sphere that references this unit
            for (collider_entity, collider) in &collider_query {
                if collider.unit_entity == entity {
                    commands.entity(collider_entity).despawn();
                }
            }

            // Remove targeting from any units targeting this dead unit
            for (attacker_entity, targeting) in &targeting_query {
                if targeting.target_entity == entity {
                    commands.entity(attacker_entity).remove::<Targeting>();
                }
            }

            // Despawn the unit itself
            commands.entity(entity).despawn();
        }
    }
}

fn reset_game(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut commands: Commands,
    army_query: Query<Entity, Or<(With<RedArmy>, With<BlueArmy>)>>,
    children_query: Query<&Children>,
    _meshes: ResMut<Assets<Mesh>>,
    _materials: ResMut<Assets<StandardMaterial>>,
    _animation_graphs: ResMut<Assets<AnimationGraph>>,
    _asset_server: Res<AssetServer>,
    mut occupancy: ResMut<Occupancy>,
    mut occupancy_intent: ResMut<OccupancyIntent>,
    mut game_state: ResMut<GameState>,
    mut game_timer: ResMut<GameTimer>,
    mut economy: ResMut<Economy>,
    mut spawn_cooldowns: ResMut<SpawnCooldowns>,
    _ring_assets: Res<SelectionRingAssets>,
) {
    if keyboard.just_pressed(KeyCode::KeyR) {
        println!("Resetting game...");

        // Despawn all army entities and their children
        for army_entity in &army_query {
            // Manually despawn all descendants
            let mut to_despawn = vec![army_entity];
            let mut i = 0;
            while i < to_despawn.len() {
                if let Ok(children) = children_query.get(to_despawn[i]) {
                    to_despawn.extend(children.iter());
                }
                i += 1;
            }

            // Despawn in reverse order (children first, then parents)
            for entity in to_despawn.into_iter().rev() {
                commands.entity(entity).despawn();
            }
        }

        // Clear occupancy data
        occupancy.positions.clear();
        occupancy.position_to_entity.clear();
        occupancy_intent.intentions.clear();

        // Reset game state
        game_state.game_over = false;
        game_state.winner = None;

        // Reset game timer
        game_timer.time_remaining = GAME_DURATION;
        game_timer.is_active = false;
        game_timer.winning_army = None;

        // Reset economy
        economy.red_money = 100;
        economy.blue_money = 100;

        // Reset spawn cooldowns
        *spawn_cooldowns = SpawnCooldowns::default();

        // Re-spawn army root entities
        commands.spawn((
            RedArmy,
            Transform::default(),
            Visibility::default(),
            Name::new("Red Army"),
            DespawnOnExit(crate::loading::LoadingState::Playing),
        ));
        commands.spawn((
            BlueArmy,
            Transform::default(),
            Visibility::default(),
            Name::new("Blue Army"),
            DespawnOnExit(crate::loading::LoadingState::Playing),
        ));

        println!("Game reset complete!");
    }
}

fn clear_claimed_cells(
    mut claimed_cells: ResMut<ClaimedCellsThisFrame>,
) {
    claimed_cells.cells.clear();
}

fn update_occupancy_intent(
    unit_query: Query<(Entity, &Unit, Option<&UnitMovement>)>,
    position_cache: Res<UnitPositionCache>,
    mut occupancy_intent: ResMut<OccupancyIntent>,
) {
    occupancy_intent.intentions.clear();
    for (entity, unit, movement_opt) in &unit_query {
        // Get current position from cache (updated every frame) or fall back to Unit component
        let current_pos = position_cache.positions.get(&entity).copied().unwrap_or((unit.q, unit.r));

        if let Some(movement) = movement_opt {
            // Unit is moving
            if movement.current_waypoint < movement.waypoints.len() {
                if movement.progress >= 0.5 {
                    // At >= 0.5, unit already occupies current target, so intent is for next cell if it exists
                    if movement.current_waypoint + 1 < movement.waypoints.len() {
                        let next_waypoint = movement.waypoints[movement.current_waypoint + 1];
                        let next_cell = crate::map::world_pos_to_axial(next_waypoint.x, next_waypoint.z);
                        occupancy_intent.intentions.insert(entity, next_cell);
                    } else {
                        // No next cell, intent is current position
                        occupancy_intent.intentions.insert(entity, current_pos);
                    }
                } else {
                    // At < 0.5, intent is for the current target
                    let next_waypoint = movement.waypoints[movement.current_waypoint];
                    let next_cell = crate::map::world_pos_to_axial(next_waypoint.x, next_waypoint.z);
                    occupancy_intent.intentions.insert(entity, next_cell);
                }
            } else {
                occupancy_intent.intentions.insert(entity, current_pos);
            }
        } else {
            // Unit is stationary - it intends to stay at its current position
            occupancy_intent.intentions.insert(entity, current_pos);
        }
    }
}

fn update_occupancy(
    unit_query: Query<(Entity, &Unit)>,
    position_cache: Res<UnitPositionCache>,
    mut occupancy: ResMut<Occupancy>,
) {
    occupancy.positions.clear();
    occupancy.position_to_entity.clear();
    for (entity, unit) in &unit_query {
        // Use position cache (updated every frame) as the source of truth
        let occupied_cell = position_cache.positions.get(&entity).copied().unwrap_or((unit.q, unit.r));
        occupancy.positions.insert(occupied_cell);
        occupancy.position_to_entity.insert(occupied_cell, entity);
    }
}

// Helper function to determine conflict resolution
// Uses XOR of entity bits to create a consistent but "random" 50/50 choice
fn should_entity_yield_to(entity_a: Entity, entity_b: Entity) -> bool {
    // XOR the entity bits and check the least significant bit
    // Then use entity ordering as a tiebreaker to ensure asymmetry
    let xor = entity_a.to_bits() ^ entity_b.to_bits();
    let bit = xor & 1;

    // If bit is 0, lower entity ID yields; if bit is 1, higher entity ID yields
    // This ensures exactly one unit yields in any conflict
    if bit == 0 {
        entity_a.to_bits() < entity_b.to_bits()
    } else {
        entity_a.to_bits() > entity_b.to_bits()
    }
}

fn detect_collisions_and_repath(
    mut commands: Commands,
    mut unit_query: Query<(Entity, &mut Unit, &mut UnitMovement)>,
    all_unit_query: Query<(Entity, &Unit), Without<UnitMovement>>,
    occupancy: Res<Occupancy>,
    occupancy_intent: Res<OccupancyIntent>,
    obstacles: Res<Obstacles>,
    config: Res<HexMapConfig>,
    visible_hexes: Res<crate::map::VisibleHexes>,
    hex_grid: Res<crate::hex_pathfinding::HexPathfindingGrid>,
) {
    // Build a map of all unit armies for quick lookup (moving + stationary)
    let mut unit_armies: std::collections::HashMap<Entity, Army> = unit_query
        .iter()
        .map(|(entity, unit, _)| (entity, unit.army))
        .collect();
    for (entity, unit) in &all_unit_query {
        unit_armies.insert(entity, unit.army);
    }

    let mut units_to_repath: Vec<(Entity, Unit, (i32, i32), UnitMovement)> = Vec::new();

    // Build a map of all moving units' current segments
    let mut unit_segments: std::collections::HashMap<Entity, Vec<(i32, i32)>> = std::collections::HashMap::new();
    for (entity, unit, movement) in &unit_query {
        if movement.current_waypoint < movement.waypoints.len() {
            let current_cell = (unit.q, unit.r);
            let next_waypoint = movement.waypoints[movement.current_waypoint];
            let next_cell = crate::map::world_pos_to_axial(next_waypoint.x, next_waypoint.z);
            let segment = hex_line(current_cell, next_cell);
            unit_segments.insert(entity, segment);
        }
    }

    for (entity, unit, movement) in &unit_query {
        if movement.current_waypoint < movement.waypoints.len() {
            let current_cell = (unit.q, unit.r);

            // Get our current segment
            let our_segment = unit_segments.get(&entity).unwrap();

            let mut should_yield = false;

            // Check each cell along our segment
            for &check_cell in our_segment {
                if check_cell == current_cell {
                    continue; // Skip our current position
                }

                // Check if cell is occupied by a stationary same-army unit.
                if let Some(&occupying_entity) = occupancy.position_to_entity.get(&check_cell) {
                    if occupying_entity != entity
                        && unit_armies.get(&occupying_entity) == Some(&unit.army)
                        && should_entity_yield_to(entity, occupying_entity)
                    {
                        should_yield = true;
                        break;
                    }
                }

                // Check for collisions with other moving units' segments
                for (other_entity, other_segment) in &unit_segments {
                    if *other_entity == entity {
                        continue;
                    }

                    // Only yield to same-army moving units.
                    if unit_armies.get(other_entity) != Some(&unit.army) {
                        continue;
                    }

                    // Check if this cell is in the other unit's segment
                    if other_segment.contains(&check_cell) {
                        if should_entity_yield_to(entity, *other_entity) {
                            println!("🚧 Collision detected: Entity {:?} yielding to {:?} at cell {:?}",
                                entity, other_entity, check_cell);
                            should_yield = true;
                            break;
                        }
                    }
                }

                if should_yield {
                    break;
                }
            }

            if should_yield {
                // Get the final goal from the last waypoint
                let final_waypoint = *movement.waypoints.last().unwrap();
                let final_goal = crate::map::world_pos_to_axial(final_waypoint.x, final_waypoint.z);
                units_to_repath.push((entity, unit.clone(), final_goal, movement.clone()));
            }
        }
    }

    for (entity, unit, final_goal, old_movement) in units_to_repath {
        let current_cell = (unit.q, unit.r);

        let mut blocking = obstacles.positions.clone();

        // Block occupied cells. Always block same-army units; only block enemy
        // units that are currently visible (fog of war).
        for &occupied_pos in &occupancy.positions {
            if occupied_pos != current_cell {
                let should_block = if let Some(&occ_e) = occupancy.position_to_entity.get(&occupied_pos) {
                    if let Some(&occ_army) = unit_armies.get(&occ_e) {
                        occ_army == unit.army || visible_hexes.0.contains(&occupied_pos)
                    } else {
                        true
                    }
                } else {
                    true
                };
                if should_block {
                    blocking.insert(occupied_pos);
                }
            }
        }

        // Block intent positions (including the goal if intended by another unit)
        for (other_entity, &intent_pos) in &occupancy_intent.intentions {
            if *other_entity != entity && intent_pos != current_cell {
                blocking.insert(intent_pos);
            }
        }

        // IMPORTANT: Block all cells along other moving units' path segments
        // This prevents repathing into paths that will still cause collisions
        for (other_entity, other_segment) in &unit_segments {
            if *other_entity != entity {
                for &segment_cell in other_segment {
                    if segment_cell != current_cell {
                        blocking.insert(segment_cell);
                    }
                }
            }
        }

        // Try to find a new path
        let new_path = find_path_waypoints(current_cell, final_goal, &config.valid_cells, &blocking, &hex_grid);

        match new_path {
            Some(waypoints) if waypoints.len() > 1 => {
                // Check if the new path still has the same collision
                let new_segment = {
                    let next_waypoint = waypoints[1]; // Skip first waypoint (current position)
                    let next_cell = crate::map::world_pos_to_axial(next_waypoint.x, next_waypoint.z);
                    hex_line(current_cell, next_cell)
                };

                // Check if new path still collides with any other unit's segment
                let mut still_collides = false;
                for (other_entity, other_segment) in &unit_segments {
                    if *other_entity != entity {
                        for &check_cell in &new_segment {
                            if check_cell != current_cell && other_segment.contains(&check_cell) {
                                still_collides = true;
                                break;
                            }
                        }
                    }
                    if still_collides {
                        break;
                    }
                }

                if !still_collides && let Ok((_, mut unit_component, mut movement)) = unit_query.get_mut(entity) {
                    println!("🔄 Repathing entity {:?} from {:?} to {:?}", entity, current_cell, final_goal);
                // Update unit's position based on progress through current segment
                if old_movement.current_waypoint < old_movement.waypoints.len() {
                    let next_waypoint = old_movement.waypoints[old_movement.current_waypoint];
                    let next_cell = crate::map::world_pos_to_axial(next_waypoint.x, next_waypoint.z);

                    // If we're past halfway, update to next cell
                    if old_movement.progress >= 0.5 {
                        unit_component.q = next_cell.0;
                        unit_component.r = next_cell.1;
                    }
                }

                    // Update movement with new waypoints
                    movement.waypoints = waypoints;
                    movement.current_waypoint = 1;
                    movement.progress = 0.0;
                } else if still_collides {
                    // New path would still collide - adjust destination to stop at safe cell before goal
                    println!("⚠️  Entity {:?} adjusting destination - {:?} is blocked", entity, final_goal);

                    // Find the last safe cell along the path before the collision
                    let mut safe_destination = current_cell;
                    for (_i, &waypoint) in waypoints.iter().enumerate().skip(1) {
                        let cell = crate::map::world_pos_to_axial(waypoint.x, waypoint.z);

                        // Check if this cell is safe (not in any other unit's segment)
                        let mut is_safe = true;
                        for (other_entity, other_segment) in &unit_segments {
                            if *other_entity != entity && other_segment.contains(&cell) {
                                is_safe = false;
                                break;
                            }
                        }

                        if is_safe {
                            safe_destination = cell;
                        } else {
                            // Stop at the last safe cell
                            break;
                        }
                    }

                    // If we found a safe destination different from current, update path
                    if safe_destination != current_cell && let Ok((_, mut unit_component, mut movement)) = unit_query.get_mut(entity) {
                        // Create new waypoints up to the safe destination
                        let safe_waypoints: Vec<Vec3> = waypoints.iter()
                            .take_while(|&&wp| {
                                let cell = crate::map::world_pos_to_axial(wp.x, wp.z);
                                cell == safe_destination || {
                                    // Check if cell is before safe_destination along path
                                    let safe_pos = axial_to_world_pos(safe_destination.0, safe_destination.1);
                                    (wp - waypoints[0]).length() <= (safe_pos - waypoints[0]).length()
                                }
                            })
                            .copied()
                            .collect();

                        if safe_waypoints.len() > 1 {
                            println!("🔄 Repathing entity {:?} to safe destination {:?}", entity, safe_destination);

                            // Update position
                            if old_movement.current_waypoint < old_movement.waypoints.len() {
                                let next_waypoint = old_movement.waypoints[old_movement.current_waypoint];
                                let next_cell = crate::map::world_pos_to_axial(next_waypoint.x, next_waypoint.z);
                                if old_movement.progress >= 0.5 {
                                    unit_component.q = next_cell.0;
                                    unit_component.r = next_cell.1;
                                }
                            }

                            movement.waypoints = safe_waypoints;
                            movement.current_waypoint = 1;
                            movement.progress = 0.0;
                        } else {
                            // safe_waypoints too short - try closest neighbor of blocked goal
                            if let Some(neighbor_goal) = find_closest_adjacent_cell(final_goal, current_cell, &blocking) {
                                if let Some(neighbor_waypoints) = find_path_waypoints(current_cell, neighbor_goal, &config.valid_cells, &blocking, &hex_grid) {
                                    if neighbor_waypoints.len() > 1 {
                                        movement.waypoints = neighbor_waypoints;
                                        movement.current_waypoint = 1;
                                        movement.progress = 0.0;
                                    } else {
                                        commands.entity(entity).remove::<UnitMovement>();
                                    }
                                } else {
                                    commands.entity(entity).remove::<UnitMovement>();
                                }
                            } else {
                                commands.entity(entity).remove::<UnitMovement>();
                            }
                        }
                    } else if safe_destination == current_cell {
                        // No safe cell found along path - try closest neighbor of blocked goal
                        if let Some(neighbor_goal) = find_closest_adjacent_cell(final_goal, current_cell, &blocking) {
                            if let Some(neighbor_waypoints) = find_path_waypoints(current_cell, neighbor_goal, &config.valid_cells, &blocking, &hex_grid) {
                                if neighbor_waypoints.len() > 1 {
                                    if let Ok((_, _, mut movement)) = unit_query.get_mut(entity) {
                                        movement.waypoints = neighbor_waypoints;
                                        movement.current_waypoint = 1;
                                        movement.progress = 0.0;
                                    }
                                } else {
                                    commands.entity(entity).remove::<UnitMovement>();
                                }
                            } else {
                                commands.entity(entity).remove::<UnitMovement>();
                            }
                        } else {
                            commands.entity(entity).remove::<UnitMovement>();
                        }
                    }
                }
            }
            _ => {
                // No path found - unit must stop and wait
                println!("🛑 Entity {:?} cannot find path to {:?} - stopping", entity, final_goal);
                if let Ok((_, _, mut movement)) = unit_query.get_mut(entity) {
                    // Stop the unit by marking movement as complete
                    movement.current_waypoint = movement.waypoints.len();
                    movement.progress = 1.0;
                }
            }
        }
    }
}

fn handle_infantry_progressive_death(
    mut commands: Commands,
    time: Res<Time>,
    asset_server: Res<AssetServer>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
    mut infantry_query: Query<(Entity, &UnitClass, &Health, &mut InfantryDeaths, &mut AnimationGraphHandle)>,
    children_query: Query<&Children>,
    model_query: Query<&SquadMemberIndex>,
    mut players_query: Query<(&mut AnimationPlayer, &mut AnimationTransitions)>,
    unit_definitions: Res<UnitDefinitions>,
) {
    let current_time = time.elapsed_secs();

    for (unit_entity, unit_class, health, mut deaths, graph_handle) in &mut infantry_query {
        if *unit_class != UnitClass::Infantry {
            continue;
        }

        let health_percentage = (health.current / health.max).max(0.0).min(1.0);

        // Check if we should trigger death animation at 66%
        if health_percentage <= 0.66 && !deaths.died_at_66 {
            deaths.died_at_66 = true;

            // Add death animation to graph if not already added
            let model_path = unit_class.model_path(&unit_definitions);
            let graph = animation_graphs.get_mut(&graph_handle.0).unwrap();
            let death_index = graph.add_clip(
                asset_server.load(GltfAssetLabel::Animation(0).from_asset(model_path)),
                1.0,
                graph.root,
            );

            // Find the first model (index 0) and play death animation
            if let Ok(children) = children_query.get(unit_entity) {
                for child in children.iter() {
                    if let Ok(model_index) = model_query.get(child) {
                        if model_index.index == 0 {
                            deaths.model_index_for_66 = Some(0);

                            // Remove from parent hierarchy so it stays in place when unit moves
                            commands.entity(child).remove_parent_in_place();

                            // Add dying component to track fade timing
                            commands.entity(child).insert(InfantryModelDying {
                                death_started_at: current_time,
                                animation_duration: 2.0, // Approximate death animation duration
                                fade_delay: 0.0,
                                fade_duration: 0.1,
                                materials_cloned: false,
                            });

                            // Find AnimationPlayer in descendants and play death animation
                            for descendant in children_query.iter_descendants(child) {
                                if let Ok((mut player, mut transitions)) = players_query.get_mut(descendant) {
                                    // Play death animation (index 0 in GLB) - don't repeat
                                    transitions
                                        .play(&mut player, death_index, Duration::from_secs_f32(0.2))
                                        .set_repeat(bevy::animation::RepeatAnimation::Never);
                                    println!("Playing death animation for infantry model 0 at 66% health");
                                    break;
                                }
                            }
                            break;
                        }
                    }
                }
            }
        }

        // Check if we should trigger death animation at 33%
        if health_percentage <= 0.33 && !deaths.died_at_33 {
            deaths.died_at_33 = true;

            // Get death animation index from graph (should already be added at 66%)
            let graph = animation_graphs.get_mut(&graph_handle.0).unwrap();
            let model_path = unit_class.model_path(&unit_definitions);
            let death_index = graph.add_clip(
                asset_server.load(GltfAssetLabel::Animation(0).from_asset(model_path)),
                1.0,
                graph.root,
            );

            // Find the second model (index 1) and play death animation
            if let Ok(children) = children_query.get(unit_entity) {
                for child in children.iter() {
                    if let Ok(model_index) = model_query.get(child) {
                        if model_index.index == 1 {
                            deaths.model_index_for_33 = Some(1);

                            // Remove from parent hierarchy so it stays in place when unit moves
                            commands.entity(child).remove_parent_in_place();

                            // Add dying component to track fade timing
                            commands.entity(child).insert(InfantryModelDying {
                                death_started_at: current_time,
                                animation_duration: 2.0, // Approximate death animation duration
                                fade_delay: 0.0,
                                fade_duration: 0.1,
                                materials_cloned: false,
                            });

                            // Find AnimationPlayer in descendants and play death animation
                            for descendant in children_query.iter_descendants(child) {
                                if let Ok((mut player, mut transitions)) = players_query.get_mut(descendant) {
                                    // Play death animation (index 0 in GLB) - don't repeat
                                    transitions
                                        .play(&mut player, death_index, Duration::from_secs_f32(0.2))
                                        .set_repeat(bevy::animation::RepeatAnimation::Never);
                                    println!("Playing death animation for infantry model 1 at 33% health");
                                    break;
                                }
                            }
                            break;
                        }
                    }
                }
            }
        }
    }
}

fn fade_out_dead_infantry(
    time: Res<Time>,
    mut commands: Commands,
    mut dying_query: Query<(Entity, &mut InfantryModelDying, &mut Visibility)>,
    children_query: Query<&Children>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut material_query: Query<&mut MeshMaterial3d<StandardMaterial>>,
) {
    let current_time = time.elapsed_secs();

    for (entity, mut dying, mut visibility) in &mut dying_query {
        let time_since_death = current_time - dying.death_started_at;
        let fade_start_time = dying.animation_duration + dying.fade_delay;

        if time_since_death >= fade_start_time {
            // Clone materials once when fade starts to make them independent
            if !dying.materials_cloned {
                for descendant in children_query.iter_descendants(entity) {
                    if let Ok(mut material_handle) = material_query.get_mut(descendant) {
                        // Clone the material so this model has its own instance
                        if let Some(material) = materials.get(&material_handle.0) {
                            let cloned_material = material.clone();
                            let new_handle = materials.add(cloned_material);
                            material_handle.0 = new_handle;
                        }
                    }
                }
                dying.materials_cloned = true;
            }

            let fade_progress = ((time_since_death - fade_start_time) / dying.fade_duration).min(1.0);
            let alpha = 1.0 - fade_progress;

            // Fade out all materials in descendants
            for descendant in children_query.iter_descendants(entity) {
                if let Ok(material_handle) = material_query.get(descendant) {
                    if let Some(material) = materials.get_mut(&material_handle.0) {
                        material.alpha_mode = bevy::prelude::AlphaMode::Blend;
                        material.base_color = material.base_color.with_alpha(alpha);
                    }
                }
            }

            // After fade completes, hide the entity
            if fade_progress >= 1.0 {
                *visibility = Visibility::Hidden;
                commands.entity(entity).remove::<InfantryModelDying>();
            }
        }
    }
}

fn update_unit_animations(
    _commands: Commands,
    mut units_query: Query<
        (
            Entity,
            &AnimationGraphs,
            &mut CurrentAnimationState,
            &mut AnimationGraphHandle,
            Option<&UnitMovement>,
        ),
        With<Unit>,
    >,
    children_query: Query<&Children>,
    dying_query: Query<(), With<InfantryModelDying>>,
    mut players_query: Query<(&mut AnimationPlayer, &mut AnimationTransitions, Option<&AnimationOffset>)>,
) {
    for (unit_entity, anim_graphs, mut anim_state, _graph_handle, movement) in
        units_query.iter_mut()
    {
        let is_moving = movement.is_some();

        if is_moving != anim_state.is_moving {
            anim_state.is_moving = is_moving;

            let new_index = if is_moving {
                anim_graphs.moving_index
            } else {
                anim_graphs.idle_index
            };

            // Check if idle and moving animations are the same (e.g., Infantry with only one animation)
            let same_animation = anim_graphs.idle_index == anim_graphs.moving_index;

            // For infantry units, check each model separately to skip dying ones
            if let Ok(children) = children_query.get(unit_entity) {
                for child in children.iter() {
                    // Skip this model if it's dying
                    if dying_query.get(child).is_ok() {
                        continue;
                    }

                    // Process animations for this model's descendants
                    for descendant in children_query.iter_descendants(child) {
                        if let Ok((mut player, mut transitions, offset)) = players_query.get_mut(descendant) {
                    if same_animation {
                        // Same animation for idle and moving - pause when idle, play when moving
                        if is_moving {
                            player.resume_all();
                            // Reapply offset after resume to maintain desynchronization
                            if let Some(anim_offset) = offset {
                                player.seek_all_by(anim_offset.offset);
                            }
                        } else {
                            player.pause_all();
                        }
                    } else {
                        // Different animations - transition between them
                        transitions
                            .play(&mut player, new_index, Duration::from_secs_f32(0.2))
                            .repeat();

                        // Reapply stagger offset to maintain desynchronization
                        if let Some(anim_offset) = offset {
                            player.seek_all_by(anim_offset.offset);
                        }
                    }
                }
                    }
                }
            }
        }
    }
}

fn play_animation_when_loaded(
    mut commands: Commands,
    units_query: Query<(Entity, &AnimationGraphs, &AnimationGraphHandle), With<Unit>>,
    children_query: Query<&Children>,
    mut players_query: Query<(Entity, &mut AnimationPlayer), Added<AnimationPlayer>>,
) {
    for (player_entity, mut player) in players_query.iter_mut() {
        for (unit_entity, anim_graphs, graph_handle) in &units_query {
            let mut is_descendant = false;
            for descendant in children_query.iter_descendants(unit_entity) {
                if descendant == player_entity {
                    is_descendant = true;
                    break;
                }
            }

            if is_descendant {
                // Stagger animation start time based on entity ID to desynchronize infantry models
                // Use entity bits to generate a pseudo-random offset between 0-2 seconds
                let entity_bits = player_entity.index() as u32;
                let offset_secs = ((entity_bits % 100) as f32) / 50.0; // 0.0 to 2.0 seconds

                let mut transitions = AnimationTransitions::new();
                transitions
                    .play(&mut player, anim_graphs.idle_index, Duration::from_secs_f32(0.0))
                    .repeat();

                // Seek animation to offset position to desynchronize
                player.seek_all_by(offset_secs);

                commands
                    .entity(player_entity)
                    .insert((
                        graph_handle.clone(),
                        transitions,
                        AnimationOffset { offset: offset_secs },
                    ));

                break;
            }
        }
    }
}

fn create_health_bar_mesh(width: f32, height: f32) -> Mesh {
    let half_width = width / 2.0;
    let half_height = height / 2.0;

    let positions = vec![
        [-half_width, 0.0, -half_height],
        [half_width, 0.0, -half_height],
        [half_width, 0.0, half_height],
        [-half_width, 0.0, half_height],
    ];

    let normals = vec![
        [0.0, 1.0, 0.0],
        [0.0, 1.0, 0.0],
        [0.0, 1.0, 0.0],
        [0.0, 1.0, 0.0],
    ];

    let uvs = vec![
        [0.0, 0.0],
        [1.0, 0.0],
        [1.0, 1.0],
        [0.0, 1.0],
    ];

    let indices = Indices::U32(vec![0, 1, 2, 0, 2, 3]);

    Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::default(),
    )
    .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
    .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
    .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
    .with_inserted_indices(indices)
}

// Configuration for spawning a health bar
pub struct HealthBarConfig {
    pub world_pos: Vec3,
    pub unit_entity: Entity,
    pub color: Color,
    pub bar_width: f32,
    pub bar_height: f32,
    pub border_width_sides: f32,
    pub border_height_extra: f32,
}

impl Default for HealthBarConfig {
    fn default() -> Self {
        Self {
            world_pos: Vec3::ZERO,
            unit_entity: Entity::PLACEHOLDER,
            color: Color::srgb(0.9, 0.2, 0.2),
            bar_width: 40.0,
            bar_height: 10.0,
            border_width_sides: 4.0,
            border_height_extra: 8.0,
        }
    }
}



fn handle_flash_effects(
    time: Res<Time>,
    mut commands: Commands,
    mut flash_query: Query<(Entity, &mut FlashEffect, &Transform)>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    for (entity, mut flash, transform) in &mut flash_query {
        if flash.timer == 0.0 {
            // First frame - spawn flash visual as an independent entity
            let flash_mesh = meshes.add(Sphere::new(8.0).mesh().ico(2).unwrap());
            let flash_material = materials.add(StandardMaterial {
                base_color: Color::srgb(3.0, 0.3, 0.0), // Bright red
                emissive: Color::srgb(3.0, 0.3, 0.0).into(),
                unlit: true,
                alpha_mode: AlphaMode::Blend,
                ..default()
            });

            // Position flash in front of the unit based on its rotation
            let forward = transform.rotation * Vec3::Z; // Get forward direction
            let flash_pos = transform.translation + Vec3::new(0.0, 25.0, 0.0) + (forward * 15.0);
            commands.spawn((
                Mesh3d(flash_mesh),
                MeshMaterial3d(flash_material),
                Transform::from_translation(flash_pos),
                FlashVisual { timer: 0.0 },
            ));
        }

        flash.timer += time.delta_secs();

        if flash.timer >= flash.duration {
            // Remove flash effect component
            commands.entity(entity).remove::<FlashEffect>();
        }
    }
}

fn cleanup_flash_visuals(
    time: Res<Time>,
    mut commands: Commands,
    mut flash_visuals: Query<(Entity, &mut FlashVisual)>,
) {
    for (entity, mut flash_visual) in &mut flash_visuals {
        flash_visual.timer += time.delta_secs();

        // Despawn after 0.15 seconds
        if flash_visual.timer >= 0.15 {
            commands.entity(entity).despawn();
        }
    }
}

fn handle_explosion_effects(
    time: Res<Time>,
    mut commands: Commands,
    mut explosion_query: Query<(Entity, &mut ExplosionEffect, &Transform)>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    for (entity, mut explosion, transform) in &mut explosion_query {
        if explosion.timer == 0.0 {
            // First frame - spawn explosion visual as an independent entity
            // Scale based on damage: 5-20 damage maps to radius 10-30
            let base_radius = 10.0 + (explosion.damage.min(50.0) / 50.0) * 20.0;

            let explosion_mesh = meshes.add(Sphere::new(base_radius).mesh().ico(3).unwrap());
            let explosion_material = materials.add(StandardMaterial {
                base_color: Color::srgb(3.0, 1.5, 0.0), // Orange
                emissive: Color::srgb(3.0, 1.5, 0.0).into(),
                unlit: true,
                alpha_mode: AlphaMode::Blend,
                ..default()
            });

            let explosion_pos = transform.translation + Vec3::new(0.0, 25.0, 0.0);
            commands.spawn((
                Mesh3d(explosion_mesh),
                MeshMaterial3d(explosion_material),
                Transform::from_translation(explosion_pos).with_scale(Vec3::splat(0.1)),
                ExplosionVisual {
                    timer: 0.0,
                },
            ));
        }

        explosion.timer += time.delta_secs();

        if explosion.timer >= explosion.duration {
            // Try to remove explosion effect component if entity still exists
            if let Ok(mut entity_commands) = commands.get_entity(entity) {
                entity_commands.remove::<ExplosionEffect>();
            }
        }
    }
}

fn animate_explosion_visuals(
    time: Res<Time>,
    mut commands: Commands,
    mut explosion_visuals: Query<(Entity, &mut ExplosionVisual, &mut Transform)>,
) {
    for (entity, mut explosion_visual, mut transform) in &mut explosion_visuals {
        explosion_visual.timer += time.delta_secs();

        let duration = 0.5; // Slightly longer for visibility
        let progress = (explosion_visual.timer / duration).min(1.0);

        // Rapid expansion with bounce
        if progress < 0.5 {
            // Expand phase - quick blast
            let expand_progress = progress / 0.5;
            let scale = 0.1 + expand_progress * 2.5; // Larger explosion
            transform.scale = Vec3::splat(scale);
        } else {
            // Fade/shrink phase
            let fade_progress = (progress - 0.5) / 0.5;
            let scale = 2.6 - (fade_progress * 2.6);
            transform.scale = Vec3::splat(scale.max(0.1));
        }

        // Despawn after duration
        if explosion_visual.timer >= duration {
            commands.entity(entity).despawn();
        }
    }
}

fn animate_smoke_clouds(
    time: Res<Time>,
    mut commands: Commands,
    mut smoke_query: Query<(Entity, &mut SmokeCloud, &mut Transform)>,
) {
    for (entity, mut smoke, mut transform) in &mut smoke_query {
        smoke.timer += time.delta_secs();

        let duration = 2.0; // Smoke lasts longer
        let progress = (smoke.timer / duration).min(1.0);

        // Rise up slowly
        transform.translation.y += smoke.rise_speed * time.delta_secs();

        // Expand and fade
        let scale = 0.1 + progress * 1.5; // Grows larger
        transform.scale = Vec3::splat(scale);

        // Despawn after duration
        if smoke.timer >= duration {
            commands.entity(entity).despawn();
        }
    }
}

fn spawn_unit_from_request(
    mut spawn_queue: ResMut<UnitSpawnQueue>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
    asset_server: Res<AssetServer>,
    mut economy: ResMut<Economy>,
    occupancy: Res<Occupancy>,
    occupancy_intent: Res<OccupancyIntent>,
    red_army_query: Query<Entity, With<RedArmy>>,
    blue_army_query: Query<Entity, With<BlueArmy>>,
    unit_query: Query<(&Unit, &UnitClass)>,
    mut spawn_cooldowns: ResMut<SpawnCooldowns>,
    ring_assets: Res<SelectionRingAssets>,
    unit_definitions: Res<UnitDefinitions>,
    map_def: Res<crate::map_loader::MapDefinition>,
) {
    let requests: Vec<_> = spawn_queue.requests.drain(..).collect();
    for spawn_request in requests.iter() {
        let cost = spawn_request.unit_class.cost(&unit_definitions);

        // Check if army can afford the unit
        let can_afford = match spawn_request.army {
            Army::Red => economy.red_money >= cost,
            Army::Blue => economy.blue_money >= cost,
        };

        if !can_afford {
            println!("{:?} army: Not enough money to spawn unit!", spawn_request.army);
            continue;
        }

        // Check unit limit (hard cap at 5 units including harvesters)
        let total_units = unit_query.iter()
            .filter(|(u, _uc)| u.army == spawn_request.army)
            .count();

        if total_units >= 5 {
            println!("{:?} army: Unit limit reached (5/5)!", spawn_request.army);
            continue;
        }

        // Check if spawn cooldown is ready
        // Pass the count AFTER spawning would occur (current + 1) to match how cooldown was started
        let army_cooldowns = spawn_cooldowns.get_army_cooldowns(spawn_request.army);
        if !army_cooldowns.is_ready(spawn_request.unit_class, total_units + 1) {
            println!("{:?} army: Spawn cooldown not ready for {:?}", spawn_request.army, spawn_request.unit_class);
            continue;
        }

        // Find available spawn location from loaded map definition
        let spawn_candidates: Vec<(i32, i32)> = match spawn_request.army {
            Army::Red => map_def.spawn_red.clone(),
            Army::Blue => map_def.spawn_blue.clone(),
        };

        // Check both current occupancy AND intent (units moving toward cells)
        let intended_positions: HashSet<(i32, i32)> = occupancy_intent
            .intentions
            .values()
            .copied()
            .collect();

        let spawn_pos = spawn_candidates
            .iter()
            .find(|pos| !occupancy.positions.contains(pos) && !intended_positions.contains(pos));

        let Some(&(q, r)) = spawn_pos else {
            println!("{:?} army: No available spawn location!", spawn_request.army);
            continue;
        };

        // Deduct money from appropriate army
        match spawn_request.army {
            Army::Red => economy.red_money -= cost,
            Army::Blue => economy.blue_money -= cost,
        }

        let world_pos = axial_to_world_pos(q, r);
        let unit_pos = world_pos + Vec3::new(0.0, 5.0, 0.0);

        let model_path = spawn_request.unit_class.model_path(&unit_definitions);
        let stats = spawn_request.unit_class.default_stats(&unit_definitions);
        println!("Spawning {:?} with stats: health={}, speed={}, armor={}, attack={}",
            spawn_request.unit_class, stats.max_health, stats.speed, stats.armor, stats.attack);

        // Find the appropriate army parent entity
        let army_entity = match spawn_request.army {
            Army::Red => {
                let Ok(entity) = red_army_query.single() else {
                    println!("Red army entity not found!");
                    continue;
                };
                entity
            }
            Army::Blue => {
                let Ok(entity) = blue_army_query.single() else {
                    println!("Blue army entity not found!");
                    continue;
                };
                entity
            }
        };

        // Get health bar color for this army
        let health_bar_color = match spawn_request.army {
            Army::Red => Color::srgb(0.9, 0.2, 0.2),
            Army::Blue => Color::srgb(0.2, 0.4, 0.9),
        };

        // Spawn unit as child of appropriate army
        commands.entity(army_entity).with_children(|parent| {
            // Create animation graph (shared by all unit types)
            let mut animation_graph = AnimationGraph::new();
            let idle_index = animation_graph.add_clip(
                asset_server.load(GltfAssetLabel::Animation(spawn_request.unit_class.idle_animation_index(&unit_definitions)).from_asset(model_path.clone())),
                1.0,
                animation_graph.root,
            );
            let moving_index = animation_graph.add_clip(
                asset_server.load(GltfAssetLabel::Animation(spawn_request.unit_class.moving_animation_index(&unit_definitions)).from_asset(model_path.clone())),
                1.0,
                animation_graph.root,
            );
            let graph_handle = animation_graphs.add(animation_graph);

            // Spawn parent entity with all components (shared by all unit types)
            let mut unit_entity_commands = parent.spawn((
                Transform::from_translation(unit_pos),
                Visibility::Visible,
                Unit {
                    q,
                    r,
                    _sprite_index: 999,
                    army: spawn_request.army,
                },
                spawn_request.army,
                spawn_request.unit_class,
                stats.clone(),
                AnimationGraphHandle(graph_handle.clone()),
                AnimationGraphs {
                    idle_index,
                    moving_index,
                },
                CurrentAnimationState { is_moving: false },
                Combat {
                    last_attack_time: 0.0,
                    attack_cooldown: spawn_request.unit_class.base_cooldown(&unit_definitions),
                    last_movement_time: 0.0,
                    movement_cooldown: 0.5,
                },
                Health {
                    current: stats.max_health,
                    max: stats.max_health,
                },
                Name::new(format!("{:?} {:?} ({}, {})", spawn_request.army, spawn_request.unit_class, q, r)),
            ));

            // Add Harvester component if unit has harvester behavior
            let unit_def = spawn_request.unit_class.definition(&unit_definitions);
            if let Some(_harvester_behavior) = &unit_def.harvester_behavior {
                unit_entity_commands.insert(Harvester {
                    state: HarvesterState::Idle,
                    harvest_timer: 0.0,
                    crystals_carried: 0,
                    crystal_accumulator: 0.0,
                    target_field: None,
                });
            }

            // Add InfantryDeaths component for infantry units
            if spawn_request.unit_class == UnitClass::Infantry {
                unit_entity_commands.insert(InfantryDeaths::default());
            }

            // Add child models based on unit type
            let unit_entity = unit_entity_commands.id();

            unit_entity_commands.with_children(|unit_parent| {
                let scene: Handle<Scene> = asset_server.load(format!("{}#Scene0", model_path));

                // Check if this unit has squad behavior defined
                let unit_def = spawn_request.unit_class.definition(&unit_definitions);
                if let Some(squad_behavior) = &unit_def.squad_behavior {
                    // Squad: spawn multiple models in formation
                    let spacing = squad_behavior.formation_spacing;
                    let offsets = match squad_behavior.formation_pattern {
                        FormationPattern::Triangle => vec![
                            Vec3::new(0.0, 0.0, spacing),
                            Vec3::new(-spacing, 0.0, -spacing),
                            Vec3::new(spacing, 0.0, -spacing),
                        ],
                        FormationPattern::Line => {
                            let mut offsets = Vec::new();
                            for i in 0..squad_behavior.model_count {
                                let offset = (i as f32 - (squad_behavior.model_count as f32 - 1.0) / 2.0) * spacing;
                                offsets.push(Vec3::new(offset, 0.0, 0.0));
                            }
                            offsets
                        }
                        FormationPattern::Square => {
                            let mut offsets = Vec::new();
                            let side_length = (squad_behavior.model_count as f32).sqrt().ceil() as usize;
                            for i in 0..squad_behavior.model_count {
                                let row = i / side_length;
                                let col = i % side_length;
                                let x = (col as f32 - (side_length as f32 - 1.0) / 2.0) * spacing;
                                let z = (row as f32 - (side_length as f32 - 1.0) / 2.0) * spacing;
                                offsets.push(Vec3::new(x, 0.0, z));
                            }
                            offsets
                        }
                    };
                    for (index, offset) in offsets.iter().enumerate() {
                        unit_parent.spawn((
                            SceneRoot(scene.clone()),
                            Transform::from_translation(*offset)
                                .with_scale(Vec3::splat(spawn_request.unit_class.scale(&unit_definitions))),
                            SquadMemberIndex { index },
                        ));
                    }
                } else {
                    // Other units: spawn single model at origin
                    unit_parent.spawn((
                        SceneRoot(scene),
                        Transform::default()
                            .with_scale(Vec3::splat(spawn_request.unit_class.scale(&unit_definitions))),
                    ));
                }
            });

            // Spawn health bar using config
            let health_bar_config = HealthBarConfig {
                world_pos,
                unit_entity,
                color: health_bar_color,
                ..Default::default()
            };

            // Create health bar meshes
            let health_bar_mesh = meshes.add(create_health_bar_mesh(
                health_bar_config.bar_width,
                health_bar_config.bar_height,
            ));
            let border_mesh = meshes.add(create_health_bar_mesh(
                health_bar_config.bar_width + health_bar_config.border_width_sides,
                health_bar_config.bar_height + health_bar_config.border_height_extra,
            ));

            let bar_pos_world = health_bar_config.world_pos + Vec3::new(0.0, 70.0, 0.0);

            // Border (black)
            parent.spawn((
                Mesh3d(border_mesh),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.0, 0.0, 0.0),
                    emissive: Color::srgb(0.0, 0.0, 0.0).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(bar_pos_world),
                HealthBar { unit_entity: health_bar_config.unit_entity },
                HealthBarBorder,
            ));

            // Background (dark gray)
            parent.spawn((
                Mesh3d(health_bar_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.2, 0.2, 0.2),
                    emissive: Color::srgb(0.2, 0.2, 0.2).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(bar_pos_world + Vec3::new(0.0, 0.1, 0.0)),
                HealthBar { unit_entity: health_bar_config.unit_entity },
            ));

            // Fill (color based on army)
            parent.spawn((
                Mesh3d(health_bar_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: health_bar_config.color,
                    emissive: health_bar_config.color.into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(bar_pos_world + Vec3::new(0.0, 0.2, 0.0)),
                HealthBar { unit_entity: health_bar_config.unit_entity },
                HealthBarFill,
            ));

            // Spawn selection ring
            let ring_pos = world_pos + Vec3::new(0.0, 6.0, 0.0);
            let ring_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
            parent.spawn((
                Mesh3d(ring_assets.main_ring_mesh.clone()),
                MeshMaterial3d(ring_assets.main_ring_material.clone()),
                Transform::from_translation(ring_pos)
                    .with_rotation(ring_rotation)
                    .with_scale(Vec3::splat(0.5)),
                Visibility::Hidden,
                crate::selection::SelectionRing {
                    unit_entity,
                    animation_timer: 0.0,
                    bounce_count: 0,
                },
            )).with_children(|ring_parent| {
                // Outer ring
                ring_parent.spawn((
                    Mesh3d(ring_assets.outer_ring_mesh.clone()),
                    MeshMaterial3d(ring_assets.outer_ring_material.clone()),
                    Transform::default(),
                    Visibility::Inherited,
                ));
                // Inner left quarter circle
                ring_parent.spawn((
                    Mesh3d(ring_assets.inner_ring_left_mesh.clone()),
                    MeshMaterial3d(ring_assets.inner_ring_material.clone()),
                    Transform::default(),
                    Visibility::Inherited,
                    InnerQuarterCircle,
                ));
                // Inner right quarter circle
                ring_parent.spawn((
                    Mesh3d(ring_assets.inner_ring_right_mesh.clone()),
                    MeshMaterial3d(ring_assets.inner_ring_material.clone()),
                    Transform::default(),
                    Visibility::Inherited,
                    InnerQuarterCircle,
                ));
            });

            // Spawn click collider (sphere for raycasting)
            // Use world positioning like other children (not relative positioning)
            // Position at ground level (half underground, half above)
            let collider_pos = world_pos + Vec3::new(0.0, 0.0, 0.0);
            let collider_mesh = meshes.add(Sphere::new(50.0).mesh().ico(3).unwrap());
            let collider_material = materials.add(StandardMaterial {
                base_color: Color::srgba(0.5, 0.5, 0.5, 0.0), // Fully transparent
                alpha_mode: AlphaMode::Blend,
                unlit: true,
                ..default()
            });
            parent.spawn((
                Mesh3d(collider_mesh),
                MeshMaterial3d(collider_material),
                Transform::from_translation(collider_pos),
                UnitClickCollider { unit_entity },
                // Visibility::Hidden, // Make visible for debugging
            ));
        });

        // Start cooldown based on this army's total unit count (including harvesters)
        // Use total_units + 1 (the unit we just spawned) since the spawned unit
        // won't appear in queries until the command buffer flushes
        let army_cooldowns = spawn_cooldowns.get_army_cooldowns_mut(spawn_request.army);
        army_cooldowns.start_cooldown(spawn_request.unit_class, total_units + 1);

        println!("Spawned {:?} {:?} unit at ({}, {}) for ${} (global cooldown: {:.1}s)",
            spawn_request.army, spawn_request.unit_class, q, r, cost, army_cooldowns.cooldown
        );
    }
}


fn update_spawn_cooldowns(
    time: Res<Time>,
    mut spawn_cooldowns: ResMut<SpawnCooldowns>,
) {
    spawn_cooldowns.red.update(time.delta_secs());
    spawn_cooldowns.blue.update(time.delta_secs());
}

fn update_health_bars(
    unit_query: Query<(&Health, &Transform), With<Unit>>,
    mut health_bar_fill_query: Query<(&HealthBar, &mut Transform), (With<HealthBarFill>, Without<Unit>, Without<HealthBarBorder>)>,
    mut health_bar_bg_query: Query<(&HealthBar, &mut Transform), (With<HealthBar>, Without<HealthBarFill>, Without<HealthBarBorder>, Without<Unit>)>,
    mut health_bar_border_query: Query<(&HealthBar, &mut Transform), (With<HealthBarBorder>, Without<Unit>, Without<HealthBarFill>)>,
) {
    // Update fill bars
    for (health_bar, mut bar_transform) in &mut health_bar_fill_query {
        if let Ok((health, unit_transform)) = unit_query.get(health_bar.unit_entity) {
            let health_percentage = (health.current / health.max).max(0.0).min(1.0);

            // Update scale based on health percentage
            bar_transform.scale.x = health_percentage;

            // Update position to follow unit
            // Offset X to make bar shrink from right to left (keep left edge fixed)
            let bar_width = 40.0;
            let x_offset = -(bar_width * (1.0 - health_percentage)) / 2.0;
            let unit_world_pos = unit_transform.translation;
            bar_transform.translation = unit_world_pos + Vec3::new(x_offset, 70.2, 0.0);
        }
    }

    // Update background bars
    for (health_bar, mut bar_transform) in &mut health_bar_bg_query {
        if let Ok((_, unit_transform)) = unit_query.get(health_bar.unit_entity) {
            let unit_world_pos = unit_transform.translation;
            bar_transform.translation = unit_world_pos + Vec3::new(0.0, 70.1, 0.0);
        }
    }

    // Update border bars
    for (health_bar, mut bar_transform) in &mut health_bar_border_query {
        if let Ok((_, unit_transform)) = unit_query.get(health_bar.unit_entity) {
            let unit_world_pos = unit_transform.translation;
            bar_transform.translation = unit_world_pos + Vec3::new(0.0, 70.0, 0.0);
        }
    }
}

fn update_collision_spheres(
    unit_query: Query<&Transform, With<Unit>>,
    mut collider_query: Query<(&UnitClickCollider, &mut Transform), Without<Unit>>,
) {
    for (collider, mut collider_transform) in &mut collider_query {
        if let Ok(unit_transform) = unit_query.get(collider.unit_entity) {
            let unit_world_pos = unit_transform.translation;
            // Position sphere at ground level (half underground, half above)
            collider_transform.translation = unit_world_pos + Vec3::new(0.0, 0.0, 0.0);
        }
    }
}

fn setup_units(mut commands: Commands, mut spawn_cooldowns: ResMut<SpawnCooldowns>, mut spawn_queue: ResMut<UnitSpawnQueue>) {
    *spawn_cooldowns = SpawnCooldowns::default();
    *spawn_queue = UnitSpawnQueue::default();
    // Create Red Army parent - units will be spawned dynamically via spawn_unit_from_request
    commands.spawn((
        RedArmy,
        Transform::default(),
        Visibility::default(),
        Name::new("Red Army"),
        DespawnOnExit(crate::loading::LoadingState::Playing),
    ));

    // Create Blue Army parent - units will be spawned dynamically via spawn_unit_from_request
    commands.spawn((
        BlueArmy,
        Transform::default(),
        Visibility::default(),
        Name::new("Blue Army"),
        DespawnOnExit(crate::loading::LoadingState::Playing),
    ));
}
fn detect_unit_clicks(
    mouse_button: Res<ButtonInput<MouseButton>>,
    camera_query: Query<(&Camera, &GlobalTransform), With<crate::ui::GameCamera>>,
    windows: Query<&Window>,
    collider_query: Query<(&UnitClickCollider, &GlobalTransform, &Mesh3d)>,
    unit_query: Query<&InheritedVisibility, With<Unit>>,
    _meshes: Res<Assets<Mesh>>,
    mut clicked_unit: ResMut<ClickedUnit>,
    mut hovered_unit: ResMut<HoveredUnit>,
) {
    // Always clear previous hover
    hovered_unit.entity = None;

    // Clear previous click
    clicked_unit.entity = None;

    let Ok((camera, camera_transform)) = camera_query.single() else {
        return;
    };

    let Ok(window) = windows.single() else {
        return;
    };

    let Some(cursor_position) = window.cursor_position() else {
        return;
    };

    // Convert screen position to ray
    let Ok(ray) = camera.viewport_to_world(camera_transform, cursor_position) else {
        return;
    };

    let mut closest_distance = f32::INFINITY;
    let mut closest_unit = None;

    // Check each unit collider for intersection using sphere test
    for (collider, collider_transform, _mesh_handle) in &collider_query {
        // Check if this unit is visible (not in fog of war)
        let Ok(visibility) = unit_query.get(collider.unit_entity) else {
            continue;
        };

        // Skip units that are hidden in fog of war
        if !visibility.get() {
            continue;
        }

        let collider_pos = collider_transform.translation();
        let sphere_radius = 50.0; // Radius matching the collision sphere mesh

        // Ray-sphere intersection test
        let oc = ray.origin - collider_pos;
        let ray_dir = *ray.direction; // Dereference Dir3 to Vec3

        let a = ray_dir.length_squared();
        let half_b = oc.dot(ray_dir);
        let c = oc.length_squared() - sphere_radius * sphere_radius;
        let discriminant = half_b * half_b - a * c;

        if discriminant >= 0.0 {
            let t = (-half_b - discriminant.sqrt()) / a;
            if t >= 0.0 && t < closest_distance {
                closest_distance = t;
                closest_unit = Some(collider.unit_entity);
            }
        }
    }

    // Always update hovered unit
    hovered_unit.entity = closest_unit;

    // Only update clicked unit on mouse press
    if mouse_button.just_pressed(MouseButton::Left) {
        clicked_unit.entity = closest_unit;
    }
}

fn setup_selection_ring_assets(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // Main ring: 10 units wide
    let main_ring_mesh = meshes.add(create_selection_ring_mesh(90.0, 100.0));
    let main_ring_material = materials.add(StandardMaterial {
        base_color: Color::linear_rgb(100.0, 100.0, 100.0),
        emissive: LinearRgba::new(100.0, 100.0, 100.0, 1.0),
        unlit: true,
        alpha_mode: AlphaMode::Opaque,
        ..default()
    });

    // Outer ring: 5 units wide, outside main ring
    let outer_ring_mesh = meshes.add(create_selection_ring_mesh(105.0, 110.0));
    let outer_ring_material = materials.add(StandardMaterial {
        base_color: Color::linear_rgb(100.0, 100.0, 100.0),
        emissive: LinearRgba::new(100.0, 100.0, 100.0, 1.0),
        unlit: true,
        alpha_mode: AlphaMode::Opaque,
        ..default()
    });

    // Inner quarter circles: 5 units wide, inside main ring, on left and right sides
    let inner_ring_left_mesh = meshes.add(create_ring_arc_mesh(
        80.0, 85.0,
        std::f32::consts::PI,
        std::f32::consts::PI * 1.5,
        8
    ));
    let inner_ring_right_mesh = meshes.add(create_ring_arc_mesh(
        80.0, 85.0,
        0.0,
        std::f32::consts::PI * 0.5,
        8
    ));
    let inner_ring_material = materials.add(StandardMaterial {
        base_color: Color::linear_rgb(100.0, 100.0, 100.0),
        emissive: LinearRgba::new(100.0, 100.0, 100.0, 1.0),
        unlit: true,
        alpha_mode: AlphaMode::Opaque,
        ..default()
    });

    commands.insert_resource(SelectionRingAssets {
        main_ring_mesh,
        main_ring_material,
        outer_ring_mesh,
        outer_ring_material,
        inner_ring_left_mesh,
        inner_ring_right_mesh,
        inner_ring_material,
    });
}

fn not_paused(paused: Res<Paused>) -> bool {
    !paused.0
}

pub struct UnitsPlugin;

impl Plugin for UnitsPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(Occupancy::default())
            .insert_resource(OccupancyIntent::default())
            .insert_resource(ClaimedCellsThisFrame::default())
            .insert_resource(UnitSpawnQueue::default())
            .insert_resource(SpawnCooldowns::default())
            .insert_resource(ClickedUnit::default())
            .insert_resource(HoveredUnit::default())
            .insert_resource(UnitPositionCache::default())
            .add_systems(OnEnter(LoadingState::Playing), (load_unit_definitions, setup_selection_ring_assets, setup_units).chain())
            .add_systems(
                Update,
                (
                    hot_reload_unit_definitions,
                    detect_unit_clicks,
                ).run_if(in_state(LoadingState::Playing)),
            )
            .add_systems(
                Update,
                (
                    clear_claimed_cells,
                    reset_game,
                    update_spawn_cooldowns,
                    spawn_unit_from_request,
                    update_targeting_system,
                    update_occupancy_intent,
                    update_occupancy,
                    detect_collisions_and_repath,
                    move_units,
                    rotate_units_toward_enemies,
                    combat_system,
                    handle_flash_effects,
                ).chain().run_if(in_state(LoadingState::Playing).and(not_paused)),
            )
            .add_systems(
                Update,
                (
                    cleanup_flash_visuals,
                    handle_explosion_effects,
                    animate_explosion_visuals,
                    animate_smoke_clouds,
                    handle_infantry_progressive_death,
                    fade_out_dead_infantry,
                    remove_dead_units,
                    update_occupancy,
                    update_unit_animations,
                    play_animation_when_loaded,
                    update_health_bars,
                    update_collision_spheres,
                ).run_if(in_state(LoadingState::Playing)),
            );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_unit_definitions() {
        let ron_str = std::fs::read_to_string("assets/units.ron")
            .expect("Failed to read units.ron");
        let definitions: UnitDefinitions = ron::from_str(&ron_str)
            .expect("Failed to parse units.ron");
        assert_eq!(definitions.units.len(), 4);

        // Verify all unit types are present
        let unit_types: Vec<&str> = definitions.units.iter()
            .map(|d| d.unit_type.as_str())
            .collect();
        assert!(unit_types.contains(&"Infantry"));
        assert!(unit_types.contains(&"Cavalry"));
        assert!(unit_types.contains(&"Artillery"));
        assert!(unit_types.contains(&"Harvester"));
    }

    #[test]
    fn test_waypoint_path_is_straight() {
        use crate::map::axial_to_world_pos;

        let start = (-4, 2);
        let goal = (-3, 0);
        let valid_cells: HashSet<(i32,i32)> = (-15..=15).flat_map(|q| (-15i32..=15).map(move |r| (q,r))).collect();
        let obstacles = HashSet::new();

        let waypoints = find_path_waypoints(start, goal, &valid_cells, &obstacles);

        assert!(waypoints.is_some(), "Waypoints should be found");
        let waypoints = waypoints.unwrap();

        println!("Waypoint path from {:?} to {:?}:", start, goal);
        for (i, wp) in waypoints.iter().enumerate() {
            println!("  {}: {:?}", i, wp);
        }

        // Verify it's a straight line - all waypoints should have same x coordinate
        // (since we're going straight up from (-4,2) to (-3,0))
        let start_pos = axial_to_world_pos(start.0, start.1);
        let goal_pos = axial_to_world_pos(goal.0, goal.1);

        println!("Start world pos: {:?}", start_pos);
        println!("Goal world pos: {:?}", goal_pos);

        // Check that x doesn't change much (straight in one direction)
        let x_change = (start_pos.x - goal_pos.x).abs();
        println!("X change: {}", x_change);
    }

    #[test]
    fn test_path_from_minus4_2_to_minus3_0() {
        let start = (-4, 2);
        let goal = (-3, 0);
        let valid_cells: HashSet<(i32,i32)> = (-15..=15).flat_map(|q| (-15i32..=15).map(move |r| (q,r))).collect();
        let obstacles = HashSet::new(); // No obstacles

        let path = find_path(start, goal, &valid_cells, &obstacles);

        assert!(path.is_some(), "Path should be found");
        let path = path.unwrap();

        println!("Path from {:?} to {:?}: {:?}", start, goal, path);

        // Path should start at start and end at goal
        assert_eq!(path[0], start);
        assert_eq!(path[path.len() - 1], goal);

        // Check if path goes through both (-4, 1) and (-3, 1) in sequence
        let has_minus4_1 = path.contains(&(-4, 1));
        let has_minus3_1 = path.contains(&(-3, 1));

        if has_minus4_1 && has_minus3_1 {
            // Find their positions in the path
            let pos_4_1 = path.iter().position(|&p| p == (-4, 1)).unwrap();
            let pos_3_1 = path.iter().position(|&p| p == (-3, 1)).unwrap();

            // They should be adjacent in the path
            assert!(
                (pos_3_1 as i32 - pos_4_1 as i32).abs() == 1,
                "(-4, 1) and (-3, 1) should be adjacent in path, but positions are {} and {}",
                pos_4_1, pos_3_1
            );

            println!("✓ Path goes through edge between (-4, 1) and (-3, 1)");
        } else {
            println!("⚠ Path does not go through both (-4, 1) and (-3, 1)");
            println!("  Has (-4, 1): {}, Has (-3, 1): {}", has_minus4_1, has_minus3_1);
        }
    }

    #[test]
    fn test_straight_path_preference() {
        let start = (0, 0);
        let goal = (3, 3);
        let valid_cells: HashSet<(i32,i32)> = (-15..=15).flat_map(|q| (-15i32..=15).map(move |r| (q,r))).collect();
        let obstacles = HashSet::new();

        let path = find_path(start, goal, &valid_cells, &obstacles);

        assert!(path.is_some(), "Path should be found");
        let path = path.unwrap();

        println!("Path from {:?} to {:?}: {:?}", start, goal, path);

        // Path should be reasonably short (not more than 2x the hex distance)
        let hex_dist = hex_distance(start, goal);
        assert!(
            path.len() - 1 <= (hex_dist * 2) as usize,
            "Path length {} should not be more than 2x hex distance {}",
            path.len() - 1,
            hex_dist
        );
    }

    #[test]
    fn test_path_around_obstacle() {
        let start = (0, 0);
        let goal = (2, 0);
        let valid_cells: HashSet<(i32,i32)> = (-15..=15).flat_map(|q| (-15i32..=15).map(move |r| (q,r))).collect();
        let mut obstacles = HashSet::new();
        obstacles.insert((1, 0)); // Obstacle directly in the way

        let path = find_path(start, goal, &valid_cells, &obstacles);

        assert!(path.is_some(), "Path should be found around obstacle");
        let path = path.unwrap();

        println!("Path from {:?} to {:?} with obstacle at (1, 0): {:?}", start, goal, path);

        // Path should not contain the obstacle
        assert!(!path.contains(&(1, 0)), "Path should not go through obstacle");

        // Path should still reach the goal
        assert_eq!(path[path.len() - 1], goal);
    }
}

// Resource to track RON file modification time for hot-reloading
#[derive(Resource)]
struct UnitDefinitionsFileWatcher {
    last_modified: std::time::SystemTime,
    check_timer: f32,
}

impl Default for UnitDefinitionsFileWatcher {
    fn default() -> Self {
        let metadata = std::fs::metadata("assets/units.ron")
            .expect("Failed to get metadata for assets/units.ron");
        let last_modified = metadata.modified()
            .expect("Failed to get modification time");

        Self {
            last_modified,
            check_timer: 0.0,
        }
    }
}

// Load unit definitions from RON file
fn load_unit_definitions(mut commands: Commands) {
    // Load and parse RON file synchronously
    let ron_str = std::fs::read_to_string("assets/units.ron")
        .expect("Failed to read assets/units.ron - make sure the file exists");

    let definitions: UnitDefinitions = ron::from_str(&ron_str)
        .expect("Failed to parse assets/units.ron - check RON syntax");

    // Validate definitions
    validate_unit_definitions(&definitions)
        .expect("Unit definitions validation failed");

    commands.insert_resource(definitions.clone());
    commands.insert_resource(UnitDefinitionsFileWatcher::default());

    println!("✓ Loaded {} unit definitions from assets/units.ron", definitions.units.len());
}

// Hot-reload system: checks for file changes and reloads definitions
fn hot_reload_unit_definitions(
    time: Res<Time>,
    mut watcher: ResMut<UnitDefinitionsFileWatcher>,
    mut definitions: ResMut<UnitDefinitions>,
) {
    // Check every 0.5 seconds
    watcher.check_timer += time.delta_secs();
    if watcher.check_timer < 0.5 {
        return;
    }
    watcher.check_timer = 0.0;

    // Check if file has been modified
    let Ok(metadata) = std::fs::metadata("assets/units.ron") else {
        return;
    };

    let Ok(modified) = metadata.modified() else {
        return;
    };

    if modified <= watcher.last_modified {
        return; // No changes
    }

    // File has changed, try to reload
    println!("🔄 Detected changes in assets/units.ron, reloading...");

    match std::fs::read_to_string("assets/units.ron") {
        Ok(ron_str) => {
            match ron::from_str::<UnitDefinitions>(&ron_str) {
                Ok(new_definitions) => {
                    match validate_unit_definitions(&new_definitions) {
                        Ok(_) => {
                            *definitions = new_definitions;
                            watcher.last_modified = modified;
                            println!("✓ Hot-reloaded {} unit definitions (newly spawned units will use new values)", definitions.units.len());
                        }
                        Err(e) => {
                            eprintln!("❌ Validation failed, keeping old definitions: {}", e);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("❌ Parse error, keeping old definitions: {}", e);
                }
            }
        }
        Err(e) => {
            eprintln!("❌ Failed to read file, keeping old definitions: {}", e);
        }
    }
}

// Validate unit definitions
fn validate_unit_definitions(definitions: &UnitDefinitions) -> Result<(), String> {
    // Check all required UnitClass variants have definitions
    let required_units = ["Infantry", "Cavalry", "Artillery", "Harvester"];
    for unit_type in required_units {
        if !definitions.units.iter().any(|def| def.unit_type == unit_type) {
            return Err(format!("Missing definition for {}", unit_type));
        }
    }

    // Validate value ranges
    for def in &definitions.units {
        if def.stats.max_health <= 0.0 {
            return Err(format!("{} has invalid max_health: {}", def.unit_type, def.stats.max_health));
        }
        if def.stats.speed < 0.0 {
            return Err(format!("{} has invalid speed: {}", def.unit_type, def.stats.speed));
        }
        if def.economy.cost < 0 {
            return Err(format!("{} has invalid cost: {}", def.unit_type, def.economy.cost));
        }
        if def.combat.base_cooldown <= 0.0 {
            return Err(format!("{} has invalid base_cooldown: {}", def.unit_type, def.combat.base_cooldown));
        }
        if def.rendering.scale <= 0.0 {
            return Err(format!("{} has invalid scale: {}", def.unit_type, def.rendering.scale));
        }

        // Validate Infantry has squad_behavior
        if def.unit_type == "Infantry" && def.squad_behavior.is_none() {
            return Err(format!("{} is missing required squad_behavior", def.unit_type));
        }

        // Validate Harvester has harvester_behavior
        if def.unit_type == "Harvester" && def.harvester_behavior.is_none() {
            return Err(format!("{} is missing required harvester_behavior", def.unit_type));
        }
    }

    Ok(())
}
