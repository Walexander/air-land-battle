use bevy::prelude::*;
use bevy::gltf::GltfAssetLabel;
use bevy::mesh::{Indices, PrimitiveTopology};
use bevy::asset::RenderAssetUsages;
use std::time::Duration;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering;
use rand::Rng;

use crate::economy::{Economy, Harvester, HarvesterState};
use crate::map::{axial_to_world_pos, HexMapConfig, Obstacles};
use crate::selection::{create_selection_ring_mesh, create_ring_arc_mesh, InnerQuarterCircle};
use crate::launch_pads::{GameState, GameTimer, GAME_DURATION};
use crate::loading::LoadingState;

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
    pub path: Vec<(i32, i32)>,
    pub current_waypoint: usize,
    pub progress: f32,
    pub speed: f32,
    pub segment_start: (i32, i32), // The hex position where this segment started
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

// Marker to identify which infantry model this is (0, 1, or 2)
#[derive(Component)]
pub struct InfantryModelIndex {
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
    pub fn default_stats(&self) -> UnitStats {
        match self {
            UnitClass::Infantry => UnitStats {
                max_health: 100.0,
                speed: 100.0,
                armor: 50.0,
                attack: 15.0,
            },
            UnitClass::Cavalry => UnitStats {
                max_health: 80.0,
                speed: 150.0,
                armor: 30.0,
                attack: 12.0,
            },
            UnitClass::Artillery => UnitStats {
                max_health: 120.0,
                speed: 50.0,
                armor: 70.0,
                attack: 25.0,
            },
            UnitClass::Harvester => UnitStats {
                max_health: 90.0,
                speed: 40.0,
                armor: 40.0,
                attack: 5.0, // Low attack - not a combat unit
            },
        }
    }

    pub fn base_cooldown(&self) -> f32 {
        match self {
            UnitClass::Infantry => 2.0,
            UnitClass::Cavalry => 1.5,
            UnitClass::Artillery => 3.0,
            UnitClass::Harvester => 2.5,
        }
    }

    pub fn cost(&self) -> i32 {
        match self {
            UnitClass::Infantry => 40,
            UnitClass::Cavalry => 50,
            UnitClass::Artillery => 60,
            UnitClass::Harvester => 50,
        }
    }

    pub fn model_path(&self) -> &'static str {
        match self {
            UnitClass::Infantry => "walking-rifle.glb",
            UnitClass::Cavalry => "Fox.glb",
            UnitClass::Artillery => "CesiumMan.glb",
            UnitClass::Harvester => "Tractor.glb",
        }
    }

    pub fn scale(&self) -> f32 {
        match self {
            UnitClass::Infantry => 12.0,
            UnitClass::Cavalry => 0.4,  // 20% smaller (was 0.5)
            UnitClass::Artillery => 36.0,  // 20% bigger (was 30.0)
            UnitClass::Harvester => 16.0,
        }
    }

    pub fn idle_animation_index(&self) -> usize {
        match self {
            UnitClass::Infantry => 2,  // Idle animation at index 2
            UnitClass::Cavalry => 0,
            UnitClass::Artillery => 0, // CesiumMan has walking animation at index 0
            UnitClass::Harvester => 0,
        }
    }

    pub fn moving_animation_index(&self) -> usize {
        match self {
            UnitClass::Infantry => 1,  // Walking animation at index 1
            UnitClass::Cavalry => 2,
            UnitClass::Artillery => 0, // CesiumMan only has one animation at index 0
            UnitClass::Harvester => 0,
        }
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

// Pathfinding structures
#[derive(Copy, Clone, Eq, PartialEq)]
struct PathNode {
    pos: (i32, i32),
    f_score: i32,
}

impl Ord for PathNode {
    fn cmp(&self, other: &Self) -> Ordering {
        other.f_score.cmp(&self.f_score)
    }
}

impl PartialOrd for PathNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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

pub fn find_path(
    start: (i32, i32),
    goal: (i32, i32),
    map_radius: i32,
    obstacles: &HashSet<(i32, i32)>,
) -> Option<Vec<(i32, i32)>> {
    let mut open_set = BinaryHeap::new();
    let mut came_from: HashMap<(i32, i32), (i32, i32)> = HashMap::new();
    let mut g_score: HashMap<(i32, i32), i32> = HashMap::new();

    g_score.insert(start, 0);
    open_set.push(PathNode {
        pos: start,
        f_score: hex_distance(start, goal),
    });

    while let Some(PathNode { pos: current, .. }) = open_set.pop() {
        if current == goal {
            let mut path = vec![current];
            let mut current = current;
            while let Some(&prev) = came_from.get(&current) {
                path.push(prev);
                current = prev;
            }
            path.reverse();
            return Some(path);
        }

        let current_g = *g_score.get(&current).unwrap_or(&i32::MAX);

        for neighbor in hex_neighbors(current) {
            let (q, r) = neighbor;
            if q.abs() > map_radius || r.abs() > map_radius || (q + r).abs() > map_radius {
                continue;
            }

            if obstacles.contains(&neighbor) && neighbor != goal {
                continue;
            }

            let tentative_g = current_g + 1;
            let neighbor_g = *g_score.get(&neighbor).unwrap_or(&i32::MAX);

            if tentative_g < neighbor_g {
                came_from.insert(neighbor, current);
                g_score.insert(neighbor, tentative_g);
                let f_score = tentative_g + hex_distance(neighbor, goal);
                open_set.push(PathNode {
                    pos: neighbor,
                    f_score,
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
    mut query: Query<(Entity, &mut Transform, &mut Unit, &mut UnitMovement, &UnitStats, Option<&mut Combat>)>,
) {
    let current_time = time.elapsed_secs();

    // Track cells claimed THIS FRAME to prevent race conditions
    let mut cells_claimed_this_frame: HashMap<(i32, i32), Entity> = HashMap::new();

    for (entity, mut transform, mut unit, mut movement, stats, combat_opt) in &mut query {
        if movement.current_waypoint >= movement.path.len() {
            // Update last_movement_time when movement ends
            if let Some(mut combat) = combat_opt {
                combat.last_movement_time = current_time;
            }
            commands.entity(entity).remove::<UnitMovement>();
            continue;
        }

        let target_hex = movement.path[movement.current_waypoint];
        let start_hex = movement.segment_start;

        let start_pos = axial_to_world_pos(start_hex.0, start_hex.1);
        let target_pos = axial_to_world_pos(target_hex.0, target_hex.1);
        let distance = start_pos.distance(target_pos);

        let target_rotation = if distance > 0.0 {
            let direction = (target_pos - start_pos).normalize();
            let angle = direction.z.atan2(direction.x);
            Quat::from_rotation_y(-angle + std::f32::consts::PI / 2.0)
        } else {
            transform.rotation
        };

        let rotation_speed = 8.0;
        transform.rotation = transform
            .rotation
            .slerp(target_rotation, time.delta_secs() * rotation_speed);

        if distance > 0.0 {
            movement.progress += (time.delta_secs() * stats.speed) / distance;
        } else {
            movement.progress = 1.0;
        }

        // Update unit occupancy at halfway point
        if movement.progress >= 0.5 && (unit.q, unit.r) == start_hex {
            // CRITICAL: Check if target cell is occupied by another unit before claiming it
            let mut cell_occupied = false;

            // Check existing occupancy
            if let Some(&occupying_entity) = occupancy.position_to_entity.get(&target_hex)
                && occupying_entity != entity {
                    cell_occupied = true;
                }

            // Also check if another unit claimed it THIS FRAME
            if let Some(&claiming_entity) = cells_claimed_this_frame.get(&target_hex)
                && claiming_entity != entity {
                    cell_occupied = true;
                }

            if cell_occupied {
                // Target cell is occupied - reverse direction and return to start cell
                println!("⚠️  Unit {:?} stopped: destination ({}, {}) occupied, reversing to ({}, {})",
                    entity, target_hex.0, target_hex.1, start_hex.0, start_hex.1);

                // Create a reverse path back to the start cell
                movement.path = vec![start_hex];
                movement.current_waypoint = 0;
                movement.progress = 1.0 - movement.progress; // Reverse the progress
                movement.segment_start = target_hex; // We're now moving "from" target back to start

                // Update unit position to claim the start cell
                unit.q = start_hex.0;
                unit.r = start_hex.1;
                cells_claimed_this_frame.insert(start_hex, entity);

                continue; // Let the movement system handle the rest
            }

            // Claim the cell
            unit.q = target_hex.0;
            unit.r = target_hex.1;
            cells_claimed_this_frame.insert(target_hex, entity);
        }

        if movement.progress >= 1.0 {
            // Ensure unit position is correct
            unit.q = target_hex.0;
            unit.r = target_hex.1;
            transform.translation.x = target_pos.x;
            transform.translation.z = target_pos.z;

            movement.current_waypoint += 1;
            movement.progress = 0.0;

            // Update segment_start for next waypoint
            if movement.current_waypoint < movement.path.len() {
                movement.segment_start = target_hex;
            }

            if movement.current_waypoint >= movement.path.len() {
                // Update last_movement_time when movement ends
                if let Some(mut combat) = combat_opt {
                    combat.last_movement_time = current_time;
                }
                commands.entity(entity).remove::<UnitMovement>();
            }
        } else {
            let current_pos = start_pos.lerp(target_pos, movement.progress);
            transform.translation.x = current_pos.x;
            transform.translation.z = current_pos.z;
        }
    }
}

fn rotate_units_toward_enemies(
    time: Res<Time>,
    mut unit_query: Query<(Entity, &Unit, &Army, &Health, &mut Transform), Without<UnitMovement>>,
) {
    // Collect all unit positions first to avoid borrowing issues
    let units: Vec<_> = unit_query.iter().map(|(e, u, a, h, t)| {
        (e, u.q, u.r, *a, h.current, t.translation)
    }).collect();

    for (entity, q, r, army, health, pos) in &units {
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
        for (other_entity, other_q, other_r, other_army, other_health, other_pos) in &units {
            if other_entity == entity || *other_health <= 0.0 || other_army == army {
                continue;
            }

            if adjacent_hexes.contains(&(*other_q, *other_r)) {
                nearest_enemy_pos = Some(*other_pos);
                break;
            }
        }

        // Rotate toward nearest enemy if found
        if let Some(enemy_pos) = nearest_enemy_pos
            && let Ok((_, _, _, _, mut transform)) = unit_query.get_mut(*entity) {
                let direction = (enemy_pos - *pos).normalize();
                let distance = pos.distance(enemy_pos);

                if distance > 0.0 {
                    let angle = direction.z.atan2(direction.x);
                    let target_rotation = Quat::from_rotation_y(-angle + std::f32::consts::PI / 2.0);

                    // Smooth rotation over time
                    let rotation_speed = 8.0;
                    transform.rotation = transform.rotation.slerp(target_rotation, time.delta_secs() * rotation_speed);
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
    target_query: Query<&Unit, Without<Targeting>>,
    mut targeting_query: Query<(Entity, &mut Unit, &UnitStats, &mut Targeting, Option<&UnitMovement>)>,
) {
    let current_time = time.elapsed_secs();

    for (attacker_entity, mut attacker_unit, stats, mut targeting, movement_opt) in &mut targeting_query {
        // Check if target still exists
        if let Ok(target_unit) = target_query.get(targeting.target_entity) {
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
                        if movement.current_waypoint < movement.path.len() {
                            let current_cell = (attacker_unit.q, attacker_unit.r);
                            let next_cell = movement.path[movement.current_waypoint];

                            // Compare paths from current cell vs next cell
                            let path_from_current = find_path(current_cell, goal, config.map_radius, &blocking_cells);
                            let path_from_next = find_path(next_cell, goal, config.map_radius, &blocking_cells);

                            let should_reverse = match (path_from_current, path_from_next) {
                                (Some(p1), Some(p2)) => p1.len() < p2.len(),
                                (Some(_), None) => true,
                                (None, Some(_)) => false,
                                (None, None) => false,
                            };

                            if should_reverse {
                                // Reverse direction - go back to current cell and repath
                                if let Some(path) = find_path(current_cell, goal, config.map_radius, &blocking_cells) {
                                    let mut new_path = vec![current_cell];
                                    if path.len() > 1 {
                                        new_path.extend_from_slice(&path[1..]);
                                    }

                                    // Update unit position based on progress
                                    let unit_position = if movement.progress >= 0.5 {
                                        next_cell
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
                                        path: new_path,
                                        current_waypoint: 0,
                                        progress: 1.0 - movement.progress,
                                        speed: stats.speed,
                                        segment_start: next_cell,
                                    });
                                }
                            } else {
                                // Continue forward but with new goal
                                if let Some(path) = find_path(next_cell, goal, config.map_radius, &blocking_cells) {
                                    let mut new_full_path = vec![next_cell];
                                    if path.len() > 1 {
                                        new_full_path.extend_from_slice(&path[1..]);
                                    }

                                    if new_full_path.len() > 1 {
                                        commands.entity(attacker_entity).insert(UnitMovement {
                                            path: new_full_path,
                                            current_waypoint: 0,
                                            progress: movement.progress,
                                            speed: stats.speed,
                                            segment_start: movement.segment_start,
                                        });
                                    }
                                }
                            }
                        }
                    } else {
                        // Unit not moving - start new movement
                        if let Some(path) = find_path(attacker_pos, goal, config.map_radius, &blocking_cells) {
                            let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                                path[1..].to_vec()
                            } else {
                                vec![]
                            };

                            if !path_to_follow.is_empty() {
                                commands.entity(attacker_entity).insert(UnitMovement {
                                    path: path_to_follow,
                                    current_waypoint: 0,
                                    progress: 0.0,
                                    speed: stats.speed,
                                    segment_start: attacker_pos,
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

        // Respawn units by calling setup_units logic
        setup_units(commands);

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
    mut occupancy_intent: ResMut<OccupancyIntent>,
) {
    occupancy_intent.intentions.clear();
    for (entity, unit, movement_opt) in &unit_query {
        if let Some(movement) = movement_opt {
            // Unit is moving
            if movement.current_waypoint < movement.path.len() {
                if movement.progress >= 0.5 {
                    // At >= 0.5, unit already occupies current target, so intent is for next cell if it exists
                    if movement.current_waypoint + 1 < movement.path.len() {
                        let next_cell = movement.path[movement.current_waypoint + 1];
                        occupancy_intent.intentions.insert(entity, next_cell);
                    } else {
                        // No next cell, intent is current position
                        occupancy_intent
                            .intentions
                            .insert(entity, (unit.q, unit.r));
                    }
                } else {
                    // At < 0.5, intent is for the current target
                    let next_cell = movement.path[movement.current_waypoint];
                    occupancy_intent.intentions.insert(entity, next_cell);
                }
            } else {
                occupancy_intent
                    .intentions
                    .insert(entity, (unit.q, unit.r));
            }
        } else {
            // Unit is stationary - it intends to stay at its current position
            occupancy_intent
                .intentions
                .insert(entity, (unit.q, unit.r));
        }
    }
}

fn update_occupancy(
    unit_query: Query<(Entity, &Unit, Option<&UnitMovement>)>,
    mut occupancy: ResMut<Occupancy>,
) {
    occupancy.positions.clear();
    occupancy.position_to_entity.clear();
    for (entity, unit, _movement_opt) in &unit_query {
        // Unit.q/r is now updated at 0.5 progress, so it always reflects the occupied cell
        let occupied_cell = (unit.q, unit.r);
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
    mut unit_query: Query<(Entity, &mut Unit, &mut UnitMovement)>,
    occupancy: Res<Occupancy>,
    occupancy_intent: Res<OccupancyIntent>,
    obstacles: Res<Obstacles>,
    config: Res<HexMapConfig>,
) {
    let mut units_to_repath: Vec<(Entity, Unit, (i32, i32), UnitMovement)> = Vec::new();

    for (entity, unit, movement) in &unit_query {
        if movement.current_waypoint < movement.path.len() {
            let next_cell = movement.path[movement.current_waypoint];
            let current_cell = (unit.q, unit.r);

            let should_yield_to_occupied_cell =
                if let Some(&occupying_entity) = occupancy.position_to_entity.get(&next_cell) {
                    next_cell != current_cell && should_entity_yield_to(entity, occupying_entity)
                } else {
                    false
                };

            let should_yield_to_intent = movement.progress >= 0.4
                && occupancy_intent.intentions.iter().any(
                    |(other_entity, &intent_pos)| {
                        if *other_entity != entity && intent_pos == next_cell {
                            should_entity_yield_to(entity, *other_entity)
                        } else {
                            false
                        }
                    },
                );

            if should_yield_to_occupied_cell || should_yield_to_intent {
                let final_goal = *movement.path.last().unwrap();
                units_to_repath.push((entity, unit.clone(), final_goal, movement.clone()));
            }
        }
    }

    for (entity, unit, final_goal, old_movement) in units_to_repath {
        let current_cell = (unit.q, unit.r);
        let next_cell = old_movement.path[old_movement.current_waypoint];

        let mut blocking = obstacles.positions.clone();
        for &occupied_pos in &occupancy.positions {
            if occupied_pos != current_cell && occupied_pos != final_goal {
                blocking.insert(occupied_pos);
            }
        }
        for (other_entity, &intent_pos) in &occupancy_intent.intentions {
            if *other_entity != entity && intent_pos != current_cell && intent_pos != final_goal {
                blocking.insert(intent_pos);
            }
        }

        if let Some(path) = find_path(current_cell, final_goal, config.map_radius, &blocking)
            && path.len() > 1 {
                let mut new_path = vec![current_cell];
                new_path.extend_from_slice(&path[1..]);

                if new_path != old_movement.path
                    && let Ok((_, mut unit_component, mut movement)) = unit_query.get_mut(entity) {
                        // Update unit's stored position to next_cell so lerp works correctly
                        // Visual position is currently: segment_start.lerp(next_cell, progress)
                        // We want to maintain that position while reversing direction
                        // By setting unit position to next_cell and segment_start to next_cell, inverting progress:
                        // new visual = next_cell.lerp(current_cell, 1.0 - progress) = segment_start.lerp(next_cell, progress) ✓
                        unit_component.q = next_cell.0;
                        unit_component.r = next_cell.1;

                        movement.path = new_path;
                        movement.current_waypoint = 0;
                        movement.progress = 1.0 - old_movement.progress;
                        movement.segment_start = next_cell;
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
    model_query: Query<&InfantryModelIndex>,
    mut players_query: Query<(&mut AnimationPlayer, &mut AnimationTransitions)>,
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
            let model_path = unit_class.model_path();
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
            let model_path = unit_class.model_path();
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
) {
    let requests: Vec<_> = spawn_queue.requests.drain(..).collect();
    for spawn_request in requests.iter() {
        let cost = spawn_request.unit_class.cost();

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

        // Find available spawn location based on army
        // Harvesters prefer (-4, 0) and (4, 0) positions
        let spawn_candidates = match spawn_request.army {
            Army::Red => {
                if spawn_request.unit_class == UnitClass::Harvester {
                    vec![
                        (-4, 0), (-3, 1), (-4, 1), (-4, 2), (-5, 1), (-5, 2),
                        (-2, 1), (-3, 0), (-3, 2), (-2, 2),
                    ]
                } else {
                    vec![
                        (-3, 1), (-4, 1), (-4, 2), (-5, 1), (-5, 2),
                        (-2, 1), (-3, 0), (-3, 2), (-2, 2), (-4, 0),
                    ]
                }
            },
            Army::Blue => {
                if spawn_request.unit_class == UnitClass::Harvester {
                    vec![
                        (4, 0), (3, 1), (4, 1), (4, 2), (5, 1), (5, 2),
                        (2, 1), (3, 0), (3, 2), (2, 2),
                    ]
                } else {
                    vec![
                        (3, 1), (4, 1), (4, 2), (5, 1), (5, 2),
                        (2, 1), (3, 0), (3, 2), (2, 2), (4, 0),
                    ]
                }
            },
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

        let model_path = spawn_request.unit_class.model_path();
        let stats = spawn_request.unit_class.default_stats();

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
                asset_server.load(GltfAssetLabel::Animation(spawn_request.unit_class.idle_animation_index()).from_asset(model_path)),
                1.0,
                animation_graph.root,
            );
            let moving_index = animation_graph.add_clip(
                asset_server.load(GltfAssetLabel::Animation(spawn_request.unit_class.moving_animation_index()).from_asset(model_path)),
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
                    attack_cooldown: spawn_request.unit_class.base_cooldown(),
                    last_movement_time: 0.0,
                    movement_cooldown: 0.5,
                },
                Health {
                    current: stats.max_health,
                    max: stats.max_health,
                },
                Name::new(format!("{:?} {:?} ({}, {})", spawn_request.army, spawn_request.unit_class, q, r)),
            ));

            // Add Harvester component for harvester units
            if spawn_request.unit_class == UnitClass::Harvester {
                unit_entity_commands.insert(Harvester {
                    state: HarvesterState::Idle,
                    harvest_timer: 0.0,
                    harvest_duration: 10.0,
                    crystals_carried: 0,
                    crystal_accumulator: 0.0,
                    spawn_point: (q, r),
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

                if spawn_request.unit_class == UnitClass::Infantry {
                    // Infantry: spawn 3 models in triangle formation
                    let spacing = 20.0;
                    let offsets = [
                        Vec3::new(0.0, 0.0, spacing),
                        Vec3::new(-spacing, 0.0, -spacing),
                        Vec3::new(spacing, 0.0, -spacing),
                    ];
                    for (index, offset) in offsets.iter().enumerate() {
                        unit_parent.spawn((
                            SceneRoot(scene.clone()),
                            Transform::from_translation(*offset)
                                .with_scale(Vec3::splat(spawn_request.unit_class.scale())),
                            InfantryModelIndex { index },
                        ));
                    }
                } else {
                    // Other units: spawn single model at origin
                    unit_parent.spawn((
                        SceneRoot(scene),
                        Transform::default()
                            .with_scale(Vec3::splat(spawn_request.unit_class.scale())),
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

fn setup_units(mut commands: Commands) {
    // Create Red Army parent - units will be spawned dynamically via spawn_unit_from_request
    commands.spawn((
        RedArmy,
        Transform::default(),
        Visibility::default(),
        Name::new("Red Army"),
    ));

    // Create Blue Army parent - units will be spawned dynamically via spawn_unit_from_request
    commands.spawn((
        BlueArmy,
        Transform::default(),
        Visibility::default(),
        Name::new("Blue Army"),
    ));
}
fn detect_unit_clicks(
    mouse_button: Res<ButtonInput<MouseButton>>,
    camera_query: Query<(&Camera, &GlobalTransform), With<crate::ui::GameCamera>>,
    windows: Query<&Window>,
    collider_query: Query<(&UnitClickCollider, &GlobalTransform, &Mesh3d)>,
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
        base_color: Color::srgba(1.0, 1.0, 1.0, 1.0),
        emissive: LinearRgba::new(3.0, 3.0, 3.0, 1.0), // Brighter emissive
        unlit: true,
        alpha_mode: AlphaMode::Opaque,
        ..default()
    });

    // Outer ring: 5 units wide, outside main ring
    let outer_ring_mesh = meshes.add(create_selection_ring_mesh(105.0, 110.0));
    let outer_ring_material = materials.add(StandardMaterial {
        base_color: Color::srgba(1.0, 1.0, 1.0, 1.0),
        emissive: LinearRgba::new(3.0, 3.0, 3.0, 1.0), // Brighter emissive
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
        base_color: Color::srgba(1.0, 1.0, 1.0, 1.0),
        emissive: LinearRgba::new(3.0, 3.0, 3.0, 1.0), // Brighter emissive
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
            .add_systems(OnEnter(LoadingState::Playing), (setup_selection_ring_assets, setup_units).chain())
            .add_systems(
                Update,
                (
                    detect_unit_clicks,
                    clear_claimed_cells,
                    reset_game,
                    update_spawn_cooldowns,
                    spawn_unit_from_request,
                    update_targeting_system,
                    move_units,
                    rotate_units_toward_enemies,
                    combat_system,
                    handle_flash_effects,
                ).run_if(in_state(LoadingState::Playing)),
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
                    update_occupancy_intent,
                    update_occupancy,
                    detect_collisions_and_repath,
                    update_unit_animations,
                    play_animation_when_loaded,
                    update_health_bars,
                    update_collision_spheres,
                ).run_if(in_state(LoadingState::Playing)),
            );
    }
}
