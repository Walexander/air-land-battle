use bevy::prelude::*;
use bevy::gltf::GltfAssetLabel;
use bevy::mesh::{Indices, PrimitiveTopology};
use bevy::asset::RenderAssetUsages;
use std::time::Duration;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering;
use rand::Rng;

use crate::map::{axial_to_world_pos, HexMapConfig, Obstacles, CrystalField};
use crate::selection::{SelectionRing, create_selection_ring_mesh};
use crate::launch_pads::{GameState, GameTimer, LaunchPads, LaunchPadOwnership, LaunchPadOwner};
use crate::loading::LoadingState;

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
    pub idle_graph: Handle<AnimationGraph>,
    pub idle_index: AnimationNodeIndex,
    pub moving_graph: Handle<AnimationGraph>,
    pub moving_index: AnimationNodeIndex,
}

#[derive(Component)]
pub struct CurrentAnimationState {
    pub is_moving: bool,
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

#[derive(Component, Clone, Copy, PartialEq, Eq)]
pub enum HarvesterState {
    Idle,
    MovingToField,
    Harvesting,
    MovingToBase,
}

#[derive(Component)]
pub struct Harvester {
    pub state: HarvesterState,
    pub harvest_timer: f32,
    pub harvest_duration: f32, // Time to fill up (10 seconds)
    pub crystals_carried: i32,
    pub spawn_point: (i32, i32), // Base location
    pub target_field: Option<(i32, i32)>,
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
    pub initial_scale: f32,
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

impl ClickedUnit {
    pub fn clear(&mut self) {
        self.entity = None;
    }
}

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

#[derive(Resource)]
pub struct Economy {
    pub red_money: i32,
    pub blue_money: i32,
}

impl Default for Economy {
    fn default() -> Self {
        Self {
            red_money: 100,
            blue_money: 100,
        }
    }
}

#[derive(Resource)]
pub struct PassiveIncomeTimer {
    pub timer: f32,
}

impl Default for PassiveIncomeTimer {
    fn default() -> Self {
        Self { timer: 0.0 }
    }
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
    pub fn is_ready(&self, _unit_class: UnitClass) -> bool {
        self.timer >= self.cooldown
    }

    pub fn get_progress(&self, _unit_class: UnitClass) -> f32 {
        if self.cooldown == 0.0 {
            1.0
        } else {
            (self.timer / self.cooldown).min(1.0)
        }
    }

    pub fn start_cooldown(&mut self, _unit_class: UnitClass, combat_units: usize) {
        // Exponential cooldown: 4 units = 30s, 5 units = 60s
        // Formula: 1.875 * 2^combat_units
        // 0 units: ~2s, 1: ~4s, 2: ~8s, 3: ~15s, 4: 30s, 5: 60s
        self.cooldown = 1.875 * 2.0_f32.powf(combat_units as f32);
        self.timer = 0.0;
    }

    pub fn update(&mut self, delta: f32) {
        self.timer += delta;
    }
}

#[derive(Resource)]
pub struct SpawnCooldowns {
    pub red: ArmyCooldowns,
    pub blue: ArmyCooldowns,
}

impl Default for SpawnCooldowns {
    fn default() -> Self {
        Self {
            red: ArmyCooldowns::default(),
            blue: ArmyCooldowns::default(),
        }
    }
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

#[derive(Resource)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AIStrategy {
    Economic,      // Focus on building harvesters and income
    Aggressive,    // Attack enemy units and disrupt economy
    Expansionist,  // Capture neutral pads
    Defensive,     // Protect our pads
}

#[derive(Resource)]
pub struct AIController {
    pub spawn_timer: f32,
    pub spawn_interval: f32,
    pub command_timer: f32,
    pub command_interval: f32,
    pub strategy: AIStrategy,
    pub strategy_timer: f32, // Time to reconsider strategy
    pub strategy_commitment_timer: f32, // Time in current strategy
    pub min_commitment_time: f32, // Minimum time before switching strategies
}

impl Default for AIController {
    fn default() -> Self {
        Self {
            spawn_timer: 0.0,
            spawn_interval: 2.0, // Check every 2 seconds if should spawn
            command_timer: 0.0,
            command_interval: 1.5, // Give commands every 1.5 seconds
            strategy: AIStrategy::Economic, // Start with economy
            strategy_timer: 0.0,
            strategy_commitment_timer: 0.0,
            min_commitment_time: 20.0, // Stay in strategy for at least 20 seconds
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
fn hex_distance(a: (i32, i32), b: (i32, i32)) -> i32 {
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

pub fn is_adjacent(pos1: (i32, i32), pos2: (i32, i32)) -> bool {
    hex_distance(pos1, pos2) == 1
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
            if let Some(&occupying_entity) = occupancy.position_to_entity.get(&target_hex) {
                if occupying_entity != entity {
                    cell_occupied = true;
                }
            }

            // Also check if another unit claimed it THIS FRAME
            if let Some(&claiming_entity) = cells_claimed_this_frame.get(&target_hex) {
                if claiming_entity != entity {
                    cell_occupied = true;
                }
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
        if let Some(enemy_pos) = nearest_enemy_pos {
            if let Ok((_, _, _, _, mut transform)) = unit_query.get_mut(*entity) {
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
                    initial_scale: 0.1,
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
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
    asset_server: Res<AssetServer>,
    mut occupancy: ResMut<Occupancy>,
    mut occupancy_intent: ResMut<OccupancyIntent>,
    mut game_state: ResMut<GameState>,
    mut game_timer: ResMut<GameTimer>,
    mut economy: ResMut<Economy>,
    mut spawn_cooldowns: ResMut<SpawnCooldowns>,
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
        game_timer.time_remaining = 20.0;
        game_timer.is_active = false;
        game_timer.winning_army = None;

        // Reset economy
        economy.red_money = 100;
        economy.blue_money = 100;

        // Reset spawn cooldowns
        *spawn_cooldowns = SpawnCooldowns::default();

        // Respawn units by calling setup_units logic
        setup_units(commands, meshes, materials, animation_graphs, asset_server);

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

        if let Some(path) = find_path(current_cell, final_goal, config.map_radius, &blocking) {
            if path.len() > 1 {
                let mut new_path = vec![current_cell];
                new_path.extend_from_slice(&path[1..]);

                if new_path != old_movement.path {
                    if let Ok((_, mut unit_component, mut movement)) = unit_query.get_mut(entity) {
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
    }
}

fn update_unit_animations(
    mut commands: Commands,
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
    mut players_query: Query<(&mut AnimationPlayer, &mut AnimationTransitions)>,
) {
    for (unit_entity, anim_graphs, mut anim_state, mut graph_handle, movement) in
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

            for descendant in children_query.iter_descendants(unit_entity) {
                if let Ok((mut player, mut transitions)) = players_query.get_mut(descendant) {
                    if same_animation {
                        // Same animation for idle and moving - pause when idle, play when moving
                        if is_moving {
                            player.resume_all();
                        } else {
                            player.pause_all();
                        }
                    } else {
                        // Different animations - transition between them
                        transitions
                            .play(&mut player, new_index, Duration::from_secs_f32(0.2))
                            .repeat();
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
                let mut transitions = AnimationTransitions::new();
                transitions
                    .play(&mut player, anim_graphs.idle_index, Duration::from_secs_f32(0.0))
                    .repeat();

                commands
                    .entity(player_entity)
                    .insert((
                        graph_handle.clone(),
                        transitions,
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
                    initial_scale: 1.0,
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

        let ring_mesh = meshes.add(create_selection_ring_mesh(90.0, 100.0));
        let ring_material = materials.add(StandardMaterial {
            base_color: Color::srgb(1.0, 1.0, 1.0),
            emissive: Color::srgb(1.0, 1.0, 1.0).into(),
            unlit: true,
            ..default()
        });

        // Prepare health bar meshes
        let bar_width = 40.0;
        let bar_height = 10.0;
        let border_width_sides = 4.0;
        let border_height_extra = 8.0;
        let health_bar_mesh = meshes.add(create_health_bar_mesh(bar_width, bar_height));
        let border_mesh = meshes.add(create_health_bar_mesh(
            bar_width + border_width_sides,
            bar_height + border_height_extra,
        ));

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
            let unit_entity = if spawn_request.unit_class == UnitClass::Infantry {
                // Infantry with 3 models
                let spacing = 20.0;
                let offsets = [
                    Vec3::new(0.0, 0.0, spacing),
                    Vec3::new(-spacing, 0.0, -spacing),
                    Vec3::new(spacing, 0.0, -spacing),
                ];

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

                parent
                    .spawn((
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
                            idle_graph: graph_handle.clone(),
                            idle_index,
                            moving_graph: graph_handle.clone(),
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
                    ))
                    .with_children(|unit_parent| {
                        for offset in offsets.iter() {
                            let scene: Handle<Scene> = asset_server.load(&format!("{}#Scene0", model_path));
                            unit_parent.spawn((
                                SceneRoot(scene),
                                Transform::from_translation(*offset)
                                    .with_scale(Vec3::splat(spawn_request.unit_class.scale())),
                            ));
                        }
                    })
                    .id()
            } else {
                // Single model for Cavalry and Artillery
                let scene: Handle<Scene> = asset_server.load(&format!("{}#Scene0", model_path));

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

                let mut entity_commands = parent.spawn((
                    SceneRoot(scene),
                    Transform::from_translation(unit_pos)
                        .with_scale(Vec3::splat(spawn_request.unit_class.scale())),
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
                        idle_graph: graph_handle.clone(),
                        idle_index,
                        moving_graph: graph_handle.clone(),
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
                    entity_commands.insert(Harvester {
                        state: HarvesterState::Idle,
                        harvest_timer: 0.0,
                        harvest_duration: 10.0,
                        crystals_carried: 0,
                        spawn_point: (q, r),
                        target_field: None,
                    });
                }

                entity_commands.id()
            };

            // Spawn health bars (matching original setup)
            let bar_pos_world = world_pos + Vec3::new(0.0, 70.0, 0.0);

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
                HealthBar { unit_entity },
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
                HealthBar { unit_entity },
            ));

            // Fill (color based on army)
            parent.spawn((
                Mesh3d(health_bar_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: health_bar_color,
                    emissive: health_bar_color.into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(bar_pos_world + Vec3::new(0.0, 0.2, 0.0)),
                HealthBar { unit_entity },
                HealthBarFill,
            ));

            // Spawn selection ring
            let ring_pos = world_pos + Vec3::new(0.0, 6.0, 0.0);
            let ring_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
            parent.spawn((
                Mesh3d(ring_mesh),
                MeshMaterial3d(ring_material),
                Transform::from_translation(ring_pos)
                    .with_rotation(ring_rotation)
                    .with_scale(Vec3::splat(0.5)),
                Visibility::Hidden,
                crate::selection::SelectionRing {
                    unit_entity,
                    animation_timer: 0.0,
                    bounce_count: 0,
                },
            ));

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

        // Start cooldown based on this army's combat unit count (excluding harvesters)
        let combat_units = unit_query.iter()
            .filter(|(u, uc)| u.army == spawn_request.army && **uc != UnitClass::Harvester)
            .count();
        let army_cooldowns = spawn_cooldowns.get_army_cooldowns_mut(spawn_request.army);
        army_cooldowns.start_cooldown(spawn_request.unit_class, combat_units);

        println!("Spawned {:?} {:?} unit at ({}, {}) for ${} (global cooldown: {:.1}s)",
            spawn_request.army, spawn_request.unit_class, q, r, cost, army_cooldowns.cooldown
        );
    }
}

fn ai_spawn_units(
    time: Res<Time>,
    mut ai_controller: ResMut<AIController>,
    economy: Res<Economy>,
    spawn_cooldowns: Res<SpawnCooldowns>,
    mut spawn_queue: ResMut<UnitSpawnQueue>,
    unit_query: Query<(&Unit, &UnitClass)>,
    pad_ownership: Res<LaunchPadOwnership>,
    launch_pads: Res<LaunchPads>,
    game_timer: Res<GameTimer>,
) {
    ai_controller.spawn_timer += time.delta_secs();
    ai_controller.strategy_timer += time.delta_secs();
    ai_controller.strategy_commitment_timer += time.delta_secs();

    if ai_controller.spawn_timer < ai_controller.spawn_interval {
        return;
    }

    ai_controller.spawn_timer = 0.0;

    // Reconsider strategy every 15 seconds
    if ai_controller.strategy_timer >= 15.0 {
        ai_controller.strategy_timer = 0.0;

        // Evaluate what the best strategy would be
        let new_strategy = evaluate_strategy(
            &unit_query,
            &economy,
            &pad_ownership,
            &launch_pads,
            &game_timer,
        );

        // Only switch strategies if:
        // 1. We've been in current strategy for at least min_commitment_time
        // 2. The new strategy is different from current
        if new_strategy != ai_controller.strategy {
            if ai_controller.strategy_commitment_timer >= ai_controller.min_commitment_time {
                println!("🔄 AI Strategy change: {:?} -> {:?} (after {:.1}s)",
                    ai_controller.strategy, new_strategy, ai_controller.strategy_commitment_timer);
                ai_controller.strategy = new_strategy;
                ai_controller.strategy_commitment_timer = 0.0;
            } else {
                println!("⏳ AI wants to switch to {:?} but committed to {:?} for {:.1}s more",
                    new_strategy, ai_controller.strategy,
                    ai_controller.min_commitment_time - ai_controller.strategy_commitment_timer);
            }
        }
    }

    // Count blue units by type and army
    let mut blue_infantry = 0;
    let mut blue_cavalry = 0;
    let mut blue_artillery = 0;
    let mut blue_harvesters = 0;
    let mut red_combat_units = 0;

    for (unit, unit_class) in unit_query.iter() {
        if unit.army == Army::Blue {
            match unit_class {
                UnitClass::Infantry => blue_infantry += 1,
                UnitClass::Cavalry => blue_cavalry += 1,
                UnitClass::Artillery => blue_artillery += 1,
                UnitClass::Harvester => blue_harvesters += 1,
            }
        } else if unit.army == Army::Red {
            if *unit_class != UnitClass::Harvester {
                red_combat_units += 1;
            }
        }
    }

    let blue_combat_units = blue_infantry + blue_cavalry + blue_artillery;
    let blue_money = economy.blue_money;

    // Decide what to spawn based on strategy
    let unit_to_spawn = match ai_controller.strategy {
        AIStrategy::Economic => {
            // Build only 1 harvester initially for economy
            if blue_harvesters < 1 && blue_money >= UnitClass::Harvester.cost() {
                Some(UnitClass::Harvester)
            }
            // Then build cheap infantry for defense
            else if blue_money >= UnitClass::Infantry.cost() {
                Some(UnitClass::Infantry)
            } else {
                None
            }
        }
        AIStrategy::Aggressive => {
            // Build a mix of combat units, prioritizing artillery if we can afford it
            if blue_money >= UnitClass::Artillery.cost() && blue_artillery < blue_combat_units / 3 {
                Some(UnitClass::Artillery)
            } else if blue_money >= UnitClass::Cavalry.cost() && blue_cavalry < blue_combat_units / 3 {
                Some(UnitClass::Cavalry)
            } else if blue_money >= UnitClass::Infantry.cost() {
                Some(UnitClass::Infantry)
            } else {
                None
            }
        }
        AIStrategy::Expansionist => {
            // Build fast cavalry for capturing pads
            if blue_money >= UnitClass::Cavalry.cost() && blue_cavalry < blue_combat_units / 2 {
                Some(UnitClass::Cavalry)
            } else if blue_money >= UnitClass::Infantry.cost() {
                Some(UnitClass::Infantry)
            } else {
                None
            }
        }
        AIStrategy::Defensive => {
            // Build infantry quickly for defense, then artillery
            if blue_money >= UnitClass::Infantry.cost() && blue_combat_units < 3 {
                Some(UnitClass::Infantry) // Get 3 infantry out quickly
            } else if blue_money >= UnitClass::Artillery.cost() && blue_artillery < 1 {
                Some(UnitClass::Artillery) // Then add artillery
            } else if blue_money >= UnitClass::Infantry.cost() {
                Some(UnitClass::Infantry) // Back to infantry
            } else {
                None
            }
        }
    };

    // Spawn the chosen unit if cooldown is ready
    if let Some(unit_class) = unit_to_spawn {
        let blue_cooldowns = spawn_cooldowns.get_army_cooldowns(Army::Blue);
        if blue_cooldowns.is_ready(unit_class) {
            spawn_queue.requests.push(UnitSpawnRequest {
                unit_class,
                army: Army::Blue,
            });
            println!("AI ({:?} strategy): Spawning {:?}", ai_controller.strategy, unit_class);
        }
    }
}

// Evaluate which strategy the AI should use based on game state
fn evaluate_strategy(
    unit_query: &Query<(&Unit, &UnitClass)>,
    economy: &Economy,
    pad_ownership: &LaunchPadOwnership,
    launch_pads: &LaunchPads,
    game_timer: &GameTimer,
) -> AIStrategy {
    let mut blue_units = 0;
    let mut red_units = 0;
    let mut blue_harvesters = 0;
    let mut blue_combat_units = 0;
    let mut red_combat_units = 0;

    for (unit, unit_class) in unit_query.iter() {
        match unit.army {
            Army::Blue => {
                blue_units += 1;
                if *unit_class == UnitClass::Harvester {
                    blue_harvesters += 1;
                } else {
                    blue_combat_units += 1;
                }
            }
            Army::Red => {
                red_units += 1;
                if *unit_class != UnitClass::Harvester {
                    red_combat_units += 1;
                }
            }
        }
    }

    // Count controlled pads
    let mut blue_pads = 0;
    let mut neutral_pads = 0;
    let mut red_pads = 0;
    let mut contested_pads = 0;

    for owner in &pad_ownership.owners {
        match owner {
            LaunchPadOwner::Blue => blue_pads += 1,
            LaunchPadOwner::Neutral => neutral_pads += 1,
            LaunchPadOwner::Red => red_pads += 1,
            LaunchPadOwner::Contested => contested_pads += 1,
        }
    }

    // CRITICAL 1: Early game rush detection
    // If player has combat units and we don't, respond immediately!
    if red_combat_units > 0 && blue_combat_units == 0 {
        println!("⚠️ EARLY RUSH DETECTED: Player has {} combat units, we have 0 - BUILDING ARMY!", red_combat_units);
        return AIStrategy::Defensive; // Build units to defend, not harvesters
    }

    // If player is way ahead in combat units early, stop building economy
    if red_combat_units >= 2 && blue_combat_units < red_combat_units {
        println!("⚠️ PLAYER ARMY ADVANTAGE: Red {} vs Blue {} - BUILDING COMBAT UNITS!", red_combat_units, blue_combat_units);
        return AIStrategy::Aggressive; // Focus on combat
    }

    // CRITICAL 2: Check win condition urgency
    // If timer is active and counting down, this overrides all other strategies
    if game_timer.is_active {
        if let Some(winning_army) = game_timer.winning_army {
            match winning_army {
                Army::Red => {
                    // Player is winning! We MUST attack and contest their pads immediately
                    println!("🚨 EMERGENCY: Player winning with {:.1}s left - ATTACKING PADS!", game_timer.time_remaining);
                    return AIStrategy::Aggressive; // Attack enemy pads aggressively
                }
                Army::Blue => {
                    // We're winning! Different behavior based on time remaining
                    if game_timer.time_remaining < 10.0 {
                        println!("🎯 AI WINNING: {:.1}s left - HOLD POSITIONS!", game_timer.time_remaining);
                        return AIStrategy::Defensive; // Just hold what we have
                    }
                }
            }
        }
    }

    // Score all strategies (timer urgency is handled above, this is for normal play)
    let mut scores = vec![
        (AIStrategy::Economic, score_economic_strategy(blue_harvesters, economy.blue_money)),
        (AIStrategy::Aggressive, score_aggressive_strategy(blue_units, red_units, economy.blue_money)),
        (AIStrategy::Expansionist, score_expansionist_strategy(blue_pads, red_pads, neutral_pads)),
        (AIStrategy::Defensive, score_defensive_strategy(blue_units, red_units, blue_pads, red_pads, contested_pads)),
    ];

    // Sort by score descending
    scores.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

    scores[0].0
}

// Helper functions to score each strategy
fn score_economic_strategy(blue_harvesters: usize, blue_money: i32) -> f32 {
    let mut score = 0.0;

    // Strong need for economy if no harvesters
    if blue_harvesters == 0 {
        score += 50.0;
    }

    // Moderate need for second harvester if low money
    if blue_harvesters == 1 && blue_money < 80 {
        score += 20.0; // Reduced from 30
    }

    // Economy is less valuable if we already have good income
    if blue_harvesters >= 2 {
        score -= 40.0; // Increased penalty
    }

    // After first harvester, economy is less urgent
    if blue_harvesters >= 1 {
        score -= 15.0;
    }

    score
}

fn score_aggressive_strategy(blue_units: usize, red_units: usize, blue_money: i32) -> f32 {
    let mut score = 0.0;

    // Good if we have military advantage
    if blue_units > red_units {
        score += 40.0;
    }

    // Good if we have money to sustain aggression
    if blue_money > 100 {
        score += 30.0;
    }

    // Bad if we're outnumbered
    if red_units > blue_units {
        score -= 20.0;
    }

    score
}

fn score_expansionist_strategy(blue_pads: usize, red_pads: usize, neutral_pads: usize) -> f32 {
    let mut score = 0.0;

    // Strong priority if neutral pads exist
    if neutral_pads > 0 {
        score += 50.0;
    }

    // Higher priority if enemy has more pads
    if red_pads > blue_pads {
        score += 30.0;
    }

    // No point in expansion if no pads to capture
    if neutral_pads == 0 && red_pads <= blue_pads {
        score -= 40.0;
    }

    score
}

fn score_defensive_strategy(blue_units: usize, red_units: usize, blue_pads: usize, red_pads: usize, contested_pads: usize) -> f32 {
    let mut score = 0.0;

    // High priority if outnumbered
    if red_units > blue_units * 2 {
        score += 60.0;
    } else if red_units > blue_units {
        score += 30.0;
    }

    // Very high priority if pads are contested
    if contested_pads > 0 {
        score += 50.0;
    }

    // Defensive if we're winning on pads (protect what we have)
    if blue_pads > red_pads {
        score += 20.0;
    }

    // Lower priority if we have advantage
    if blue_units > red_units {
        score -= 20.0;
    }

    score
}

fn ai_command_units(
    time: Res<Time>,
    mut ai_controller: ResMut<AIController>,
    mut commands: Commands,
    launch_pads: Res<LaunchPads>,
    pad_ownership: Res<LaunchPadOwnership>,
    obstacles: Res<Obstacles>,
    map_config: Res<HexMapConfig>,
    occupancy: Res<Occupancy>,
    occupancy_intent: Res<OccupancyIntent>,
    mut claimed_cells: ResMut<ClaimedCellsThisFrame>,
    mut unit_query: Query<(Entity, &Unit, &UnitStats, &UnitClass, Option<&UnitMovement>)>,
) {
    ai_controller.command_timer += time.delta_secs();

    if ai_controller.command_timer < ai_controller.command_interval {
        return;
    }

    ai_controller.command_timer = 0.0;

    // Find all idle blue units (units without movement or who have reached destination)
    let mut idle_blue_units = Vec::new();
    let mut all_red_units = Vec::new();

    for (entity, unit, stats, unit_class, movement) in unit_query.iter() {
        if unit.army == Army::Blue {
            // Unit is idle if it has no movement component or has reached end of path
            let is_idle = movement.is_none()
                || movement.map_or(false, |m| m.current_waypoint >= m.path.len());
            if is_idle {
                idle_blue_units.push((entity, unit, stats, unit_class));
            }
        } else if unit.army == Army::Red {
            all_red_units.push((unit.q, unit.r, unit_class));
        }
    }

    if idle_blue_units.is_empty() {
        return;
    }

    // Find targets based on current strategy
    let strategy = ai_controller.strategy;

    println!("🤖 AI commanding {} idle units (Strategy: {:?})", idle_blue_units.len(), strategy);

    for (entity, unit, stats, unit_class) in idle_blue_units {
        let unit_pos = (unit.q, unit.r);

        // Build blocking cells set
        let mut blocking_cells = obstacles.positions.clone();
        for &occupied_pos in &occupancy.positions {
            if occupied_pos != unit_pos {
                blocking_cells.insert(occupied_pos);
            }
        }
        for (other_entity, &intent_pos) in &occupancy_intent.intentions {
            if *other_entity != entity && intent_pos != unit_pos {
                blocking_cells.insert(intent_pos);
            }
        }
        for &claimed_cell in &claimed_cells.cells {
            if claimed_cell != unit_pos {
                blocking_cells.insert(claimed_cell);
            }
        }

        // Choose target based on strategy
        let target = match strategy {
            AIStrategy::Economic => {
                // Harvesters do their own thing, combat units defend blue pads
                if *unit_class == UnitClass::Harvester {
                    None // Harvesters have their own AI
                } else {
                    // Defend our pads, attack enemy pads, or capture neutral pads (game start)
                    find_defensive_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells)
                        .or_else(|| find_enemy_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
                        .or_else(|| find_neutral_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
                }
            }
            AIStrategy::Aggressive => {
                // Priority: Enemy harvesters > Enemy units on pads > Enemy pads
                find_enemy_harvester_target(unit_pos, &all_red_units, &blocking_cells)
                    .or_else(|| find_enemy_unit_target(unit_pos, &all_red_units, &launch_pads, &pad_ownership, &blocking_cells))
                    .or_else(|| find_enemy_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
                    .or_else(|| find_neutral_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
            }
            AIStrategy::Expansionist => {
                // Priority: Neutral pads > Enemy pads
                find_neutral_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells)
                    .or_else(|| find_enemy_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
            }
            AIStrategy::Defensive => {
                // Protect our pads, stay near them
                // If no blue/contested pads to defend, attack enemy pads to contest them
                find_defensive_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells)
                    .or_else(|| find_enemy_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
                    .or_else(|| find_neutral_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
            }
        };

        // Command unit to move to target
        if let Some(target_pos) = target {
            if let Some(path) = find_path(unit_pos, target_pos, map_config.map_radius, &blocking_cells) {
                if path.len() > 1 {
                    let path_to_follow: Vec<(i32, i32)> = path[1..].to_vec();

                    commands.entity(entity).insert(UnitMovement {
                        path: path_to_follow.clone(),
                        current_waypoint: 0,
                        progress: 0.0,
                        speed: stats.speed,
                        segment_start: unit_pos,
                    });

                    for &cell in &path_to_follow {
                        claimed_cells.cells.insert(cell);
                    }
                }
            }
        } else {
            println!("⚠️ AI unit at ({}, {}) has NO TARGET (Strategy: {:?}, Class: {:?})",
                unit_pos.0, unit_pos.1, strategy, unit_class);
        }
    }
}

// Helper functions for target selection
fn find_enemy_harvester_target(
    unit_pos: (i32, i32),
    red_units: &[(i32, i32, &UnitClass)],
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    let harvesters: Vec<(i32, i32)> = red_units
        .iter()
        .filter(|(_, _, class)| **class == UnitClass::Harvester)
        .map(|(q, r, _)| (*q, *r))
        .filter(|pos| !blocking_cells.contains(pos))
        .collect();

    find_nearest(unit_pos, &harvesters)
}

fn find_enemy_unit_target(
    unit_pos: (i32, i32),
    red_units: &[(i32, i32, &UnitClass)],
    launch_pads: &LaunchPads,
    pad_ownership: &LaunchPadOwnership,
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    // Find enemy units that are on pads (threatening capture)
    let mut enemy_positions_on_pads = Vec::new();

    for (pad_index, pad_tiles) in launch_pads.pads.iter().enumerate() {
        let owner = pad_ownership.owners.get(pad_index).copied().unwrap_or(LaunchPadOwner::Neutral);

        // Look for enemy units on neutral or our pads
        if owner != LaunchPadOwner::Red {
            for &(eq, er, _) in red_units {
                if pad_tiles.contains(&(eq, er)) && !blocking_cells.contains(&(eq, er)) {
                    enemy_positions_on_pads.push((eq, er));
                }
            }
        }
    }

    find_nearest(unit_pos, &enemy_positions_on_pads)
}

fn find_enemy_pad_target(
    unit_pos: (i32, i32),
    launch_pads: &LaunchPads,
    pad_ownership: &LaunchPadOwnership,
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    let mut targets = Vec::new();

    for (pad_index, pad_tiles) in launch_pads.pads.iter().enumerate() {
        let owner = pad_ownership.owners.get(pad_index).copied().unwrap_or(LaunchPadOwner::Neutral);

        if owner == LaunchPadOwner::Red || owner == LaunchPadOwner::Contested {
            for &(q, r) in pad_tiles {
                if !blocking_cells.contains(&(q, r)) {
                    targets.push((q, r));
                }
            }
        }
    }

    find_nearest(unit_pos, &targets)
}

fn find_neutral_pad_target(
    unit_pos: (i32, i32),
    launch_pads: &LaunchPads,
    pad_ownership: &LaunchPadOwnership,
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    let mut targets = Vec::new();

    for (pad_index, pad_tiles) in launch_pads.pads.iter().enumerate() {
        let owner = pad_ownership.owners.get(pad_index).copied().unwrap_or(LaunchPadOwner::Neutral);

        if owner == LaunchPadOwner::Neutral {
            for &(q, r) in pad_tiles {
                if !blocking_cells.contains(&(q, r)) {
                    targets.push((q, r));
                }
            }
        }
    }

    find_nearest(unit_pos, &targets)
}

fn find_defensive_target(
    unit_pos: (i32, i32),
    launch_pads: &LaunchPads,
    pad_ownership: &LaunchPadOwnership,
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    let mut targets = Vec::new();

    for (pad_index, pad_tiles) in launch_pads.pads.iter().enumerate() {
        let owner = pad_ownership.owners.get(pad_index).copied().unwrap_or(LaunchPadOwner::Neutral);

        // Stay near blue or contested pads
        if owner == LaunchPadOwner::Blue || owner == LaunchPadOwner::Contested {
            for &(q, r) in pad_tiles {
                if !blocking_cells.contains(&(q, r)) {
                    targets.push((q, r));
                }
            }
        }
    }

    find_nearest(unit_pos, &targets)
}

fn find_nearest(from: (i32, i32), targets: &[(i32, i32)]) -> Option<(i32, i32)> {
    if targets.is_empty() {
        return None;
    }

    let mut nearest = targets[0];
    let mut min_dist = hex_distance(from, nearest);

    for &target in &targets[1..] {
        let dist = hex_distance(from, target);
        if dist < min_dist {
            min_dist = dist;
            nearest = target;
        }
    }

    Some(nearest)
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

fn setup_units(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
    asset_server: Res<AssetServer>,
) {
    // (q, r, unit_index, army, class)
    let units: Vec<(i32, i32, usize, Army, UnitClass)> = vec![
        // Start with no units - players must purchase them
    ];

    let ring_mesh = meshes.add(create_selection_ring_mesh(53.0, 63.0));
    let ring_material = materials.add(StandardMaterial {
        base_color: Color::srgb(1.0, 1.0, 1.0), // White
        emissive: Color::srgb(1.0, 1.0, 1.0).into(), // White
        unlit: true,
        ..default()
    });

    // Create Red Army parent
    commands.spawn((
        RedArmy,
        Transform::default(),
        Visibility::default(),
        Name::new("Red Army"),
    )).with_children(|parent| {
        for (q, r, unit_index, army, unit_class) in units.iter().filter(|(_, _, _, a, _)| *a == Army::Red) {
            let world_pos = axial_to_world_pos(*q, *r);
            let unit_pos = world_pos + Vec3::new(0.0, 5.0, 0.0);

            // Load model based on class
            let model_path = unit_class.model_path();

            // Get stats for this class
            let stats = unit_class.default_stats();

            // Health bar color based on army
            let health_bar_color = match army {
                Army::Red => Color::srgb(0.9, 0.2, 0.2),
                Army::Blue => Color::srgb(0.2, 0.4, 0.9),
            };

            // Prepare health bar meshes
            let bar_width = 40.0;
            let bar_height = 10.0;
            let border_width_sides = 4.0;
            let border_height_extra = 8.0;
            let health_bar_mesh = meshes.add(create_health_bar_mesh(bar_width, bar_height));
            let border_mesh = meshes.add(create_health_bar_mesh(
                bar_width + border_width_sides,
                bar_height + border_height_extra,
            ));

            // Create unit entity based on class
            let unit_entity = if *unit_class == UnitClass::Infantry {
                // Infantry: spawn 3 model instances as children in triangle formation
                let spacing = 20.0;
                let offsets = [
                    Vec3::new(0.0, 0.0, spacing),      // Front
                    Vec3::new(-spacing, 0.0, -spacing), // Back Left
                    Vec3::new(spacing, 0.0, -spacing),  // Back Right
                ];

                // Create animation graph for parent (needed for animation system)
                let mut animation_graph = AnimationGraph::new();
                let idle_index = animation_graph.add_clip(
                    asset_server.load(GltfAssetLabel::Animation(unit_class.idle_animation_index()).from_asset(model_path)),
                    1.0,
                    animation_graph.root,
                );
                let moving_index = animation_graph.add_clip(
                    asset_server.load(GltfAssetLabel::Animation(unit_class.moving_animation_index()).from_asset(model_path)),
                    1.0,
                    animation_graph.root,
                );
                let graph_handle = animation_graphs.add(animation_graph);

                parent
                    .spawn((
                        Transform::from_translation(unit_pos),
                        Visibility::Visible,
                        Unit {
                            q: *q,
                            r: *r,
                            _sprite_index: *unit_index,
                            army: *army,
                        },
                        *army,
                        *unit_class,
                        stats.clone(),
                        AnimationGraphHandle(graph_handle.clone()),
                        AnimationGraphs {
                            idle_graph: graph_handle.clone(),
                            idle_index,
                            moving_graph: graph_handle.clone(),
                            moving_index,
                        },
                        CurrentAnimationState { is_moving: false },
                        Health {
                            current: stats.max_health,
                            max: stats.max_health,
                        },
                        Combat {
                            last_attack_time: -10.0, // Start ready to attack
                            attack_cooldown: unit_class.base_cooldown(),
                            last_movement_time: -10.0, // Start ready to fire
                            movement_cooldown: 0.5, // 0.5 second cooldown after moving
                        },
                        Name::new(format!("{:?} {} ({}, {})", unit_class, unit_index, q, r)),
                    ))
                    .with_children(|unit_parent| {
                        // Spawn model instances
                        for (i, offset) in offsets.iter().enumerate() {
                            let scene: Handle<Scene> = asset_server.load(&format!("{}#Scene0", model_path));

                            unit_parent.spawn((
                                SceneRoot(scene),
                                Transform::from_translation(*offset)
                                    .with_scale(Vec3::splat(unit_class.scale())),
                                Name::new(format!("Infantry Model {}", i)),
                            ));
                        }
                    })
                    .id()
            } else {
                // Other classes: single model instance
                let scene: Handle<Scene> = asset_server.load(&format!("{}#Scene0", model_path));
                let mut animation_graph = AnimationGraph::new();
                let idle_index = animation_graph.add_clip(
                    asset_server.load(GltfAssetLabel::Animation(unit_class.idle_animation_index()).from_asset(model_path)),
                    1.0,
                    animation_graph.root,
                );
                let moving_index = animation_graph.add_clip(
                    asset_server.load(GltfAssetLabel::Animation(unit_class.moving_animation_index()).from_asset(model_path)),
                    1.0,
                    animation_graph.root,
                );
                let graph_handle = animation_graphs.add(animation_graph);

                parent
                    .spawn((
                        SceneRoot(scene),
                        Transform::from_translation(unit_pos)
                            .with_scale(Vec3::splat(unit_class.scale())),
                        Unit {
                            q: *q,
                            r: *r,
                            _sprite_index: *unit_index,
                            army: *army,
                        },
                        *army,
                        *unit_class,
                        stats.clone(),
                        AnimationGraphHandle(graph_handle.clone()),
                        AnimationGraphs {
                            idle_graph: graph_handle.clone(),
                            idle_index,
                            moving_graph: graph_handle.clone(),
                            moving_index,
                        },
                        CurrentAnimationState { is_moving: false },
                        Health {
                            current: stats.max_health,
                            max: stats.max_health,
                        },
                        Combat {
                            last_attack_time: -10.0, // Start ready to attack
                            attack_cooldown: unit_class.base_cooldown(),
                            last_movement_time: -10.0, // Start ready to fire
                            movement_cooldown: 0.5, // 0.5 second cooldown after moving
                        },
                        Name::new(format!("{:?} {} ({}, {})", unit_class, unit_index, q, r)),
                    ))
                    .id()
            };

            // Spawn health bars (as siblings of unit, children of Army)
            let bar_pos_world = world_pos + Vec3::new(0.0, 70.0, 0.0);

            // Border (black)
            parent.spawn((
                Mesh3d(border_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.0, 0.0, 0.0),
                    emissive: Color::srgb(0.0, 0.0, 0.0).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(bar_pos_world),
                HealthBar { unit_entity },
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
                HealthBar { unit_entity },
            ));

            // Fill (color based on army)
            parent.spawn((
                Mesh3d(health_bar_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: health_bar_color,
                    emissive: health_bar_color.into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(bar_pos_world + Vec3::new(0.0, 0.2, 0.0)),
                HealthBar { unit_entity },
                HealthBarFill,
            ));

            let ring_pos = world_pos + Vec3::new(0.0, 6.0, 0.0);
            let ring_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
            parent.spawn((
                Mesh3d(ring_mesh.clone()),
                MeshMaterial3d(ring_material.clone()),
                Transform::from_translation(ring_pos)
                    .with_rotation(ring_rotation)
                    .with_scale(Vec3::splat(0.5)),
                SelectionRing {
                    unit_entity,
                    animation_timer: 0.0,
                    bounce_count: 0,
                },
                Visibility::Hidden,
            ));

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
        }
    });

    // Create Blue Army parent
    commands.spawn((
        BlueArmy,
        Transform::default(),
        Visibility::default(),
        Name::new("Blue Army"),
    )).with_children(|parent| {
        for (q, r, unit_index, army, unit_class) in units.iter().filter(|(_, _, _, a, _)| *a == Army::Blue) {
            let world_pos = axial_to_world_pos(*q, *r);
            let unit_pos = world_pos + Vec3::new(0.0, 5.0, 0.0);

            // Load model based on class
            let model_path = unit_class.model_path();

            // Get stats for this class
            let stats = unit_class.default_stats();

            // Health bar color based on army
            let health_bar_color = match army {
                Army::Red => Color::srgb(0.9, 0.2, 0.2),
                Army::Blue => Color::srgb(0.2, 0.4, 0.9),
            };

            // Prepare health bar meshes
            let bar_width = 40.0;
            let bar_height = 10.0;
            let border_width_sides = 4.0;
            let border_height_extra = 8.0;
            let health_bar_mesh = meshes.add(create_health_bar_mesh(bar_width, bar_height));
            let border_mesh = meshes.add(create_health_bar_mesh(
                bar_width + border_width_sides,
                bar_height + border_height_extra,
            ));

            // Create unit entity based on class
            let unit_entity = if *unit_class == UnitClass::Infantry {
                // Infantry: spawn 3 model instances as children in triangle formation
                let spacing = 20.0;
                let offsets = [
                    Vec3::new(0.0, 0.0, spacing),      // Front
                    Vec3::new(-spacing, 0.0, -spacing), // Back Left
                    Vec3::new(spacing, 0.0, -spacing),  // Back Right
                ];

                // Create animation graph for parent (needed for animation system)
                let mut animation_graph = AnimationGraph::new();
                let idle_index = animation_graph.add_clip(
                    asset_server.load(GltfAssetLabel::Animation(unit_class.idle_animation_index()).from_asset(model_path)),
                    1.0,
                    animation_graph.root,
                );
                let moving_index = animation_graph.add_clip(
                    asset_server.load(GltfAssetLabel::Animation(unit_class.moving_animation_index()).from_asset(model_path)),
                    1.0,
                    animation_graph.root,
                );
                let graph_handle = animation_graphs.add(animation_graph);

                parent
                    .spawn((
                        Transform::from_translation(unit_pos),
                        Visibility::Visible,
                        Unit {
                            q: *q,
                            r: *r,
                            _sprite_index: *unit_index,
                            army: *army,
                        },
                        *army,
                        *unit_class,
                        stats.clone(),
                        AnimationGraphHandle(graph_handle.clone()),
                        AnimationGraphs {
                            idle_graph: graph_handle.clone(),
                            idle_index,
                            moving_graph: graph_handle.clone(),
                            moving_index,
                        },
                        CurrentAnimationState { is_moving: false },
                        Health {
                            current: stats.max_health,
                            max: stats.max_health,
                        },
                        Combat {
                            last_attack_time: -10.0, // Start ready to attack
                            attack_cooldown: unit_class.base_cooldown(),
                            last_movement_time: -10.0, // Start ready to fire
                            movement_cooldown: 0.5, // 0.5 second cooldown after moving
                        },
                        Name::new(format!("{:?} {} ({}, {})", unit_class, unit_index, q, r)),
                    ))
                    .with_children(|unit_parent| {
                        // Spawn model instances
                        for (i, offset) in offsets.iter().enumerate() {
                            let scene: Handle<Scene> = asset_server.load(&format!("{}#Scene0", model_path));

                            unit_parent.spawn((
                                SceneRoot(scene),
                                Transform::from_translation(*offset)
                                    .with_scale(Vec3::splat(unit_class.scale())),
                                Name::new(format!("Infantry Model {}", i)),
                            ));
                        }
                    })
                    .id()
            } else {
                // Other classes: single model instance
                let scene: Handle<Scene> = asset_server.load(&format!("{}#Scene0", model_path));
                let mut animation_graph = AnimationGraph::new();
                let idle_index = animation_graph.add_clip(
                    asset_server.load(GltfAssetLabel::Animation(unit_class.idle_animation_index()).from_asset(model_path)),
                    1.0,
                    animation_graph.root,
                );
                let moving_index = animation_graph.add_clip(
                    asset_server.load(GltfAssetLabel::Animation(unit_class.moving_animation_index()).from_asset(model_path)),
                    1.0,
                    animation_graph.root,
                );
                let graph_handle = animation_graphs.add(animation_graph);

                parent
                    .spawn((
                        SceneRoot(scene),
                        Transform::from_translation(unit_pos)
                            .with_scale(Vec3::splat(unit_class.scale())),
                        Unit {
                            q: *q,
                            r: *r,
                            _sprite_index: *unit_index,
                            army: *army,
                        },
                        *army,
                        *unit_class,
                        stats.clone(),
                        AnimationGraphHandle(graph_handle.clone()),
                        AnimationGraphs {
                            idle_graph: graph_handle.clone(),
                            idle_index,
                            moving_graph: graph_handle.clone(),
                            moving_index,
                        },
                        CurrentAnimationState { is_moving: false },
                        Health {
                            current: stats.max_health,
                            max: stats.max_health,
                        },
                        Combat {
                            last_attack_time: -10.0, // Start ready to attack
                            attack_cooldown: unit_class.base_cooldown(),
                            last_movement_time: -10.0, // Start ready to fire
                            movement_cooldown: 0.5, // 0.5 second cooldown after moving
                        },
                        Name::new(format!("{:?} {} ({}, {})", unit_class, unit_index, q, r)),
                    ))
                    .id()
            };

            // Spawn health bars (as siblings of unit, children of Army)
            let bar_pos_world = world_pos + Vec3::new(0.0, 70.0, 0.0);

            // Border (black)
            parent.spawn((
                Mesh3d(border_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.0, 0.0, 0.0),
                    emissive: Color::srgb(0.0, 0.0, 0.0).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(bar_pos_world),
                HealthBar { unit_entity },
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
                HealthBar { unit_entity },
            ));

            // Fill (blue for blue army)
            parent.spawn((
                Mesh3d(health_bar_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.2, 0.4, 0.9),
                    emissive: Color::srgb(0.2, 0.4, 0.9).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(bar_pos_world + Vec3::new(0.0, 0.2, 0.0)),
                HealthBar { unit_entity },
                HealthBarFill,
            ));

            let ring_pos = world_pos + Vec3::new(0.0, 6.0, 0.0);
            let ring_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
            parent.spawn((
                Mesh3d(ring_mesh.clone()),
                MeshMaterial3d(ring_material.clone()),
                Transform::from_translation(ring_pos)
                    .with_rotation(ring_rotation)
                    .with_scale(Vec3::splat(0.5)),
                SelectionRing {
                    unit_entity,
                    animation_timer: 0.0,
                    bounce_count: 0,
                },
                Visibility::Hidden,
            ));

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
        }
    });
}

// Autonomous harvester AI - finds closest crystal field and moves to it
fn harvester_ai_find_target(
    mut harvester_query: Query<(Entity, &Unit, &mut Harvester), Without<UnitMovement>>,
    crystal_query: Query<&CrystalField>,
) {
    for (_entity, unit, mut harvester) in &mut harvester_query {
        // Only process idle harvesters
        if harvester.state != HarvesterState::Idle {
            continue;
        }

        // Find closest crystal field with crystals remaining
        let mut closest_field: Option<(i32, i32, f32)> = None;

        for crystal_field in &crystal_query {
            if crystal_field.crystals_remaining <= 0 {
                continue;
            }

            let dx = (crystal_field.q - unit.q) as f32;
            let dy = (crystal_field.r - unit.r) as f32;
            let distance = (dx * dx + dy * dy).sqrt();

            if let Some((_, _, min_dist)) = closest_field {
                if distance < min_dist {
                    closest_field = Some((crystal_field.q, crystal_field.r, distance));
                }
            } else {
                closest_field = Some((crystal_field.q, crystal_field.r, distance));
            }
        }

        if let Some((target_q, target_r, _)) = closest_field {
            harvester.target_field = Some((target_q, target_r));
            harvester.state = HarvesterState::MovingToField;
            println!("Harvester at ({}, {}) targeting crystal field at ({}, {})",
                unit.q, unit.r, target_q, target_r);
        }
    }
}

// Command harvesters to move to their target fields
fn harvester_move_to_field(
    mut commands: Commands,
    mut harvester_query: Query<(Entity, &Unit, &UnitStats, &mut Harvester), Without<UnitMovement>>,
    occupancy: Res<Occupancy>,
    obstacles: Res<Obstacles>,
    config: Res<HexMapConfig>,
) {
    for (entity, unit, stats, mut harvester) in &mut harvester_query {
        if harvester.state != HarvesterState::MovingToField {
            continue;
        }

        if let Some((target_q, target_r)) = harvester.target_field {
            // Check if we've arrived
            if unit.q == target_q && unit.r == target_r {
                harvester.state = HarvesterState::Harvesting;
                harvester.harvest_timer = 0.0;
                println!("Harvester arrived at crystal field ({}, {}), starting to harvest", target_q, target_r);
                continue;
            }

            // Build blocking cells set (obstacles + occupied cells, excluding current position and goal)
            let mut blocking_cells = obstacles.positions.clone();
            for &occupied_pos in &occupancy.positions {
                if occupied_pos != (unit.q, unit.r) && occupied_pos != (target_q, target_r) {
                    blocking_cells.insert(occupied_pos);
                }
            }

            // Use proper A* pathfinding
            if let Some(path) = find_path((unit.q, unit.r), (target_q, target_r), config.map_radius, &blocking_cells) {
                let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                    path[1..].to_vec()
                } else {
                    vec![]
                };

                if !path_to_follow.is_empty() {
                    commands.entity(entity).insert(UnitMovement {
                        path: path_to_follow,
                        current_waypoint: 0,
                        progress: 0.0,
                        speed: stats.speed,
                        segment_start: (unit.q, unit.r),
                    });
                }
            }
        }
    }
}

// Passive income system - players earn money even without harvesters
// at 25% of a harvester's rate (1.25 crystals/sec vs 5 crystals/sec)
fn passive_income_system(
    time: Res<Time>,
    mut timer: ResMut<PassiveIncomeTimer>,
    mut economy: ResMut<Economy>,
) {
    timer.timer += time.delta_secs();

    // Award 1.25 crystals per second = 1 crystal every 0.8 seconds
    const INCOME_INTERVAL: f32 = 0.8;

    while timer.timer >= INCOME_INTERVAL {
        timer.timer -= INCOME_INTERVAL;
        economy.red_money += 1;
        economy.blue_money += 1;
    }
}

// Harvest crystals over time
fn harvester_collect_crystals(
    time: Res<Time>,
    mut economy: ResMut<Economy>,
    mut harvester_query: Query<(&Unit, &Army, &mut Harvester)>,
    mut crystal_query: Query<&mut CrystalField>,
) {
    for (unit, army, mut harvester) in &mut harvester_query {
        if harvester.state != HarvesterState::Harvesting {
            continue;
        }

        harvester.harvest_timer += time.delta_secs();

        // Collect crystals over time (5 crystals per second = 50 total in 10 seconds)
        let crystals_per_second = 5.0;
        let delta_crystals = (crystals_per_second * time.delta_secs()).ceil() as i32;

        // Find the crystal field at this position
        for mut crystal_field in &mut crystal_query {
            if crystal_field.q == unit.q && crystal_field.r == unit.r {
                if crystal_field.crystals_remaining > 0 {
                    let amount = delta_crystals.min(crystal_field.crystals_remaining);
                    crystal_field.crystals_remaining -= amount;
                    harvester.crystals_carried += amount;
                }
                break;
            }
        }

        // Check if full (10 seconds passed or carried 50 crystals)
        if harvester.harvest_timer >= harvester.harvest_duration || harvester.crystals_carried >= 50 {
            harvester.state = HarvesterState::MovingToBase;
            println!("Harvester full with {} crystals, returning to base at ({}, {})",
                harvester.crystals_carried, harvester.spawn_point.0, harvester.spawn_point.1);
        }
    }
}

// Command harvesters to return to base
fn harvester_return_to_base(
    mut commands: Commands,
    mut harvester_query: Query<(Entity, &Unit, &UnitStats, &mut Harvester), Without<UnitMovement>>,
    occupancy: Res<Occupancy>,
    obstacles: Res<Obstacles>,
    config: Res<HexMapConfig>,
) {
    for (entity, unit, stats, harvester) in &mut harvester_query {
        if harvester.state != HarvesterState::MovingToBase {
            continue;
        }

        let (base_q, base_r) = harvester.spawn_point;

        // Check if we've arrived at base
        if unit.q == base_q && unit.r == base_r {
            // Deposit crystals and reset
            commands.entity(entity).insert(HarvesterDepositing);
            println!("Harvester arrived at base, depositing {} crystals", harvester.crystals_carried);
            continue;
        }

        // Build blocking cells set (obstacles + occupied cells, excluding current position and goal)
        let mut blocking_cells = obstacles.positions.clone();
        for &occupied_pos in &occupancy.positions {
            if occupied_pos != (unit.q, unit.r) && occupied_pos != (base_q, base_r) {
                blocking_cells.insert(occupied_pos);
            }
        }

        // Use proper A* pathfinding
        if let Some(path) = find_path((unit.q, unit.r), (base_q, base_r), config.map_radius, &blocking_cells) {
            let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                path[1..].to_vec()
            } else {
                vec![]
            };

            if !path_to_follow.is_empty() {
                commands.entity(entity).insert(UnitMovement {
                    path: path_to_follow,
                    current_waypoint: 0,
                    progress: 0.0,
                    speed: stats.speed,
                    segment_start: (unit.q, unit.r),
                });
            }
        }
    }
}

// Deposit crystals and reset harvester state
#[derive(Component)]
struct HarvesterDepositing;

fn harvester_deposit_crystals(
    mut commands: Commands,
    mut economy: ResMut<Economy>,
    mut harvester_query: Query<(Entity, &Army, &mut Harvester), With<HarvesterDepositing>>,
) {
    for (entity, army, mut harvester) in &mut harvester_query {
        // Award money (1 crystal = 1 money)
        match army {
            Army::Red => economy.red_money += harvester.crystals_carried,
            Army::Blue => economy.blue_money += harvester.crystals_carried,
        }

        println!("{:?} harvester deposited {} crystals (+${}) at base",
            army, harvester.crystals_carried, harvester.crystals_carried);

        // Reset harvester state
        harvester.state = HarvesterState::Idle;
        harvester.crystals_carried = 0;
        harvester.target_field = None;

        // Remove depositing marker
        commands.entity(entity).remove::<HarvesterDepositing>();
    }
}

fn detect_unit_clicks(
    mouse_button: Res<ButtonInput<MouseButton>>,
    camera_query: Query<(&Camera, &GlobalTransform), With<crate::ui::GameCamera>>,
    windows: Query<&Window>,
    collider_query: Query<(&UnitClickCollider, &GlobalTransform, &Mesh3d)>,
    meshes: Res<Assets<Mesh>>,
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


pub struct UnitsPlugin;

impl Plugin for UnitsPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(Occupancy::default())
            .insert_resource(OccupancyIntent::default())
            .insert_resource(ClaimedCellsThisFrame::default())
            .insert_resource(Economy::default())
            .insert_resource(PassiveIncomeTimer::default())
            .insert_resource(UnitSpawnQueue::default())
            .insert_resource(SpawnCooldowns::default())
            .insert_resource(AIController::default())
            .insert_resource(ClickedUnit::default())
            .insert_resource(HoveredUnit::default())
            .add_systems(OnEnter(LoadingState::Playing), setup_units)
            .add_systems(
                Update,
                (
                    detect_unit_clicks,
                    clear_claimed_cells,
                    reset_game,
                    passive_income_system,
                    update_spawn_cooldowns,
                    ai_spawn_units,
                    ai_command_units,
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
                    remove_dead_units,
                    update_occupancy_intent,
                    update_occupancy,
                    detect_collisions_and_repath,
                    update_unit_animations,
                    play_animation_when_loaded,
                    update_health_bars,
                    update_collision_spheres,
                    harvester_ai_find_target,
                    harvester_move_to_field,
                    harvester_collect_crystals,
                    harvester_return_to_base,
                    harvester_deposit_crystals,
                ).run_if(in_state(LoadingState::Playing)),
            );
    }
}
