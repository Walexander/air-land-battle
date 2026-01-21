use bevy::prelude::*;
use bevy::gltf::GltfAssetLabel;
use bevy::mesh::{Indices, PrimitiveTopology};
use bevy::asset::RenderAssetUsages;
use std::time::Duration;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering;
use rand::Rng;

use crate::map::{axial_to_world_pos, HexMapConfig, Obstacles};
use crate::selection::{SelectionRing, create_selection_ring_mesh};
use crate::launch_pads::{GameState, GameTimer};

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

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnitClass {
    Infantry,
    Cavalry,
    Artillery,
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
        }
    }

    pub fn base_cooldown(&self) -> f32 {
        match self {
            UnitClass::Infantry => 2.0,
            UnitClass::Cavalry => 1.5,
            UnitClass::Artillery => 3.0,
        }
    }

    pub fn model_path(&self) -> &'static str {
        match self {
            UnitClass::Infantry => "walking-rifle.glb",
            UnitClass::Cavalry => "Fox.glb",
            UnitClass::Artillery => "CesiumMan.glb",
        }
    }

    pub fn scale(&self) -> f32 {
        match self {
            UnitClass::Infantry => 12.0,
            UnitClass::Cavalry => 0.5,
            UnitClass::Artillery => 30.0,
        }
    }

    pub fn idle_animation_index(&self) -> usize {
        match self {
            UnitClass::Infantry => 0,
            UnitClass::Cavalry => 0,
            UnitClass::Artillery => 0, // CesiumMan has walking animation at index 0
        }
    }

    pub fn moving_animation_index(&self) -> usize {
        match self {
            UnitClass::Infantry => 0,  // walking-rifle only has Animation0
            UnitClass::Cavalry => 2,
            UnitClass::Artillery => 0, // CesiumMan only has one animation at index 0
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

// Resources
#[derive(Resource, Default)]
pub struct Occupancy {
    pub positions: HashSet<(i32, i32)>,
    pub position_to_entity: HashMap<(i32, i32), Entity>,
}

#[derive(Resource, Default)]
pub struct OccupancyIntent {
    pub intentions: HashMap<Entity, (i32, i32)>,
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

// Systems
fn move_units(
    time: Res<Time>,
    mut commands: Commands,
    mut query: Query<(Entity, &mut Transform, &mut Unit, &mut UnitMovement, &UnitStats, Option<&mut Combat>)>,
) {
    let current_time = time.elapsed_secs();

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
            unit.q = target_hex.0;
            unit.r = target_hex.1;
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
        if let Ok((_, _, _, _, _, mut defender_health, _)) = unit_query.get_mut(defender_entity) {
            if defender_health.current <= 0.0 {
                continue; // Already dead
            }

            defender_health.current -= damage;
            println!("⚔️  {:?} {:?} attacks {:?} {:?} for {:.1} damage!",
                attacker_army, attacker_entity, defender_army, defender_entity, damage);
            println!("   └─ Defender health: {:.1}/{:.1}", defender_health.current, defender_health.max);

            // Update attacker's last attack time
            if let Ok((_, _, _, _, mut attacker_combat, _, _)) = unit_query.get_mut(attacker_entity) {
                attacker_combat.last_attack_time = current_time;
            }

            // Add flash effect to attacker
            commands.entity(attacker_entity).insert(FlashEffect {
                timer: 0.0,
                duration: 0.15, // Flash for 0.15 seconds
            });
        }
    }
}

fn remove_dead_units(
    mut commands: Commands,
    unit_query: Query<(Entity, &Health, &Unit)>,
    children_query: Query<&Children>,
    health_bar_query: Query<(Entity, &HealthBar)>,
    selection_ring_query: Query<(Entity, &crate::selection::SelectionRing)>,
) {
    for (entity, health, _unit) in &unit_query {
        if health.current <= 0.0 {
            println!("Unit {:?} has been destroyed!", entity);

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

        // Respawn units by calling setup_units logic
        setup_units(commands, meshes, materials, animation_graphs, asset_server);

        println!("Game reset complete!");
    }
}

fn update_occupancy_intent(
    unit_query: Query<(Entity, &Unit, &UnitMovement)>,
    mut occupancy_intent: ResMut<OccupancyIntent>,
) {
    occupancy_intent.intentions.clear();
    for (entity, unit, movement) in &unit_query {
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

                    println!("Unit repathing to avoid collision!");
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
            println!(
                "Unit {:?}: movement state changing from {} to {}",
                unit_entity, anim_state.is_moving, is_moving
            );
        }

        if is_moving != anim_state.is_moving {
            anim_state.is_moving = is_moving;

            let new_index = if is_moving {
                anim_graphs.moving_index
            } else {
                anim_graphs.idle_index
            };

            // Check if idle and moving animations are the same (e.g., Infantry with only one animation)
            let same_animation = anim_graphs.idle_index == anim_graphs.moving_index;

            let mut found_players = 0;
            for descendant in children_query.iter_descendants(unit_entity) {
                if let Ok((mut player, mut transitions)) = players_query.get_mut(descendant) {
                    if same_animation {
                        // Same animation for idle and moving - pause when idle, play when moving
                        if is_moving {
                            player.resume_all();
                            println!("Resumed animation for unit {:?} on entity {:?}", unit_entity, descendant);
                        } else {
                            player.pause_all();
                            println!("Paused animation for unit {:?} on entity {:?}", unit_entity, descendant);
                        }
                    } else {
                        // Different animations - transition between them
                        transitions
                            .play(&mut player, new_index, Duration::from_secs_f32(0.2))
                            .repeat();

                        println!(
                            "Switched to {} animation (index {:?}) for unit {:?} on entity {:?}",
                            if is_moving { "moving" } else { "idle" },
                            new_index,
                            unit_entity,
                            descendant
                        );
                    }
                    found_players += 1;
                }
            }

            if found_players == 0 {
                println!(
                    "Warning: Could not find AnimationPlayer for unit {:?}",
                    unit_entity
                );
            } else {
                println!(
                    "Updated {} AnimationPlayer(s) for unit {:?}",
                    found_players,
                    unit_entity
                );
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
        println!(
            "Found newly added AnimationPlayer on entity {:?}",
            player_entity
        );

        for (unit_entity, anim_graphs, graph_handle) in &units_query {
            let mut is_descendant = false;
            for descendant in children_query.iter_descendants(unit_entity) {
                if descendant == player_entity {
                    is_descendant = true;
                    break;
                }
            }

            if is_descendant {
                println!("AnimationPlayer belongs to unit {:?}", unit_entity);

                commands
                    .entity(player_entity)
                    .insert((
                        graph_handle.clone(),
                        AnimationTransitions::new(),
                    ));

                player.play(anim_graphs.idle_index).repeat();
                println!(
                    "Started idle animation {:?} on entity {:?}",
                    anim_graphs.idle_index, player_entity
                );

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
            let flash_mesh = meshes.add(Sphere::new(30.0).mesh().ico(2).unwrap());
            let flash_material = materials.add(StandardMaterial {
                base_color: Color::srgb(3.0, 3.0, 0.0), // Very bright yellow
                emissive: Color::srgb(3.0, 3.0, 0.0).into(),
                unlit: true,
                alpha_mode: AlphaMode::Blend,
                ..default()
            });

            let flash_pos = transform.translation + Vec3::new(0.0, 10.0, 0.0);
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

fn setup_units(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
    asset_server: Res<AssetServer>,
) {
    // (q, r, unit_index, army, class)
    let units = vec![
        // Red Army - One of each type
        (-3, 1, 0, Army::Red, UnitClass::Infantry),
        (-4, 1, 1, Army::Red, UnitClass::Cavalry),
        (-4, 2, 2, Army::Red, UnitClass::Artillery),

        // Blue Army - Mirrored positions
        (3, 1, 3, Army::Blue, UnitClass::Infantry),
        (4, 0, 4, Army::Blue, UnitClass::Cavalry),
        (3, 2, 5, Army::Blue, UnitClass::Artillery),
    ];

    let ring_mesh = meshes.add(create_selection_ring_mesh(55.0, 63.0));
    let ring_material = materials.add(StandardMaterial {
        base_color: Color::srgb(0.7, 0.7, 0.7),
        emissive: Color::srgb(0.7, 0.7, 0.7).into(),
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

            // Fill (red for red army)
            parent.spawn((
                Mesh3d(health_bar_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.9, 0.2, 0.2),
                    emissive: Color::srgb(0.9, 0.2, 0.2).into(),
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
                    .with_scale(Vec3::splat(0.75)),
                SelectionRing {
                    unit_entity,
                    animation_timer: 0.0,
                    bounce_count: 0,
                },
                Visibility::Hidden,
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
                    .with_scale(Vec3::splat(0.75)),
                SelectionRing {
                    unit_entity,
                    animation_timer: 0.0,
                    bounce_count: 0,
                },
                Visibility::Hidden,
            ));
        }
    });
}

pub struct UnitsPlugin;

impl Plugin for UnitsPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(Occupancy::default())
            .insert_resource(OccupancyIntent::default())
            .add_systems(Startup, setup_units)
            .add_systems(
                Update,
                (
                    reset_game,
                    move_units,
                    rotate_units_toward_enemies,
                    combat_system,
                    handle_flash_effects,
                    cleanup_flash_visuals,
                    remove_dead_units,
                    update_occupancy_intent,
                    update_occupancy,
                    detect_collisions_and_repath,
                    update_unit_animations,
                    play_animation_when_loaded,
                    update_health_bars,
                ),
            );
    }
}
