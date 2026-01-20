use bevy::prelude::*;
use bevy::gltf::GltfAssetLabel;
use bevy::mesh::{Indices, PrimitiveTopology};
use bevy::asset::RenderAssetUsages;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering;

use crate::map::{axial_to_world_pos, HexMapConfig, Obstacles};
use crate::selection::{SelectionRing, create_selection_ring_mesh};

// Components
#[derive(Component)]
pub struct RedArmy;

#[derive(Component)]
pub struct BlueArmy;

#[derive(Component, Clone, Copy, PartialEq, Eq)]
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
pub struct HealthBar {
    pub unit_entity: Entity,
}

#[derive(Component)]
pub struct HealthBarFill;

#[derive(Component)]
pub struct HealthBarBorder;

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
    mut query: Query<(Entity, &mut Transform, &mut Unit, &mut UnitMovement)>,
) {
    for (entity, mut transform, mut unit, mut movement) in &mut query {
        if movement.current_waypoint >= movement.path.len() {
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
            movement.progress += (time.delta_secs() * movement.speed) / distance;
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
                commands.entity(entity).remove::<UnitMovement>();
            }
        } else {
            let current_pos = start_pos.lerp(target_pos, movement.progress);
            transform.translation.x = current_pos.x;
            transform.translation.z = current_pos.z;
        }
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
    mut players_query: Query<&mut AnimationPlayer>,
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

            let (new_graph, new_index) = if is_moving {
                (anim_graphs.moving_graph.clone(), anim_graphs.moving_index)
            } else {
                (anim_graphs.idle_graph.clone(), anim_graphs.idle_index)
            };

            *graph_handle = AnimationGraphHandle(new_graph.clone());

            let mut found_player = false;
            for descendant in children_query.iter_descendants(unit_entity) {
                if let Ok(mut player) = players_query.get_mut(descendant) {
                    commands
                        .entity(descendant)
                        .insert(AnimationGraphHandle(new_graph.clone()));

                    player.stop_all();
                    player.play(new_index).repeat();
                    println!(
                        "Switched to {} animation (index {:?}) for unit {:?} on entity {:?}",
                        if is_moving { "moving" } else { "idle" },
                        new_index,
                        unit_entity,
                        descendant
                    );
                    found_player = true;
                    break;
                }
            }

            if !found_player {
                println!(
                    "Warning: Could not find AnimationPlayer for unit {:?}",
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
                    .insert(graph_handle.clone());

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
            let unit_world_pos = unit_transform.translation;
            bar_transform.translation = unit_world_pos + Vec3::new(0.0, 40.2, 0.0);
        }
    }

    // Update background bars
    for (health_bar, mut bar_transform) in &mut health_bar_bg_query {
        if let Ok((_, unit_transform)) = unit_query.get(health_bar.unit_entity) {
            let unit_world_pos = unit_transform.translation;
            bar_transform.translation = unit_world_pos + Vec3::new(0.0, 40.1, 0.0);
        }
    }

    // Update border bars
    for (health_bar, mut bar_transform) in &mut health_bar_border_query {
        if let Ok((_, unit_transform)) = unit_query.get(health_bar.unit_entity) {
            let unit_world_pos = unit_transform.translation;
            bar_transform.translation = unit_world_pos + Vec3::new(0.0, 40.0, 0.0);
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
    let stickman_scene: Handle<Scene> = asset_server.load("Fox.glb#Scene0");

    let units = vec![
        (-3, 1, 0, Army::Red),
        (3, 1, 1, Army::Blue),
        (-4, 1, 2, Army::Red),
        (4, 1, 3, Army::Blue),
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
        for (q, r, unit_index, army) in units.iter().filter(|(_, _, _, a)| *a == Army::Red) {
            let world_pos = axial_to_world_pos(*q, *r);
            let unit_pos = world_pos + Vec3::new(0.0, 5.0, 0.0);

            let (idle_graph, idle_index) = AnimationGraph::from_clip(
                asset_server.load(GltfAssetLabel::Animation(0).from_asset("Fox.glb")),
            );
            let (moving_graph, moving_index) = AnimationGraph::from_clip(
                asset_server.load(GltfAssetLabel::Animation(2).from_asset("Fox.glb")),
            );
            let idle_graph_handle = animation_graphs.add(idle_graph);
            let moving_graph_handle = animation_graphs.add(moving_graph);

            let unit_entity = parent
                .spawn((
                    SceneRoot(stickman_scene.clone()),
                    Transform::from_translation(unit_pos).with_scale(Vec3::splat(0.5)),
                    Unit {
                        q: *q,
                        r: *r,
                        _sprite_index: *unit_index,
                        army: *army,
                    },
                    AnimationGraphHandle(idle_graph_handle.clone()),
                    AnimationGraphs {
                        idle_graph: idle_graph_handle,
                        idle_index,
                        moving_graph: moving_graph_handle,
                        moving_index,
                    },
                    CurrentAnimationState { is_moving: false },
                    Health {
                        current: 100.0,
                        max: 100.0,
                    },
                    Name::new(format!("Unit {} ({}, {})", unit_index, q, r)),
                ))
                .id();

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

            // Spawn health bar above unit
            let bar_width = 40.0;
            let bar_height = 10.0;
            let border_width_sides = 4.0;
            let border_height_extra = 8.0; // 4px extra on top and bottom (2px more each)
            let bar_pos = world_pos + Vec3::new(0.0, 40.0, 0.0);
            let health_bar_mesh = meshes.add(create_health_bar_mesh(bar_width, bar_height));
            let border_mesh = meshes.add(create_health_bar_mesh(
                bar_width + border_width_sides,
                bar_height + border_height_extra,
            ));

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
                Transform::from_translation(bar_pos),
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
                Transform::from_translation(bar_pos + Vec3::new(0.0, 0.1, 0.0)),
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
                Transform::from_translation(bar_pos + Vec3::new(0.0, 0.2, 0.0)),
                HealthBar { unit_entity },
                HealthBarFill,
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
        for (q, r, unit_index, army) in units.iter().filter(|(_, _, _, a)| *a == Army::Blue) {
            let world_pos = axial_to_world_pos(*q, *r);
            let unit_pos = world_pos + Vec3::new(0.0, 5.0, 0.0);

            let (idle_graph, idle_index) = AnimationGraph::from_clip(
                asset_server.load(GltfAssetLabel::Animation(0).from_asset("Fox.glb")),
            );
            let (moving_graph, moving_index) = AnimationGraph::from_clip(
                asset_server.load(GltfAssetLabel::Animation(2).from_asset("Fox.glb")),
            );
            let idle_graph_handle = animation_graphs.add(idle_graph);
            let moving_graph_handle = animation_graphs.add(moving_graph);

            let unit_entity = parent
                .spawn((
                    SceneRoot(stickman_scene.clone()),
                    Transform::from_translation(unit_pos).with_scale(Vec3::splat(0.5)),
                    Unit {
                        q: *q,
                        r: *r,
                        _sprite_index: *unit_index,
                        army: *army,
                    },
                    AnimationGraphHandle(idle_graph_handle.clone()),
                    AnimationGraphs {
                        idle_graph: idle_graph_handle,
                        idle_index,
                        moving_graph: moving_graph_handle,
                        moving_index,
                    },
                    CurrentAnimationState { is_moving: false },
                    Health {
                        current: 100.0,
                        max: 100.0,
                    },
                    Name::new(format!("Unit {} ({}, {})", unit_index, q, r)),
                ))
                .id();

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

            // Spawn health bar above unit
            let bar_width = 40.0;
            let bar_height = 10.0;
            let border_width_sides = 4.0;
            let border_height_extra = 8.0; // 4px extra on top and bottom (2px more each)
            let bar_pos = world_pos + Vec3::new(0.0, 40.0, 0.0);
            let health_bar_mesh = meshes.add(create_health_bar_mesh(bar_width, bar_height));
            let border_mesh = meshes.add(create_health_bar_mesh(
                bar_width + border_width_sides,
                bar_height + border_height_extra,
            ));

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
                Transform::from_translation(bar_pos),
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
                Transform::from_translation(bar_pos + Vec3::new(0.0, 0.1, 0.0)),
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
                Transform::from_translation(bar_pos + Vec3::new(0.0, 0.2, 0.0)),
                HealthBar { unit_entity },
                HealthBarFill,
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
                    move_units,
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
