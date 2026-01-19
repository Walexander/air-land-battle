use bevy::prelude::*;
use bevy::gltf::GltfAssetLabel;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering;

use crate::map::{axial_to_world_pos, HexMapConfig, Obstacles};
use crate::selection::{SelectionRing, create_selection_ring_mesh};

// Components
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

        let current_hex = (unit.q, unit.r);
        let target_hex = movement.path[movement.current_waypoint];

        let start_pos = axial_to_world_pos(current_hex.0, current_hex.1);
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

        if movement.progress >= 1.0 {
            unit.q = target_hex.0;
            unit.r = target_hex.1;
            transform.translation.x = target_pos.x;
            transform.translation.z = target_pos.z;

            movement.current_waypoint += 1;
            movement.progress = 0.0;

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
                let next_cell = movement.path[movement.current_waypoint];
                occupancy_intent.intentions.insert(entity, next_cell);
            } else {
                occupancy_intent
                    .intentions
                    .insert(entity, (unit.q, unit.r));
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
    for (entity, unit, movement_opt) in &unit_query {
        let current_cell = (unit.q, unit.r);

        let occupied_cell = if let Some(movement) = movement_opt {
            if movement.current_waypoint < movement.path.len() && movement.progress >= 0.5 {
                movement.path[movement.current_waypoint]
            } else {
                current_cell
            }
        } else {
            current_cell
        };

        occupancy.positions.insert(occupied_cell);
        occupancy.position_to_entity.insert(occupied_cell, entity);
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
                    next_cell != current_cell && entity.to_bits() > occupying_entity.to_bits()
                } else {
                    false
                };

            let should_yield_to_intent = movement.progress >= 0.4
                && occupancy_intent.intentions.iter().any(
                    |(other_entity, &intent_pos)| {
                        if *other_entity != entity && intent_pos == next_cell {
                            entity.to_bits() > other_entity.to_bits()
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
                        unit_component.q = next_cell.0;
                        unit_component.r = next_cell.1;

                        movement.path = new_path;
                        movement.current_waypoint = 0;
                        movement.progress = 1.0 - old_movement.progress;
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
    ];

    let ring_mesh = meshes.add(create_selection_ring_mesh(20.0, 30.0));
    let ring_material = materials.add(StandardMaterial {
        base_color: Color::srgb(1.0, 1.0, 1.0),
        emissive: Color::srgb(1.0, 1.0, 1.0).into(),
        unlit: true,
        ..default()
    });

    for (q, r, unit_index, army) in units {
        let world_pos = axial_to_world_pos(q, r);
        let unit_pos = world_pos + Vec3::new(0.0, 5.0, 0.0);

        let (idle_graph, idle_index) = AnimationGraph::from_clip(
            asset_server.load(GltfAssetLabel::Animation(0).from_asset("Fox.glb")),
        );
        let (moving_graph, moving_index) = AnimationGraph::from_clip(
            asset_server.load(GltfAssetLabel::Animation(2).from_asset("Fox.glb")),
        );
        let idle_graph_handle = animation_graphs.add(idle_graph);
        let moving_graph_handle = animation_graphs.add(moving_graph);

        let unit_entity = commands
            .spawn((
                SceneRoot(stickman_scene.clone()),
                Transform::from_translation(unit_pos).with_scale(Vec3::splat(0.5)),
                Unit {
                    q,
                    r,
                    _sprite_index: unit_index,
                    army,
                },
                AnimationGraphHandle(idle_graph_handle.clone()),
                AnimationGraphs {
                    idle_graph: idle_graph_handle,
                    idle_index,
                    moving_graph: moving_graph_handle,
                    moving_index,
                },
                CurrentAnimationState { is_moving: false },
                Name::new(format!("Unit {} ({}, {})", unit_index, q, r)),
            ))
            .id();

        let ring_pos = world_pos + Vec3::new(0.0, 6.0, 0.0);
        let ring_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
        commands.spawn((
            Mesh3d(ring_mesh.clone()),
            MeshMaterial3d(ring_material.clone()),
            Transform::from_translation(ring_pos).with_rotation(ring_rotation),
            SelectionRing {
                unit_entity,
                animation_timer: 0.0,
            },
            Visibility::Hidden,
        ));
    }
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
                ),
            );
    }
}
