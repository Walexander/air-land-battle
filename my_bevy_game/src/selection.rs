use bevy::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::mesh::{Indices, PrimitiveTopology};

use crate::map::{axial_to_world_pos, HexMapConfig, HexTile, HoveredHex, Obstacles};
use crate::units::{find_path, Occupancy, OccupancyIntent, Unit, UnitMovement};

// Components
#[derive(Component)]
pub struct Selected;

#[derive(Component)]
pub struct SelectionRing {
    pub unit_entity: Entity,
    pub animation_timer: f32,
}

#[derive(Component)]
pub struct PathVisualization {
    pub unit_entity: Entity,
    pub animation_progress: f32,
    pub loop_count: u32,
}

#[derive(Component)]
pub struct DestinationRing {
    pub unit_entity: Entity,
    pub animation_timer: f32,
}

// Mesh creation functions
pub fn create_selection_ring_mesh(inner_radius: f32, outer_radius: f32) -> Mesh {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    let segments = 32;

    for i in 0..segments {
        let angle = (i as f32 / segments as f32) * std::f32::consts::PI * 2.0;
        let cos = angle.cos();
        let sin = angle.sin();

        positions.push([outer_radius * cos, 0.1, outer_radius * sin]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.5 + cos * 0.5, 0.5 + sin * 0.5]);

        positions.push([inner_radius * cos, 0.1, inner_radius * sin]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.5 + cos * 0.3, 0.5 + sin * 0.3]);
    }

    for i in 0..segments {
        let outer_current = (i * 2) as u32;
        let inner_current = (i * 2 + 1) as u32;
        let outer_next = ((i * 2 + 2) % (segments * 2)) as u32;
        let inner_next = ((i * 2 + 3) % (segments * 2)) as u32;

        indices.push(outer_current);
        indices.push(inner_current);
        indices.push(outer_next);

        indices.push(inner_current);
        indices.push(inner_next);
        indices.push(outer_next);
    }

    let mut mesh = Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::default(),
    );
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
    mesh.insert_indices(Indices::U32(indices));
    mesh
}

pub fn spawn_destination_ring(
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    unit_entity: Entity,
    destination: (i32, i32),
) {
    let dest_pos = axial_to_world_pos(destination.0, destination.1);
    let ring_pos = dest_pos + Vec3::new(0.0, 6.5, 0.0);

    let ring_mesh = meshes.add(create_selection_ring_mesh(25.0, 35.0));
    let ring_material = materials.add(StandardMaterial {
        base_color: Color::srgb(0.0, 0.5, 0.0),
        emissive: Color::srgb(0.0, 0.5, 0.0).into(),
        unlit: true,
        ..default()
    });

    let ring_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
    commands.spawn((
        Mesh3d(ring_mesh),
        MeshMaterial3d(ring_material),
        Transform::from_translation(ring_pos).with_rotation(ring_rotation),
        DestinationRing {
            unit_entity,
            animation_timer: 0.0,
        },
    ));
}

fn create_path_line_mesh(
    waypoints: &[(i32, i32)],
    current_pos: Vec3,
    animation_progress: f32,
) -> Mesh {
    let mut positions = Vec::new();
    let mut colors = Vec::new();
    let mut indices = Vec::new();

    if waypoints.is_empty() {
        let mut mesh = Mesh::new(
            PrimitiveTopology::TriangleList,
            RenderAssetUsages::default(),
        );
        mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
        mesh.insert_attribute(Mesh::ATTRIBUTE_COLOR, colors);
        mesh.insert_indices(Indices::U32(indices));
        return mesh;
    }

    let destination_ring_outer_radius = 35.0;
    let line_width = 8.0;
    let line_height = 8.0;
    let white_color = [1.0, 1.0, 1.0, 1.0];
    let dark_color = [0.0, 0.5, 0.0, 1.0];
    let dark_segment_length = 25.0;

    let mut prev_pos = Vec3::new(current_pos.x, line_height, current_pos.z);
    let mut accumulated_distance = 0.0;
    let subdivision_length = 0.5;

    // Add starting circle
    let circle_segments = 8;
    let circle_radius = line_width * 0.5;
    let center_idx = positions.len() as u32;

    positions.push([prev_pos.x, prev_pos.y, prev_pos.z]);
    colors.push(white_color);

    for i in 0..circle_segments {
        let angle = (i as f32 / circle_segments as f32) * std::f32::consts::PI * 2.0;
        let x_offset = circle_radius * angle.cos();
        let z_offset = circle_radius * angle.sin();

        positions.push([
            prev_pos.x + x_offset,
            prev_pos.y,
            prev_pos.z + z_offset,
        ]);
        colors.push(white_color);
    }

    for i in 0..circle_segments {
        let current_vertex = center_idx + 1 + i as u32;
        let next_vertex = center_idx + 1 + ((i + 1) % circle_segments) as u32;

        indices.push(center_idx);
        indices.push(current_vertex);
        indices.push(next_vertex);
    }

    for (idx, &(q, r)) in waypoints.iter().enumerate() {
        let waypoint_pos = axial_to_world_pos(q, r);
        let mut curr_pos = Vec3::new(waypoint_pos.x, line_height, waypoint_pos.z);

        let is_last = idx == waypoints.len() - 1;
        if is_last {
            let direction_to_dest = (curr_pos - prev_pos).normalize();
            let full_distance = prev_pos.distance(curr_pos);
            if full_distance > destination_ring_outer_radius {
                curr_pos = curr_pos - direction_to_dest * destination_ring_outer_radius;
            }
        }

        let segment_length = prev_pos.distance(curr_pos);
        let direction = (curr_pos - prev_pos).normalize();
        let perpendicular = Vec3::new(-direction.z, 0.0, direction.x) * line_width * 0.5;

        let num_subdivisions = (segment_length / subdivision_length).ceil() as i32;
        let num_subdivisions = num_subdivisions.max(1);

        for i in 0..num_subdivisions {
            let t_start = i as f32 / num_subdivisions as f32;
            let t_end = (i + 1) as f32 / num_subdivisions as f32;

            let sub_start = prev_pos.lerp(curr_pos, t_start);
            let sub_end = prev_pos.lerp(curr_pos, t_end);

            let sub_start_dist = accumulated_distance + (segment_length * t_start);
            let sub_end_dist = accumulated_distance + (segment_length * t_end);

            let base_idx = positions.len() as u32;

            positions.push([
                sub_start.x - perpendicular.x,
                sub_start.y,
                sub_start.z - perpendicular.z,
            ]);
            positions.push([
                sub_start.x + perpendicular.x,
                sub_start.y,
                sub_start.z + perpendicular.z,
            ]);
            positions.push([
                sub_end.x + perpendicular.x,
                sub_end.y,
                sub_end.z + perpendicular.z,
            ]);
            positions.push([
                sub_end.x - perpendicular.x,
                sub_end.y,
                sub_end.z - perpendicular.z,
            ]);

            let dark_start = animation_progress;
            let dark_end = animation_progress + dark_segment_length;

            let segment_color = if dark_start <= sub_end_dist && dark_end >= sub_start_dist {
                dark_color
            } else {
                white_color
            };

            colors.push(segment_color);
            colors.push(segment_color);
            colors.push(segment_color);
            colors.push(segment_color);

            indices.push(base_idx);
            indices.push(base_idx + 1);
            indices.push(base_idx + 2);

            indices.push(base_idx);
            indices.push(base_idx + 2);
            indices.push(base_idx + 3);
        }

        if !is_last {
            let circle_segments = 8;
            let circle_radius = line_width * 0.5;
            let center_idx = positions.len() as u32;

            positions.push([curr_pos.x, curr_pos.y, curr_pos.z]);
            colors.push(white_color);

            for i in 0..circle_segments {
                let angle = (i as f32 / circle_segments as f32) * std::f32::consts::PI * 2.0;
                let x_offset = circle_radius * angle.cos();
                let z_offset = circle_radius * angle.sin();

                positions.push([
                    curr_pos.x + x_offset,
                    curr_pos.y,
                    curr_pos.z + z_offset,
                ]);
                colors.push(white_color);
            }

            for i in 0..circle_segments {
                let current_vertex = center_idx + 1 + i as u32;
                let next_vertex = center_idx + 1 + ((i + 1) % circle_segments) as u32;

                indices.push(center_idx);
                indices.push(current_vertex);
                indices.push(next_vertex);
            }
        }

        accumulated_distance += segment_length;
        prev_pos = curr_pos;
    }

    let mut mesh = Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::default(),
    );
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_COLOR, colors);
    mesh.insert_indices(Indices::U32(indices));
    mesh
}

// Systems
fn handle_unit_selection(
    mouse_button: Res<ButtonInput<MouseButton>>,
    hovered_hex: Res<HoveredHex>,
    hex_query: Query<&HexTile>,
    config: Res<HexMapConfig>,
    obstacles: Res<Obstacles>,
    occupancy: Res<Occupancy>,
    occupancy_intent: Res<OccupancyIntent>,
    unit_query: Query<(Entity, &Unit, Option<&UnitMovement>), Without<Selected>>,
    selected_query: Query<(Entity, &Unit, Option<&UnitMovement>, &Transform), With<Selected>>,
    path_viz_query: Query<(Entity, &PathVisualization)>,
    dest_ring_query: Query<(Entity, &DestinationRing)>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut commands: Commands,
) {
    if mouse_button.just_pressed(MouseButton::Left) {
        if let Some(hovered_entity) = hovered_hex.entity {
            if let Ok(hovered_tile) = hex_query.get(hovered_entity) {
                let mut clicked_unit = None;
                for (entity, unit, movement) in &unit_query {
                    if movement.is_none()
                        && unit.q == hovered_tile.q
                        && unit.r == hovered_tile.r
                    {
                        clicked_unit = Some(entity);
                        break;
                    }
                }

                if let Some(unit_entity) = clicked_unit {
                    for (entity, _, _, _) in &selected_query {
                        commands.entity(entity).remove::<Selected>();
                    }
                    commands.entity(unit_entity).insert(Selected);
                } else {
                    if let Ok((selected_entity, selected_unit, existing_movement, _unit_transform)) =
                        selected_query.single()
                    {
                        let goal = (hovered_tile.q, hovered_tile.r);

                        if obstacles.positions.contains(&goal) {
                            return;
                        }

                        let mut blocking_cells = obstacles.positions.clone();
                        let unit_current_pos = (selected_unit.q, selected_unit.r);
                        for &occupied_pos in &occupancy.positions {
                            if occupied_pos != unit_current_pos && occupied_pos != goal {
                                blocking_cells.insert(occupied_pos);
                            }
                        }
                        for (entity, &intent_pos) in &occupancy_intent.intentions {
                            if *entity != selected_entity
                                && intent_pos != unit_current_pos
                                && intent_pos != goal
                            {
                                blocking_cells.insert(intent_pos);
                            }
                        }

                        for (viz_entity, path_viz) in &path_viz_query {
                            if path_viz.unit_entity == selected_entity {
                                commands.entity(viz_entity).despawn();
                                break;
                            }
                        }
                        for (ring_entity, dest_ring) in &dest_ring_query {
                            if dest_ring.unit_entity == selected_entity {
                                commands.entity(ring_entity).despawn();
                                break;
                            }
                        }

                        if let Some(movement) = existing_movement {
                            let current_cell = (selected_unit.q, selected_unit.r);
                            let next_cell = movement.path[movement.current_waypoint];

                            let path_from_current =
                                find_path(current_cell, goal, config.map_radius, &blocking_cells);
                            let path_from_next =
                                find_path(next_cell, goal, config.map_radius, &blocking_cells);

                            let should_reverse = match (path_from_current, path_from_next) {
                                (Some(p1), Some(p2)) => p1.len() < p2.len(),
                                (Some(_), None) => true,
                                (None, Some(_)) => false,
                                (None, None) => false,
                            };

                            if should_reverse {
                                if let Some(path) = find_path(
                                    current_cell,
                                    goal,
                                    config.map_radius,
                                    &blocking_cells,
                                ) {
                                    let mut new_path = vec![current_cell];
                                    if path.len() > 1 {
                                        new_path.extend_from_slice(&path[1..]);
                                    }

                                    commands.entity(selected_entity).insert((
                                        Unit {
                                            q: next_cell.0,
                                            r: next_cell.1,
                                            _sprite_index: selected_unit._sprite_index,
                                            army: selected_unit.army,
                                        },
                                        UnitMovement {
                                            path: new_path,
                                            current_waypoint: 0,
                                            progress: 1.0 - movement.progress,
                                            speed: 100.0,
                                        },
                                    ));

                                    spawn_destination_ring(
                                        &mut commands,
                                        &mut meshes,
                                        &mut materials,
                                        selected_entity,
                                        goal,
                                    );
                                }
                            } else {
                                if let Some(path) =
                                    find_path(next_cell, goal, config.map_radius, &blocking_cells)
                                {
                                    let mut new_full_path = vec![next_cell];
                                    if path.len() > 1 {
                                        new_full_path.extend_from_slice(&path[1..]);
                                    }

                                    if new_full_path.len() > 1 {
                                        commands.entity(selected_entity).insert(UnitMovement {
                                            path: new_full_path,
                                            current_waypoint: 0,
                                            progress: movement.progress,
                                            speed: 100.0,
                                        });

                                        spawn_destination_ring(
                                            &mut commands,
                                            &mut meshes,
                                            &mut materials,
                                            selected_entity,
                                            goal,
                                        );
                                    }
                                }
                            }
                        } else {
                            let start = (selected_unit.q, selected_unit.r);
                            if let Some(path) =
                                find_path(start, goal, config.map_radius, &blocking_cells)
                            {
                                let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                                    path[1..].to_vec()
                                } else {
                                    vec![]
                                };

                                if !path_to_follow.is_empty() {
                                    println!(
                                        "User-commanded unit moving from ({}, {}) to destination ({}, {})",
                                        start.0, start.1, goal.0, goal.1
                                    );
                                    commands.entity(selected_entity).insert(UnitMovement {
                                        path: path_to_follow,
                                        current_waypoint: 0,
                                        progress: 0.0,
                                        speed: 100.0,
                                    });

                                    spawn_destination_ring(
                                        &mut commands,
                                        &mut meshes,
                                        &mut materials,
                                        selected_entity,
                                        goal,
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn update_selected_visual(
    selected_query: Query<&MeshMaterial3d<StandardMaterial>, (With<Unit>, With<Selected>)>,
    unselected_query: Query<&MeshMaterial3d<StandardMaterial>, (With<Unit>, Without<Selected>)>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    for material_handle in &selected_query {
        if let Some(material) = materials.get_mut(&material_handle.0) {
            material.emissive = Color::srgb(0.5, 0.5, 0.0).into();
        }
    }

    for material_handle in &unselected_query {
        if let Some(material) = materials.get_mut(&material_handle.0) {
            material.emissive = Color::BLACK.into();
        }
    }
}

fn animate_selection_rings(
    time: Res<Time>,
    unit_query: Query<(Entity, &Transform, Has<Selected>), With<Unit>>,
    mut ring_query: Query<
        (&mut SelectionRing, &mut Transform, &mut Visibility),
        Without<Unit>,
    >,
) {
    for (mut ring, mut ring_transform, mut visibility) in &mut ring_query {
        if let Ok((_, unit_transform, is_selected)) = unit_query.get(ring.unit_entity) {
            let was_visible = *visibility == Visibility::Visible;
            *visibility = if is_selected {
                Visibility::Visible
            } else {
                Visibility::Hidden
            };

            if is_selected && !was_visible {
                ring.animation_timer = 0.0;
            }

            ring_transform.translation.x = unit_transform.translation.x;
            ring_transform.translation.y = unit_transform.translation.y + 1.0;
            ring_transform.translation.z = unit_transform.translation.z;

            if is_selected {
                ring.animation_timer += time.delta_secs();

                let animation_duration = 0.5;
                let progress = (ring.animation_timer / animation_duration).min(1.0);

                let start_scale = 2.5;
                let end_scale = 1.0;
                let current_scale = start_scale + (end_scale - start_scale) * progress;

                ring_transform.scale = Vec3::new(current_scale, 1.0, current_scale);
                ring_transform.rotate_y(time.delta_secs() * 1.0);
            }
        }
    }
}

fn animate_destination_rings(
    time: Res<Time>,
    unit_query: Query<(Entity, &UnitMovement)>,
    mut ring_query: Query<(Entity, &mut DestinationRing, &mut Transform)>,
    mut commands: Commands,
) {
    let moving_units: std::collections::HashSet<Entity> =
        unit_query.iter().map(|(e, _)| e).collect();

    let mut to_despawn = Vec::new();

    for (entity, mut ring, mut ring_transform) in &mut ring_query {
        if !moving_units.contains(&ring.unit_entity) {
            to_despawn.push(entity);
            continue;
        }

        ring.animation_timer += time.delta_secs();

        let pulse_frequency = 2.0;
        let pulse_progress = (ring.animation_timer * pulse_frequency).sin();

        let min_scale = 0.9;
        let max_scale = 1.1;
        let current_scale = min_scale + (max_scale - min_scale) * ((pulse_progress + 1.0) / 2.0);

        ring_transform.scale = Vec3::new(current_scale, 1.0, current_scale);
        ring_transform.rotate_y(time.delta_secs() * 0.5);
    }

    for entity in to_despawn {
        commands.entity(entity).despawn();
    }
}

fn update_path_visualizations(
    time: Res<Time>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    unit_query: Query<(Entity, &Unit, &Transform, Option<&UnitMovement>)>,
    mut path_viz_query: Query<(Entity, &mut PathVisualization, &mut Mesh3d)>,
) {
    let mut units_with_movement = std::collections::HashSet::new();
    for (unit_entity, _, _, movement) in &unit_query {
        if movement.is_some() {
            units_with_movement.insert(unit_entity);
        }
    }

    for (viz_entity, path_viz, _) in &path_viz_query {
        if !units_with_movement.contains(&path_viz.unit_entity) {
            commands.entity(viz_entity).despawn();
        }
    }

    for (unit_entity, _unit, transform, movement) in &unit_query {
        if let Some(movement) = movement {
            let remaining_path = &movement.path[movement.current_waypoint..];

            let mut total_length = 0.0;
            let mut prev_pos = transform.translation;
            for &(q, r) in remaining_path {
                let waypoint_pos = axial_to_world_pos(q, r);
                let curr_pos = Vec3::new(waypoint_pos.x, 0.0, waypoint_pos.z);
                total_length += prev_pos.distance(curr_pos);
                prev_pos = curr_pos;
            }

            let mut found = false;
            for (_viz_entity, mut path_viz, mut mesh_handle) in &mut path_viz_query {
                if path_viz.unit_entity == unit_entity {
                    found = true;

                    if path_viz.loop_count < 2 {
                        let animation_speed = movement.speed * 2.0;
                        path_viz.animation_progress += time.delta_secs() * animation_speed;

                        if total_length > 0.0 && path_viz.animation_progress >= total_length {
                            path_viz.loop_count += 1;
                            path_viz.animation_progress %= total_length;
                        }
                    }

                    let animation_progress = if path_viz.loop_count >= 2 {
                        total_length + 100.0
                    } else {
                        path_viz.animation_progress
                    };

                    let new_mesh = create_path_line_mesh(
                        remaining_path,
                        transform.translation,
                        animation_progress,
                    );
                    mesh_handle.0 = meshes.add(new_mesh);
                    break;
                }
            }

            if !found {
                let path_mesh =
                    create_path_line_mesh(remaining_path, transform.translation, 0.0);

                commands.spawn((
                    Mesh3d(meshes.add(path_mesh)),
                    MeshMaterial3d(materials.add(StandardMaterial {
                        base_color: Color::srgba(1.0, 1.0, 1.0, 1.0),
                        alpha_mode: AlphaMode::Blend,
                        unlit: true,
                        cull_mode: None,
                        ..default()
                    })),
                    Transform::default(),
                    PathVisualization {
                        unit_entity,
                        animation_progress: 0.0,
                        loop_count: 0,
                    },
                ));
            }
        }
    }
}

pub struct SelectionPlugin;

impl Plugin for SelectionPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                handle_unit_selection,
                update_selected_visual,
                animate_selection_rings,
                animate_destination_rings,
                update_path_visualizations,
            ),
        );
    }
}
