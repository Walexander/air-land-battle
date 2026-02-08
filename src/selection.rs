use bevy::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::mesh::{Indices, PrimitiveTopology};
use bevy_mod_outline::OutlineVolume;

use crate::map::{axial_to_world_pos, HexMapConfig, HoveredHex, Obstacles};
use crate::units::{find_path, Occupancy, OccupancyIntent, ClaimedCellsThisFrame, Unit, UnitMovement, Army, UnitStats};
use crate::loading::LoadingState;

// Components
#[derive(Component)]
pub struct Selected;

#[derive(Component)]
pub struct InnerQuarterCircle;

#[derive(Component)]
pub struct SelectionRing {
    pub unit_entity: Entity,
    pub animation_timer: f32,
    pub bounce_count: u32,
}

#[derive(Component)]
pub struct PathVisualization {
    pub unit_entity: Entity,
    pub animation_progress: f32,
    pub loop_count: u32,
    pub cached_path: Vec<(i32, i32)>,
    pub cached_target_pos: Option<Vec3>,
    pub last_mesh_update: f32, // Track when we last updated the mesh
}

#[derive(Component)]
pub struct DestinationRing {
    pub unit_entity: Entity,
    pub animation_timer: f32,
    pub bounce_count: u32,
}

#[derive(Component)]
pub struct TargetRing {
    pub unit_entity: Entity,
    pub target_entity: Entity,
}

#[derive(Component)]
pub struct HoverRing {
    pub hovered_entity: Entity,
}

// Mesh creation functions
pub fn create_selection_ring_mesh(inner_radius: f32, outer_radius: f32) -> Mesh {
    create_ring_mesh_with_segments(inner_radius, outer_radius, 32)
}

pub fn create_ring_arc_mesh(inner_radius: f32, outer_radius: f32, start_angle: f32, end_angle: f32, segments: u32) -> Mesh {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    for i in 0..=segments {
        let t = i as f32 / segments as f32;
        let angle = start_angle + (end_angle - start_angle) * t;
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
        let outer_current = i * 2 ;
        let inner_current = i * 2 + 1 ;
        let outer_next = (i + 1) * 2 ;
        let inner_next = (i + 1) * 2 + 1 ;

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

pub fn create_ring_mesh_with_segments(inner_radius: f32, outer_radius: f32, segments: u32) -> Mesh {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

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
        let outer_current = i * 2 ;
        let inner_current = i * 2 + 1 ;
        let outer_next = ((i * 2 + 2) % (segments * 2));
        let inner_next = ((i * 2 + 3) % (segments * 2));

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

fn create_filled_hexagon_mesh(radius: f32) -> Mesh {
    // Create a simple filled hexagon with custom radius
    let center = ([0.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.5, 0.5]);

    let x = |i: f32| radius * (i * 2.0 * std::f32::consts::PI / 6.0).cos();
    let z = |i: f32| radius * (i * 2.0 * std::f32::consts::PI / 6.0).sin();

    let spike0 = ([x(0.0), 0.0, z(0.0)], [0.0, 1.0, 0.0], [1.0, 0.5]);
    let spike1 = ([x(1.0), 0.0, z(1.0)], [0.0, 1.0, 0.0], [0.75, 1.0]);
    let spike2 = ([x(2.0), 0.0, z(2.0)], [0.0, 1.0, 0.0], [0.25, 1.0]);
    let spike3 = ([x(3.0), 0.0, z(3.0)], [0.0, 1.0, 0.0], [0.0, 0.5]);
    let spike4 = ([x(4.0), 0.0, z(4.0)], [0.0, 1.0, 0.0], [0.25, 0.0]);
    let spike5 = ([x(5.0), 0.0, z(5.0)], [0.0, 1.0, 0.0], [0.75, 0.0]);

    let vertices = [center, spike0, spike1, spike2, spike3, spike4, spike5];
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();

    for (position, normal, uv) in vertices.iter() {
        positions.push(*position);
        normals.push(*normal);
        uvs.push(*uv);
    }

    let indices = Indices::U32(vec![
        0, 1, 2,
        0, 2, 3,
        0, 3, 4,
        0, 4, 5,
        0, 5, 6,
        0, 6, 1
    ]);

    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
        .with_inserted_indices(indices)
}

pub fn create_hexagon_outline_mesh(radius: f32, line_width: f32) -> Mesh {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    let y = 0.0;

    // Create 6 edges around the hexagon
    for i in 0..6 {
        let angle1 = (i as f32) * std::f32::consts::PI / 3.0;
        let angle2 = ((i + 1) % 6) as f32 * std::f32::consts::PI / 3.0;

        let x1 = radius * angle1.cos();
        let z1 = radius * angle1.sin();
        let x2 = radius * angle2.cos();
        let z2 = radius * angle2.sin();

        // Direction along the edge
        let dx = x2 - x1;
        let dz = z2 - z1;
        let edge_len = (dx * dx + dz * dz).sqrt();
        let edge_dir_x = dx / edge_len;
        let edge_dir_z = dz / edge_len;

        // Perpendicular direction (outward from center)
        let perp_x = -edge_dir_z;
        let perp_z = edge_dir_x;

        let base_idx = positions.len() as u32;

        // Create 4 vertices for this edge segment
        positions.push([x1 + perp_x * line_width, y, z1 + perp_z * line_width]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.0, 0.0]);

        positions.push([x1 - perp_x * line_width, y, z1 - perp_z * line_width]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.0, 1.0]);

        positions.push([x2 + perp_x * line_width, y, z2 + perp_z * line_width]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([1.0, 0.0]);

        positions.push([x2 - perp_x * line_width, y, z2 - perp_z * line_width]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([1.0, 1.0]);

        // Two triangles for the quad
        indices.push(base_idx);
        indices.push(base_idx + 1);
        indices.push(base_idx + 2);

        indices.push(base_idx + 1);
        indices.push(base_idx + 3);
        indices.push(base_idx + 2);
    }

    // Add circles at each corner vertex to connect the edges
    let circle_segments = 8;
    for i in 0..6 {
        let angle = (i as f32) * std::f32::consts::PI / 3.0;
        let corner_x = radius * angle.cos();
        let corner_z = radius * angle.sin();

        let center_idx = positions.len() as u32;

        // Center vertex at the corner
        positions.push([corner_x, y, corner_z]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.5, 0.5]);

        // Create circle vertices around the corner
        for j in 0..circle_segments {
            let circle_angle = (j as f32 / circle_segments as f32) * 2.0 * std::f32::consts::PI;
            let offset_x = line_width * circle_angle.cos();
            let offset_z = line_width * circle_angle.sin();

            positions.push([corner_x + offset_x, y, corner_z + offset_z]);
            normals.push([0.0, 1.0, 0.0]);
            uvs.push([0.0, 0.0]);
        }

        // Create triangles for the circle
        for j in 0..circle_segments {
            let next_j = (j + 1) % circle_segments;
            indices.push(center_idx);
            indices.push(center_idx + 1 + j);
            indices.push(center_idx + 1 + next_j);
        }
    }

    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
        .with_inserted_indices(Indices::U32(indices))
}

pub fn spawn_destination_ring(
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    unit_entity: Entity,
    destination: (i32, i32),
    _army: Army,
) {
    let dest_pos = axial_to_world_pos(destination.0, destination.1);
    let hex_pos = dest_pos + Vec3::new(0.0, 2.5, 0.0);

    let color = Color::srgba(1.0, 1.0, 1.0, 0.5); // White with 50% opacity
    let border_color = Color::srgb(0.0, 0.0, 0.0); // Black

    // Use a filled hexagon
    let hex_mesh = meshes.add(create_filled_hexagon_mesh(63.0));
    let hex_material = materials.add(StandardMaterial {
        base_color: color,
        emissive: LinearRgba::new(1.0, 1.0, 1.0, 0.5),
        unlit: true,
        double_sided: true,
        cull_mode: None,
        alpha_mode: AlphaMode::Blend,
        ..default()
    });

    // Create black border outline (slightly larger)
    let border_mesh = meshes.add(create_hexagon_outline_mesh(63.0, 4.0));
    let border_material = materials.add(StandardMaterial {
        base_color: border_color,
        emissive: border_color.into(),
        unlit: true,
        double_sided: true,
        cull_mode: None,
        ..default()
    });

    let hex_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

    // Spawn the main white hexagon with border as child
    commands.spawn((
        Mesh3d(hex_mesh),
        MeshMaterial3d(hex_material),
        Transform::from_translation(hex_pos)
            .with_rotation(hex_rotation)
            .with_scale(Vec3::splat(0.65)), // Resting state at 0.65x scale
        DestinationRing {
            unit_entity,
            animation_timer: 0.0,
            bounce_count: 0,
        },
    )).with_children(|parent| {
        // Add border slightly below the filled hexagon
        parent.spawn((
            Mesh3d(border_mesh),
            MeshMaterial3d(border_material),
            Transform::from_translation(Vec3::new(0.0, -0.1, 0.0)),
        ));
    });
}

fn create_path_line_mesh(
    waypoints: &[(i32, i32)],
    current_pos: Vec3,
    animation_progress: f32,
    army: Army,
    target_pos: Option<Vec3>,
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
    let dark_color = match army {
        Army::Red => [0.9, 0.2, 0.2, 1.0],
        Army::Blue => [0.2, 0.4, 0.9, 1.0],
    };
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
                curr_pos -= direction_to_dest * destination_ring_outer_radius;
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

    // Draw line to target if targeting
    if let Some(target_position) = target_pos {
        let target_vec = Vec3::new(target_position.x, line_height, target_position.z);
        let segment_length = prev_pos.distance(target_vec);
        let direction = (target_vec - prev_pos).normalize();
        let perpendicular = Vec3::new(-direction.z, 0.0, direction.x) * line_width * 0.5;

        let num_subdivisions = (segment_length / subdivision_length).ceil() as i32;
        let num_subdivisions = num_subdivisions.max(1);

        for i in 0..num_subdivisions {
            let t_start = i as f32 / num_subdivisions as f32;
            let t_end = (i + 1) as f32 / num_subdivisions as f32;

            let sub_start = prev_pos.lerp(target_vec, t_start);
            let sub_end = prev_pos.lerp(target_vec, t_end);

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

        // Add ending circle at target
        let circle_segments = 8;
        let circle_radius = line_width * 0.5;
        let center_idx = positions.len() as u32;

        positions.push([target_vec.x, target_vec.y, target_vec.z]);
        colors.push(white_color);

        for i in 0..circle_segments {
            let angle = (i as f32 / circle_segments as f32) * std::f32::consts::PI * 2.0;
            let x_offset = circle_radius * angle.cos();
            let z_offset = circle_radius * angle.sin();

            positions.push([
                target_vec.x + x_offset,
                target_vec.y,
                target_vec.z + z_offset,
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
    clicked_unit: Res<crate::units::ClickedUnit>,
    hovered_hex: Res<HoveredHex>,
    config: Res<HexMapConfig>,
    obstacles: Res<Obstacles>,
    occupancy: Res<Occupancy>,
    occupancy_intent: Res<OccupancyIntent>,
    mut claimed_cells: ResMut<ClaimedCellsThisFrame>,
    unit_query: Query<(Entity, &Unit, Option<&UnitMovement>), Without<Selected>>,
    selected_query: Query<(Entity, &Unit, &UnitStats, Option<&UnitMovement>, &Transform), With<Selected>>,
    path_viz_query: Query<(Entity, &PathVisualization)>,
    dest_ring_query: Query<(Entity, &DestinationRing)>,
    ui_clicked: Res<crate::ui::UIClicked>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut commands: Commands,
) {
    if mouse_button.just_pressed(MouseButton::Left) {
        // Don't process game clicks if clicking on UI
        if ui_clicked.0 {
            return;
        }

        // Check if a unit was clicked directly (prioritize direct clicks)
        if let Some(clicked_entity) = clicked_unit.entity {
            // Check if the clicked unit is from the Red army (player controlled)
            if let Ok((_entity, unit, _)) = unit_query.get(clicked_entity)
                && unit.army == Army::Red {
                    // Select this unit
                    for (entity, _, _, _, _) in &selected_query {
                        commands.entity(entity).remove::<Selected>();
                    }
                    commands.entity(clicked_entity).insert(Selected);
                    return;
                }

            // If clicked unit is an enemy and we have a unit selected, target it
            if let Ok((_, enemy_unit, _)) = unit_query.get(clicked_entity)
                && enemy_unit.army != Army::Red {
                    if let Ok((selected_entity, selected_unit, stats, existing_movement, _)) = selected_query.single() {
                        let enemy_pos = (enemy_unit.q, enemy_unit.r);

                        // Remove old path visualizations
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

                        // Handle based on whether unit is currently moving
                        if let Some(movement) = existing_movement {
                            if movement.current_waypoint < movement.path.len() {
                                // Unit is actively moving
                                let current_cell = (selected_unit.q, selected_unit.r);
                                let next_cell = movement.path[movement.current_waypoint];

                                // Build blocking cells
                                let mut blocking_cells = obstacles.positions.clone();
                                for &occupied_pos in &occupancy.positions {
                                    if occupied_pos != current_cell && occupied_pos != next_cell {
                                        blocking_cells.insert(occupied_pos);
                                    }
                                }
                                for (entity, &intent_pos) in &occupancy_intent.intentions {
                                    if *entity != selected_entity {
                                        blocking_cells.insert(intent_pos);
                                    }
                                }

                                // Find path from both current and next cell
                                let goal_from_current = crate::units::find_closest_adjacent_cell(enemy_pos, current_cell, &blocking_cells);
                                let goal_from_next = crate::units::find_closest_adjacent_cell(enemy_pos, next_cell, &blocking_cells);

                                let path_from_current = goal_from_current.and_then(|goal|
                                    crate::units::find_path(current_cell, goal, config.map_radius, &blocking_cells)
                                );
                                let path_from_next = goal_from_next.and_then(|goal|
                                    crate::units::find_path(next_cell, goal, config.map_radius, &blocking_cells)
                                );

                                let should_reverse = match (&path_from_current, &path_from_next) {
                                    (Some(p1), Some(p2)) => p1.len() < p2.len(),
                                    (Some(_), None) => true,
                                    (None, Some(_)) => false,
                                    (None, None) => false,
                                };

                                if should_reverse {
                                    // Reverse and path from current cell
                                    if let (Some(goal), Some(path)) = (goal_from_current, path_from_current) {
                                        // Check if already adjacent
                                        if goal == current_cell {
                                            // Already adjacent, just set targeting without new movement
                                            commands.entity(selected_entity).insert(crate::units::Targeting {
                                                target_entity: clicked_entity,
                                                target_last_position: enemy_pos,
                                                repathing_cooldown: 0.5,
                                                last_repath_time: 0.0,
                                            });
                                        } else {
                                            // Build path including current_cell as first element to maintain visual position
                                            let mut new_path = vec![current_cell];
                                            if path.len() > 1 {
                                                new_path.extend_from_slice(&path[1..]);
                                            }

                                            if new_path.len() > 1 {
                                                let unit_position = if movement.progress >= 0.5 { next_cell } else { current_cell };

                                                commands.entity(selected_entity).insert((
                                                    Unit {
                                                        q: unit_position.0,
                                                        r: unit_position.1,
                                                        _sprite_index: selected_unit._sprite_index,
                                                        army: selected_unit.army,
                                                    },
                                                    UnitMovement {
                                                        path: new_path,
                                                        current_waypoint: 0,
                                                        progress: 1.0 - movement.progress,
                                                        speed: stats.speed,
                                                        segment_start: next_cell,
                                                    },
                                                    crate::units::Targeting {
                                                        target_entity: clicked_entity,
                                                        target_last_position: enemy_pos,
                                                        repathing_cooldown: 0.5,
                                                        last_repath_time: 0.0,
                                                    },
                                                ));
                                            }
                                        }
                                    }
                                } else {
                                    // Continue forward from next cell
                                    if let (Some(goal), Some(path)) = (goal_from_next, path_from_next) {
                                        // Check if already adjacent (or will be adjacent at next_cell)
                                        if goal == next_cell {
                                            // Will be adjacent when we reach next_cell, just set targeting
                                            commands.entity(selected_entity).insert(crate::units::Targeting {
                                                target_entity: clicked_entity,
                                                target_last_position: enemy_pos,
                                                repathing_cooldown: 0.5,
                                                last_repath_time: 0.0,
                                            });
                                        } else {
                                            // Build path including next_cell as first element to maintain visual position
                                            let mut new_full_path = vec![next_cell];
                                            if path.len() > 1 {
                                                new_full_path.extend_from_slice(&path[1..]);
                                            }

                                            if new_full_path.len() > 1 {
                                                commands.entity(selected_entity).insert((
                                                    UnitMovement {
                                                        path: new_full_path,
                                                        current_waypoint: 0,
                                                        progress: movement.progress,
                                                        speed: stats.speed,
                                                        segment_start: movement.segment_start,
                                                    },
                                                    crate::units::Targeting {
                                                        target_entity: clicked_entity,
                                                        target_last_position: enemy_pos,
                                                        repathing_cooldown: 0.5,
                                                        last_repath_time: 0.0,
                                                    },
                                                ));
                                            }
                                        }
                                    }
                                }
                            } else {
                                // Path complete, treat as no movement
                                let start_pos = (selected_unit.q, selected_unit.r);
                                let mut blocking_cells = obstacles.positions.clone();
                                for &occupied_pos in &occupancy.positions {
                                    if occupied_pos != start_pos {
                                        blocking_cells.insert(occupied_pos);
                                    }
                                }
                                for (entity, &intent_pos) in &occupancy_intent.intentions {
                                    if *entity != selected_entity {
                                        blocking_cells.insert(intent_pos);
                                    }
                                }

                                if let Some(goal) = crate::units::find_closest_adjacent_cell(enemy_pos, start_pos, &blocking_cells) {
                                    // Check if already adjacent (goal is current position)
                                    if goal == start_pos {
                                        // Already adjacent, just set targeting without movement
                                        commands.entity(selected_entity).insert(crate::units::Targeting {
                                            target_entity: clicked_entity,
                                            target_last_position: enemy_pos,
                                            repathing_cooldown: 0.5,
                                            last_repath_time: 0.0,
                                        });
                                    } else if let Some(path) = crate::units::find_path(start_pos, goal, config.map_radius, &blocking_cells) {
                                        let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                                            path[1..].to_vec()
                                        } else {
                                            vec![]
                                        };

                                        if !path_to_follow.is_empty() {
                                            commands.entity(selected_entity).insert((
                                                UnitMovement {
                                                    path: path_to_follow,
                                                    current_waypoint: 0,
                                                    progress: 0.0,
                                                    speed: stats.speed,
                                                    segment_start: start_pos,
                                                },
                                                crate::units::Targeting {
                                                    target_entity: clicked_entity,
                                                    target_last_position: enemy_pos,
                                                    repathing_cooldown: 0.5,
                                                    last_repath_time: 0.0,
                                                },
                                            ));
                                        }
                                    }
                                }
                            }
                        } else {
                            // No existing movement
                            let start_pos = (selected_unit.q, selected_unit.r);
                            let mut blocking_cells = obstacles.positions.clone();
                            for &occupied_pos in &occupancy.positions {
                                if occupied_pos != start_pos {
                                    blocking_cells.insert(occupied_pos);
                                }
                            }
                            for (entity, &intent_pos) in &occupancy_intent.intentions {
                                if *entity != selected_entity {
                                    blocking_cells.insert(intent_pos);
                                }
                            }

                            if let Some(goal) = crate::units::find_closest_adjacent_cell(enemy_pos, start_pos, &blocking_cells) {
                                // Check if already adjacent (goal is current position)
                                if goal == start_pos {
                                    // Already adjacent, just set targeting without movement
                                    commands.entity(selected_entity).insert(crate::units::Targeting {
                                        target_entity: clicked_entity,
                                        target_last_position: enemy_pos,
                                        repathing_cooldown: 0.5,
                                        last_repath_time: 0.0,
                                    });
                                } else if let Some(path) = crate::units::find_path(start_pos, goal, config.map_radius, &blocking_cells) {
                                    let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                                        path[1..].to_vec()
                                    } else {
                                        vec![]
                                    };

                                    if !path_to_follow.is_empty() {
                                        commands.entity(selected_entity).insert((
                                            UnitMovement {
                                                path: path_to_follow,
                                                current_waypoint: 0,
                                                progress: 0.0,
                                                speed: stats.speed,
                                                segment_start: start_pos,
                                            },
                                            crate::units::Targeting {
                                                target_entity: clicked_entity,
                                                target_last_position: enemy_pos,
                                                repathing_cooldown: 0.5,
                                                last_repath_time: 0.0,
                                            },
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    return;
                }
        }

        // Handle movement to empty cells (only if no unit was clicked directly via hitbox)
        if clicked_unit.entity.is_none()
            && hovered_hex.entity.is_some()
                && let Ok((selected_entity, selected_unit, stats, existing_movement, _unit_transform)) =
                    selected_query.single()
                {
                    // Remove targeting when issuing normal movement command
                    commands.entity(selected_entity).remove::<crate::units::Targeting>();

                    let goal = (hovered_hex.q, hovered_hex.r);

                        if obstacles.positions.contains(&goal) {
                            return;
                        }

                        // Check if goal is occupied by ANY unit
                        if occupancy.positions.contains(&goal) {
                            return; // Cannot move to occupied cell
                        }

                        // Check if another unit is already moving to this goal
                        for (entity, &intent_pos) in &occupancy_intent.intentions {
                            if *entity != selected_entity && intent_pos == goal {
                                return; // Cannot move to cell already targeted by another unit
                            }
                        }

                        // CRITICAL: Check if goal has been claimed by any system THIS FRAME
                        if claimed_cells.cells.contains(&goal) {
                            return; // Cannot move to cell claimed this frame
                        }

                        let mut blocking_cells = obstacles.positions.clone();
                        let unit_current_pos = (selected_unit.q, selected_unit.r);
                        for &occupied_pos in &occupancy.positions {
                            if occupied_pos != unit_current_pos {
                                blocking_cells.insert(occupied_pos);
                            }
                        }
                        for (entity, &intent_pos) in &occupancy_intent.intentions {
                            if *entity != selected_entity && intent_pos != unit_current_pos {
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

                            // Check if current_waypoint is within bounds
                            if movement.current_waypoint >= movement.path.len() {
                                // Path is complete, treat as if unit has no movement
                                if let Some(path) =
                                    find_path(current_cell, goal, config.map_radius, &blocking_cells)
                                {
                                    let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                                        path[1..].to_vec()
                                    } else {
                                        vec![]
                                    };

                                    if !path_to_follow.is_empty() {
                                        println!(
                                            "User-commanded unit moving from ({}, {}) to destination ({}, {})",
                                            current_cell.0, current_cell.1, goal.0, goal.1
                                        );
                                        commands.entity(selected_entity).insert(UnitMovement {
                                            path: path_to_follow.clone(),
                                            current_waypoint: 0,
                                            progress: 0.0,
                                            speed: stats.speed,
                                            segment_start: current_cell,
                                        });

                                        // Mark all cells in path as claimed
                                        for &cell in &path_to_follow {
                                            claimed_cells.cells.insert(cell);
                                        }

                                        spawn_destination_ring(
                                            &mut commands,
                                            &mut meshes,
                                            &mut materials,
                                            selected_entity,
                                            goal,
                                            selected_unit.army,
                                        );
                                    }
                                }
                            } else {
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

                                        // Only update unit position if we've already claimed next_cell (>= 0.5 progress)
                                        let unit_position = if movement.progress >= 0.5 {
                                            next_cell
                                        } else {
                                            current_cell
                                        };

                                        commands.entity(selected_entity).insert((
                                            Unit {
                                                q: unit_position.0,
                                                r: unit_position.1,
                                                _sprite_index: selected_unit._sprite_index,
                                                army: selected_unit.army,
                                            },
                                            UnitMovement {
                                                path: new_path.clone(),
                                                current_waypoint: 0,
                                                progress: 1.0 - movement.progress,
                                                speed: stats.speed,
                                                segment_start: next_cell,
                                            },
                                        ));

                                        // Mark all cells in path as claimed
                                        for &cell in &new_path {
                                            claimed_cells.cells.insert(cell);
                                        }

                                        spawn_destination_ring(
                                            &mut commands,
                                            &mut meshes,
                                            &mut materials,
                                            selected_entity,
                                            goal,
                                            selected_unit.army,
                                        );
                                    }
                                } else if let Some(path) =
                                    find_path(next_cell, goal, config.map_radius, &blocking_cells)
                                {
                                    let mut new_full_path = vec![next_cell];
                                    if path.len() > 1 {
                                        new_full_path.extend_from_slice(&path[1..]);
                                    }

                                    if new_full_path.len() > 1 {
                                        // Keep current segment_start to maintain visual position
                                        commands.entity(selected_entity).insert(UnitMovement {
                                            path: new_full_path.clone(),
                                            current_waypoint: 0,
                                            progress: movement.progress,
                                            speed: stats.speed,
                                            segment_start: movement.segment_start,
                                        });

                                        // Mark all cells in path as claimed
                                        for &cell in &new_full_path {
                                            claimed_cells.cells.insert(cell);
                                        }

                                        spawn_destination_ring(
                                            &mut commands,
                                            &mut meshes,
                                            &mut materials,
                                            selected_entity,
                                            goal,
                                            selected_unit.army,
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
                                        path: path_to_follow.clone(),
                                        current_waypoint: 0,
                                        progress: 0.0,
                                        speed: stats.speed,
                                        segment_start: start,
                                    });

                                    // Mark all cells in path as claimed
                                    for &cell in &path_to_follow {
                                        claimed_cells.cells.insert(cell);
                                    }

                                    spawn_destination_ring(
                                        &mut commands,
                                        &mut meshes,
                                        &mut materials,
                                        selected_entity,
                                        goal,
                                        selected_unit.army,
                                    );
                                }
                            }
                        }
                    }
    }
}

fn update_selected_visual(
    mut commands: Commands,
    selected_query: Query<(Entity, Option<&Children>), (With<Unit>, With<Selected>)>,
    unselected_query: Query<(Entity, Option<&Children>), (With<Unit>, Without<Selected>)>,
    children_query: Query<&Children>,
    mesh_query: Query<Entity, With<Mesh3d>>,
    outline_query: Query<Entity, With<OutlineVolume>>,
) {
    // Add outlines to newly selected units' mesh children (if they don't already have them)
    for (_unit_entity, children_opt) in &selected_query {
        if let Some(children) = children_opt
            && !has_outline_in_children(children, &children_query, &outline_query) {
                add_outline_to_children(children, &children_query, &mesh_query, &mut commands);
            }
    }

    // Remove outlines from unselected units' mesh children
    for (_unit_entity, children_opt) in &unselected_query {
        if let Some(children) = children_opt {
            remove_outline_from_children(children, &children_query, &mesh_query, &mut commands);
        }
    }
}

fn has_outline_in_children(
    children: &Children,
    children_query: &Query<&Children>,
    outline_query: &Query<Entity, With<OutlineVolume>>,
) -> bool {
    for child in children.iter() {
        if outline_query.contains(child) {
            return true;
        }

        // Check grandchildren recursively
        if let Ok(grandchildren) = children_query.get(child)
            && has_outline_in_children(grandchildren, children_query, outline_query) {
                return true;
            }
    }
    false
}

fn add_outline_to_children(
    children: &Children,
    children_query: &Query<&Children>,
    mesh_query: &Query<Entity, With<Mesh3d>>,
    commands: &mut Commands,
) {
    for child in children.iter() {
        // If this child has a mesh, try to add outline (may fail if entity was despawned)
        if mesh_query.contains(child)
            && let Ok(mut entity_commands) = commands.get_entity(child) {
                entity_commands.insert(OutlineVolume {
                    visible: true,
                    width: 2.0,
                    colour: Color::srgb(1.0, 1.0, 1.0), // White
                });
            }

        // Recursively process grandchildren
        if let Ok(grandchildren) = children_query.get(child) {
            add_outline_to_children(grandchildren, children_query, mesh_query, commands);
        }
    }
}

fn remove_outline_from_children(
    children: &Children,
    children_query: &Query<&Children>,
    mesh_query: &Query<Entity, With<Mesh3d>>,
    commands: &mut Commands,
) {
    for child in children.iter() {
        if mesh_query.contains(child)
            && let Ok(mut entity_commands) = commands.get_entity(child) {
                entity_commands.remove::<OutlineVolume>();
            }

        if let Ok(grandchildren) = children_query.get(child) {
            remove_outline_from_children(grandchildren, children_query, mesh_query, commands);
        }
    }
}

fn animate_selection_rings(
    time: Res<Time>,
    mut meshes: ResMut<Assets<Mesh>>,
    unit_query: Query<(Entity, &Transform, Has<Selected>), With<Unit>>,
    mut ring_query: Query<
        (&mut SelectionRing, &mut Transform, &mut Visibility, &mut Mesh3d),
        Without<Unit>,
    >,
) {
    for (mut ring, mut ring_transform, mut visibility, mut mesh_handle) in &mut ring_query {
        if let Ok((_, unit_transform, is_selected)) = unit_query.get(ring.unit_entity) {
            let was_visible = *visibility == Visibility::Visible;
            *visibility = if is_selected {
                Visibility::Visible
            } else {
                Visibility::Hidden
            };

            if is_selected && !was_visible {
                ring.animation_timer = 0.0;
                ring.bounce_count = 0;
            }

            ring_transform.translation.x = unit_transform.translation.x;
            ring_transform.translation.y = unit_transform.translation.y + 1.0;
            ring_transform.translation.z = unit_transform.translation.z;

            if is_selected {
                let max_bounces = 1; // Just one animation, no bouncing

                if ring.bounce_count < max_bounces {
                    ring.animation_timer += time.delta_secs();

                    let bounce_duration = 0.8;  // Slower animation (was 0.5)
                    let cycle_progress = (ring.animation_timer / bounce_duration) % 1.0;

                    let current_bounce = (ring.animation_timer / bounce_duration).floor() as u32;
                    if current_bounce > ring.bounce_count {
                        ring.bounce_count = current_bounce;
                    }

                    if ring.bounce_count < max_bounces {
                        let min_scale = 0.5;
                        let max_scale = 0.9;  // 10% smaller starting size (was 1.0)

                        // Calculate line width (lerp from thick to thin)
                        let min_line_width = 10.0;  // Final line width
                        let max_line_width = 16.0; // Initial line width

                        let (current_scale, current_line_width) = if cycle_progress < 0.25 {
                            // Hold at max scale and max line width for first quarter of animation
                            (max_scale, max_line_width)
                        } else {
                            // Spring bounce effect: oscillate around target with decreasing amplitude
                            let ease_progress = (cycle_progress - 0.25) / 0.75; // Map 0.25-1.0 to 0.0-1.0

                            // Spring parameters
                            let bounce_frequency = 3.0; // How many bounces/oscillations
                            let damping = 2.5; // How quickly oscillations decay (lower = more bounce)

                            // Create damped oscillation around target (1.0)
                            // The cosine makes it start at 1.0, dip below, bounce back above, etc.
                            let spring = (-damping * ease_progress).exp() *
                                        (bounce_frequency * std::f32::consts::PI * ease_progress).cos();

                            // Map spring oscillation to scale range
                            // When spring = 1.0 (start), we want max_scale
                            // When spring = 0.0 (settled), we want min_scale
                            let scale = min_scale + (max_scale - min_scale) * spring;
                            let line_width = min_line_width + (max_line_width - min_line_width) * spring;

                            (scale, line_width)
                        };

                        ring_transform.scale = Vec3::splat(current_scale);

                        // Update mesh with new line width
                        let outer_radius = 100.0;
                        let inner_radius = outer_radius - current_line_width;
                        let new_mesh = create_selection_ring_mesh(inner_radius, outer_radius);
                        mesh_handle.0 = meshes.add(new_mesh);
                    } else {
                        ring_transform.scale = Vec3::splat(0.5);
                        // Set final mesh with 10-unit line width
                        let new_mesh = create_selection_ring_mesh(90.0, 100.0);
                        mesh_handle.0 = meshes.add(new_mesh);
                    }
                } else {
                    ring_transform.scale = Vec3::splat(0.5);
                }
            }
        }
    }
}

fn animate_destination_rings(
    time: Res<Time>,
    unit_query: Query<(Entity, &UnitMovement, Has<Selected>)>,
    mut ring_query: Query<(Entity, &mut DestinationRing, &mut Transform)>,
    mut commands: Commands,
) {
    let selected_moving_units: std::collections::HashSet<Entity> =
        unit_query.iter()
            .filter(|(_, _, is_selected)| *is_selected)
            .map(|(e, _, _)| e)
            .collect();

    let mut to_despawn = Vec::new();

    for (entity, mut ring, mut ring_transform) in &mut ring_query {
        if !selected_moving_units.contains(&ring.unit_entity) {
            to_despawn.push(entity);
            continue;
        }

        ring.animation_timer += time.delta_secs();

        let max_bounces = 1; // Single animation cycle

        if ring.bounce_count < max_bounces {
            let bounce_duration = 0.5; // Animation takes 0.5 seconds
            let cycle_progress = (ring.animation_timer / bounce_duration) % 1.0;

            // Check if we completed the animation
            let current_bounce = (ring.animation_timer / bounce_duration).floor() as u32;
            if current_bounce > ring.bounce_count {
                ring.bounce_count = current_bounce;
            }

            if ring.bounce_count < max_bounces {
                // Scale parameters
                let min_scale = 0.65;
                let max_scale = 1.0;

                // Hold at max scale for first half, then ease down
                let current_scale = if cycle_progress < 0.5 {
                    // Hold at max for first half
                    max_scale
                } else {
                    // Ease down in second half using cosine for smooth deceleration
                    let ease_progress = (cycle_progress - 0.5) * 2.0; // Map 0.5-1.0 to 0.0-1.0
                    let ease = (ease_progress * std::f32::consts::PI / 2.0).cos();
                    max_scale - (max_scale - min_scale) * (1.0 - ease)
                };

                // Update transform scale
                ring_transform.scale = Vec3::splat(current_scale);
            }
        } else {
            // Continuous pulsing animation in resting state
            let pulse_duration = 2.0; // Slow pulse over 2 seconds
            let pulse_progress = (ring.animation_timer / pulse_duration) % 1.0;

            // Subtle scale variation using sine wave
            let base_scale = 0.65;
            let pulse_amplitude = 0.03; // Subtle +/- 3% variation
            let pulse = (pulse_progress * 2.0 * std::f32::consts::PI).sin();
            let current_scale = base_scale + pulse * pulse_amplitude;

            ring_transform.scale = Vec3::splat(current_scale);
        }
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
    unit_query: Query<(Entity, &Unit, &Transform, Option<&UnitMovement>, Option<&crate::units::Targeting>, Has<Selected>)>,
    target_transform_query: Query<&Transform, (With<Unit>, Without<PathVisualization>)>,
    mut path_viz_query: Query<(Entity, &mut PathVisualization, &mut Mesh3d)>,
) {
    let mut selected_units_with_movement = std::collections::HashSet::new();
    for (unit_entity, _, _, movement, _, is_selected) in &unit_query {
        if movement.is_some() && is_selected {
            selected_units_with_movement.insert(unit_entity);
        }
    }

    for (viz_entity, path_viz, _) in &path_viz_query {
        if !selected_units_with_movement.contains(&path_viz.unit_entity) {
            commands.entity(viz_entity).despawn();
        }
    }

    for (unit_entity, unit, transform, movement, targeting, is_selected) in &unit_query {
        if let Some(movement) = movement {
            if !is_selected {
                continue;
            }
            let remaining_path = &movement.path[movement.current_waypoint..];

            // Get target position if unit is targeting
            let target_pos = if let Some(targeting) = targeting {
                if let Ok(target_transform) = target_transform_query.get(targeting.target_entity) {
                    Some(target_transform.translation)
                } else {
                    None
                }
            } else {
                None
            };

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

                    // Update animation progress
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

                    // Check if we need to regenerate the mesh
                    let path_changed = path_viz.cached_path != remaining_path;
                    let target_changed = path_viz.cached_target_pos != target_pos;
                    let time_for_animation_update = time.elapsed_secs() - path_viz.last_mesh_update >= 0.05;

                    // Always update when there's only one waypoint left so the line starts from the unit's current position
                    let is_last_waypoint = remaining_path.len() == 1;

                    let needs_update = path_changed || target_changed || (time_for_animation_update && path_viz.loop_count < 2) || is_last_waypoint;

                    if needs_update {
                        let new_mesh = create_path_line_mesh(
                            remaining_path,
                            transform.translation,
                            animation_progress,
                            unit.army,
                            target_pos,
                        );
                        mesh_handle.0 = meshes.add(new_mesh);

                        // Update cache
                        path_viz.cached_path = remaining_path.to_vec();
                        path_viz.cached_target_pos = target_pos;
                        path_viz.last_mesh_update = time.elapsed_secs();
                    }

                    break;
                }
            }

            if !found {
                let path_mesh =
                    create_path_line_mesh(remaining_path, transform.translation, 0.0, unit.army, target_pos);

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
                        cached_path: remaining_path.to_vec(),
                        cached_target_pos: target_pos,
                        last_mesh_update: time.elapsed_secs(),
                    },
                ));
            }
        }
    }
}

fn visualize_targeting(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    selected_query: Query<(Entity, &crate::units::Targeting), With<Selected>>,
    target_query: Query<&Transform, With<Unit>>,
    existing_target_rings: Query<(Entity, &TargetRing)>,
) {
    // Track which units should have target rings
    let mut active_targeting: std::collections::HashSet<(Entity, Entity)> = std::collections::HashSet::new();

    for (unit_entity, targeting) in &selected_query {
        active_targeting.insert((unit_entity, targeting.target_entity));
    }

    // Remove rings that are no longer needed
    for (ring_entity, target_ring) in &existing_target_rings {
        let should_keep = active_targeting.contains(&(target_ring.unit_entity, target_ring.target_entity));
        if !should_keep {
            commands.entity(ring_entity).despawn();
        } else {
            // Remove from set so we don't spawn a duplicate
            active_targeting.remove(&(target_ring.unit_entity, target_ring.target_entity));
        }
    }

    // Spawn new target rings for active targeting relationships that don't have rings yet
    for (unit_entity, target_entity) in active_targeting {
        if let Ok(target_transform) = target_query.get(target_entity) {
            // Spawn red square outline on target (4-segment circle with thinner lines)
            let ring_mesh = meshes.add(create_ring_mesh_with_segments(50.0, 58.0, 4));
            let ring_material = materials.add(StandardMaterial {
                base_color: Color::srgb(0.9, 0.2, 0.2), // Red
                emissive: Color::srgb(0.9, 0.2, 0.2).into(),
                unlit: true,
                alpha_mode: AlphaMode::Blend,
                ..default()
            });

            commands.spawn((
                Mesh3d(ring_mesh),
                MeshMaterial3d(ring_material),
                Transform::from_translation(target_transform.translation + Vec3::new(0.0, 1.0, 0.0))
                    .with_scale(Vec3::splat(1.0)),
                TargetRing {
                    unit_entity,
                    target_entity,
                },
            ));
        }
    }
}

pub struct SelectionPlugin;

fn update_target_ring_positions(
    mut target_ring_query: Query<(&TargetRing, &mut Transform)>,
    unit_transform_query: Query<&Transform, (With<crate::units::Unit>, Without<TargetRing>)>,
) {
    for (target_ring, mut ring_transform) in &mut target_ring_query {
        // Update ring position to follow the target entity's actual transform
        if let Ok(target_transform) = unit_transform_query.get(target_ring.target_entity) {
            ring_transform.translation = target_transform.translation + Vec3::new(0.0, 1.0, 0.0);
        }
    }
}

fn cleanup_target_rings(
    mut commands: Commands,
    target_ring_query: Query<(Entity, &TargetRing)>,
    targeting_query: Query<&crate::units::Targeting>,
    unit_query: Query<&Unit>,
) {
    for (ring_entity, target_ring) in &target_ring_query {
        let should_remove =
            // Remove if unit no longer has targeting
            targeting_query.get(target_ring.unit_entity).is_err() ||
            // Remove if target no longer exists
            unit_query.get(target_ring.target_entity).is_err();

        if should_remove {
            commands.entity(ring_entity).despawn();
        }
    }
}

fn visualize_hover_ring(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    hovered_unit: Res<crate::units::HoveredUnit>,
    selected_query: Query<&Unit, With<Selected>>,
    unit_query: Query<(&Unit, &Transform)>,
    existing_hover_rings: Query<(Entity, &HoverRing)>,
) {
    // Check if we have a friendly unit selected
    let has_friendly_selected = selected_query.iter().any(|u| u.army == Army::Red);

    // Determine which entity should have a hover ring
    let should_have_ring = if has_friendly_selected {
        hovered_unit.entity.and_then(|entity| {
            unit_query.get(entity).ok().and_then(|(unit, _)| {
                if unit.army != Army::Red {
                    Some(entity)
                } else {
                    None
                }
            })
        })
    } else {
        None
    };

    // Remove rings that are no longer needed
    for (ring_entity, hover_ring) in &existing_hover_rings {
        if should_have_ring != Some(hover_ring.hovered_entity) {
            commands.entity(ring_entity).despawn();
        }
    }

    // Spawn new hover ring if needed and doesn't already exist
    if let Some(hovered_entity) = should_have_ring {
        let already_exists = existing_hover_rings.iter().any(|(_, ring)| ring.hovered_entity == hovered_entity);

        if !already_exists
            && let Ok((_, transform)) = unit_query.get(hovered_entity) {
                // Spawn red square outline on hovered enemy (same style as target ring)
                let ring_mesh = meshes.add(create_ring_mesh_with_segments(50.0, 58.0, 4));
                let ring_material = materials.add(StandardMaterial {
                    base_color: Color::srgb(0.9, 0.2, 0.2), // Red
                    emissive: Color::srgb(0.9, 0.2, 0.2).into(),
                    unlit: true,
                    alpha_mode: AlphaMode::Blend,
                    ..default()
                });

                commands.spawn((
                    Mesh3d(ring_mesh),
                    MeshMaterial3d(ring_material),
                    Transform::from_translation(transform.translation + Vec3::new(0.0, 1.0, 0.0))
                        .with_scale(Vec3::splat(1.0)),
                    HoverRing {
                        hovered_entity,
                    },
                ));
            }
    }
}

fn update_hover_ring_positions(
    mut hover_ring_query: Query<(&HoverRing, &mut Transform)>,
    unit_transform_query: Query<&Transform, (With<Unit>, Without<HoverRing>)>,
) {
    for (hover_ring, mut ring_transform) in &mut hover_ring_query {
        // Update ring position to follow the hovered entity's actual transform
        if let Ok(target_transform) = unit_transform_query.get(hover_ring.hovered_entity) {
            ring_transform.translation = target_transform.translation + Vec3::new(0.0, 1.0, 0.0);
        }
    }
}

fn animate_inner_quarter_circles(
    time: Res<Time>,
    mut quarter_circle_query: Query<&mut Transform, With<InnerQuarterCircle>>,
) {
    let rotation_speed = 0.5; // Radians per second (slow rotation)
    let delta_rotation = rotation_speed * time.delta_secs();

    for mut transform in &mut quarter_circle_query {
        // Rotate around Y axis
        transform.rotate_y(delta_rotation);
    }
}

impl Plugin for SelectionPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                update_selected_visual,
                animate_selection_rings,
                animate_destination_rings,
                update_path_visualizations,
                animate_inner_quarter_circles,
            ).run_if(in_state(LoadingState::Playing))
        );
        app.add_systems(
            Update,
            (
                visualize_targeting,
                update_target_ring_positions,
                cleanup_target_rings,
            ).run_if(in_state(LoadingState::Playing))
        );
        app.add_systems(
            Update,
            (
                visualize_hover_ring,
                update_hover_ring_positions,
            ).run_if(in_state(LoadingState::Playing))
        );
        app.add_systems(
            Update,
            handle_unit_selection
        );
    }
}
