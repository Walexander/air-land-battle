use bevy::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::mesh::{Indices, PrimitiveTopology};

use crate::ui::GameCamera;
use crate::launch_pads::{LaunchPads, LaunchPadOwner, LaunchPadOwnership};
use crate::selection::{create_hexagon_outline_mesh, Selected};
use crate::units::Unit;
use crate::loading::LoadingState;

// Hex grid constants
const HEX_WIDTH: f32 = 128.0;
const HEX_HEIGHT: f32 = HEX_WIDTH * 0.866025404; // width * sqrt(3)/2
const HEX_RADIUS: f32 = HEX_WIDTH / 2.0;
const OUTLINE_WIDTH: f32 = 8.0;

// Components
#[derive(Component)]
pub struct HexMap;

#[derive(Component)]
pub struct HexTile {
    pub q: i32,
    pub r: i32,
    pub _height: f32,
}

#[derive(Component)]
pub struct HexOutline {
    pub hex_entity: Entity,
}

#[derive(Component)]
pub struct ObstacleSprite;

#[derive(Component)]
pub struct LaunchPadOutline {
    pub pad_index: usize,
}

#[derive(Component)]
pub struct LaunchPadTile {
    pub pad_index: usize,
}

#[derive(Component)]
pub struct CrystalField {
    pub q: i32,
    pub r: i32,
    pub crystals_remaining: i32,
    pub max_crystals: i32,
}

#[derive(Component)]
struct CrystalMaterialApplied;

#[derive(Component)]
struct CrystalVisual {
    rotation_speed: f32,
    pulse_offset: f32,
}

// Resources
#[derive(Resource)]
pub struct HexMapConfig {
    pub map_radius: i32,
}

#[derive(Resource, Default)]
pub struct HoveredHex {
    pub entity: Option<Entity>,
    pub q: i32,
    pub r: i32,
}

#[derive(Resource, Default)]
pub struct Obstacles {
    pub positions: std::collections::HashSet<(i32, i32)>,
}

pub struct MapPlugin;

impl Plugin for MapPlugin {
    fn build(&self, app: &mut App) {
        // Set up obstacles
        let mut obstacles = Obstacles::default();
        obstacles.positions.insert((-1, 2));

        app.insert_resource(HexMapConfig { map_radius: 5 })
            .insert_resource(HoveredHex::default())
            .insert_resource(obstacles)
            .insert_resource(ClearColor(Color::srgb(0.53, 0.81, 0.92))) // Light sky blue
            .add_systems(OnEnter(LoadingState::Playing), setup_hex_map)
            .add_systems(Update, (hex_hover_system, update_outline_colors, update_launch_pad_colors, billboard_sprites, apply_crystal_materials, animate_crystal_sparkle).run_if(in_state(LoadingState::Playing)));
    }
}

pub fn axial_to_world_pos(q: i32, r: i32) -> Vec3 {
    // Pointy-top hex coordinates
    let x = HEX_HEIGHT * (q as f32 + r as f32 * 0.5);
    let z = HEX_WIDTH * 0.75 * r as f32;
    Vec3::new(x, 0.0, z)
}

fn create_hexagon_prism_mesh(height: f32) -> Mesh {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    let top_y = height;
    let base_y = 0.0;

    // Top face center vertex (index 0)
    positions.push([0.0, top_y, 0.0]);
    normals.push([0.0, 1.0, 0.0]);
    uvs.push([0.5, 0.5]);

    // Top perimeter vertices (indices 1-6)
    for i in 0..6 {
        let angle = std::f32::consts::PI / 3.0 * i as f32;
        let x = HEX_RADIUS * angle.cos();
        let z = HEX_RADIUS * angle.sin();
        positions.push([x, top_y, z]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.5 + x / HEX_WIDTH, 0.5 + z / HEX_WIDTH]);
    }

    // Top face triangle fan indices
    for i in 0..6 {
        indices.push(0);
        indices.push(1 + i);
        indices.push(1 + ((i + 1) % 6));
    }

    // SIDE FACES
    let top_start_idx = 1u32;
    let bottom_start_idx = 7u32;

    // Bottom perimeter vertices (indices 7-12)
    for i in 0..6 {
        let angle = std::f32::consts::PI / 3.0 * i as f32;
        let x = HEX_RADIUS * angle.cos();
        let z = HEX_RADIUS * angle.sin();

        positions.push([x, base_y, z]);

        let normal_angle = angle + std::f32::consts::PI / 6.0;
        let nx = normal_angle.cos();
        let nz = normal_angle.sin();
        normals.push([nx, 0.0, nz]);
        uvs.push([0.0, 0.0]);
    }

    // Create side faces
    for i in 0..6 {
        let next_i = (i + 1) % 6;

        let top_a = top_start_idx + i;
        let top_b = top_start_idx + next_i;
        let bottom_a = bottom_start_idx + i;
        let bottom_b = bottom_start_idx + next_i;

        // First triangle
        indices.push(top_a);
        indices.push(bottom_a);
        indices.push(top_b);

        // Second triangle
        indices.push(top_b);
        indices.push(bottom_a);
        indices.push(bottom_b);
    }

    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
        .with_inserted_indices(Indices::U32(indices))
}

fn create_filled_hexagon_mesh_with_radius(radius: f32) -> Mesh {
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

fn create_filled_hexagon_mesh() -> Mesh {
    // Make the colored tile 1px smaller so the border shows
    create_filled_hexagon_mesh_with_radius(HEX_RADIUS - 1.0)
}

fn create_filled_hexagon_border_mesh() -> Mesh {
    // Border uses the full hex radius
    create_filled_hexagon_mesh_with_radius(HEX_RADIUS)
}

fn create_billboard_mesh(width: f32, height: f32) -> Mesh {
    let half_width = width / 2.0;
    let half_height = height / 2.0;

    let positions = vec![
        [-half_width, half_height, 0.0],   // Top-left
        [half_width, half_height, 0.0],    // Top-right
        [half_width, -half_height, 0.0],   // Bottom-right
        [-half_width, -half_height, 0.0],  // Bottom-left
    ];

    let normals = vec![
        [0.0, 0.0, 1.0],
        [0.0, 0.0, 1.0],
        [0.0, 0.0, 1.0],
        [0.0, 0.0, 1.0],
    ];

    // UV coordinates for the sprite (x: 32, y: 0, width: 30, height: 32)
    // Texture is 128x128
    let texture_width = 128.0;
    let texture_height = 128.0;
    let u_min = 33.0 / texture_width;
    let u_max = (33.0 + 28.0) / texture_width;
    let v_min = 0.0 / texture_height;
    let v_max = 32.0 / texture_height;

    let uvs = vec![
        [u_min, v_min],  // Top-left
        [u_max, v_min],  // Top-right
        [u_max, v_max],  // Bottom-right
        [u_min, v_max],  // Bottom-left
    ];

    let indices = vec![0, 1, 2, 0, 2, 3];

    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
        .with_inserted_indices(Indices::U32(indices))
}

fn create_launch_pad_outline_mesh(perimeter_edges: &[((i32, i32), (i32, i32))], pad_center: Vec3) -> Mesh {
    // Create a mesh from the perimeter edges
    // All positions are relative to pad_center so scaling works correctly
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    let y = 0.0;
    let line_width = 4.0; // Line thickness

    // Collect all unique corner positions (in 3D world space)
    let mut corner_positions: Vec<[f32; 3]> = Vec::new();

    // For each edge, create a quad representing the shared boundary between two hexes
    for (cell1, cell2) in perimeter_edges {
        // Convert axial coords to world positions
        let pos1 = axial_to_world_pos(cell1.0, cell1.1);
        let pos2 = axial_to_world_pos(cell2.0, cell2.1);

        // The edge is at the midpoint between the two hex centers
        let midpoint = Vec3::new(
            (pos1.x + pos2.x) / 2.0,
            0.0,
            (pos1.z + pos2.z) / 2.0,
        );

        // Make positions relative to pad center
        let midpoint_relative = midpoint - pad_center;

        // Direction from cell1 to cell2
        let edge_vec = Vec3::new(pos2.x - pos1.x, 0.0, pos2.z - pos1.z);
        let edge_len = edge_vec.length();
        if edge_len < 0.001 {
            continue;
        }
        let edge_dir = edge_vec / edge_len;

        // The actual edge is perpendicular to the direction between centers
        let perp = Vec3::new(-edge_dir.z, 0.0, edge_dir.x);

        // For a pointy-top hex, the edge length is HEX_RADIUS
        let half_edge_len = HEX_RADIUS / 2.0;

        let base_idx = positions.len() as u32;

        // Store the actual geometric corners (without line_width offset in edge direction)
        // Use relative positions
        let corner1 = [
            midpoint_relative.x + perp.x * half_edge_len,
            y,
            midpoint_relative.z + perp.z * half_edge_len,
        ];
        let corner2 = [
            midpoint_relative.x - perp.x * half_edge_len,
            y,
            midpoint_relative.z - perp.z * half_edge_len,
        ];
        corner_positions.push(corner1);
        corner_positions.push(corner2);

        // Create 4 vertices for this edge segment
        // Edge extends perpendicular to the line between centers
        // Use relative positions
        let v0 = [
            midpoint_relative.x + perp.x * half_edge_len + edge_dir.x * line_width,
            y,
            midpoint_relative.z + perp.z * half_edge_len + edge_dir.z * line_width,
        ];
        let v1 = [
            midpoint_relative.x + perp.x * half_edge_len - edge_dir.x * line_width,
            y,
            midpoint_relative.z + perp.z * half_edge_len - edge_dir.z * line_width,
        ];
        let v2 = [
            midpoint_relative.x - perp.x * half_edge_len + edge_dir.x * line_width,
            y,
            midpoint_relative.z - perp.z * half_edge_len + edge_dir.z * line_width,
        ];
        let v3 = [
            midpoint_relative.x - perp.x * half_edge_len - edge_dir.x * line_width,
            y,
            midpoint_relative.z - perp.z * half_edge_len - edge_dir.z * line_width,
        ];

        positions.push(v0);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.0, 0.0]);

        positions.push(v1);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.0, 1.0]);

        positions.push(v2);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([1.0, 0.0]);

        positions.push(v3);
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

    // Deduplicate corner positions (merge positions that are very close)
    let threshold = 0.1; // Consider positions within 0.1 units as the same
    let mut unique_corners: Vec<[f32; 3]> = Vec::new();

    for corner in corner_positions {
        let is_duplicate = unique_corners.iter().any(|existing| {
            let dx = existing[0] - corner[0];
            let dz = existing[2] - corner[2];
            (dx * dx + dz * dz).sqrt() < threshold
        });

        if !is_duplicate {
            unique_corners.push(corner);
        }
    }

    // Add circles at each unique corner to fill in the gaps
    let circle_segments = 12;
    let circle_radius = line_width * 1.00; // Larger radius to fully cover corner gaps
    for corner_pos in unique_corners {
        let center_idx = positions.len() as u32;

        // Center vertex
        positions.push(corner_pos);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.5, 0.5]);

        // Create circle vertices
        for i in 0..circle_segments {
            let angle = (i as f32 / circle_segments as f32) * 2.0 * std::f32::consts::PI;
            let x = corner_pos[0] + circle_radius * angle.cos();
            let z = corner_pos[2] + circle_radius * angle.sin();

            positions.push([x, y, z]);
            normals.push([0.0, 1.0, 0.0]);
            uvs.push([0.0, 0.0]);
        }

        // Create triangles for the circle
        for i in 0..circle_segments {
            let next_i = (i + 1) % circle_segments;
            indices.push(center_idx);
            indices.push(center_idx + 1 + i);
            indices.push(center_idx + 1 + next_i);
        }
    }

    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
        .with_inserted_indices(Indices::U32(indices))
}

fn create_hexagon_edges_mesh(edges: &[bool; 6]) -> Mesh {
    // Create a mesh with only specific edges enabled
    // edges[i] = true means draw edge i
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    let y = 0.0;
    // Make outline 8px smaller
    let outer_radius = HEX_RADIUS + (OUTLINE_WIDTH / 2.0) - 8.0;
    let inner_radius = HEX_RADIUS - (OUTLINE_WIDTH / 2.0) - 8.0;

    // Create vertices only for enabled edges
    for i in 0..6 {
        if !edges[i] {
            continue;
        }

        let angle = std::f32::consts::PI / 3.0 * i as f32;
        let next_angle = std::f32::consts::PI / 3.0 * ((i + 1) % 6) as f32;

        let cos1 = angle.cos();
        let sin1 = angle.sin();
        let cos2 = next_angle.cos();
        let sin2 = next_angle.sin();

        let base_idx = positions.len() as u32;

        // First corner outer
        positions.push([outer_radius * cos1, y, outer_radius * sin1]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([1.0, 0.0]);

        // First corner inner
        positions.push([inner_radius * cos1, y, inner_radius * sin1]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.0, 0.0]);

        // Second corner outer
        positions.push([outer_radius * cos2, y, outer_radius * sin2]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([1.0, 0.0]);

        // Second corner inner
        positions.push([inner_radius * cos2, y, inner_radius * sin2]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.0, 0.0]);

        // Create triangles for this edge
        indices.push(base_idx);
        indices.push(base_idx + 1);
        indices.push(base_idx + 2);

        indices.push(base_idx + 1);
        indices.push(base_idx + 3);
        indices.push(base_idx + 2);
    }

    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
        .with_inserted_indices(Indices::U32(indices))
}

fn setup_hex_map(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    asset_server: Res<AssetServer>,
    config: Res<HexMapConfig>,
    obstacles: Res<Obstacles>,
    launch_pads: Res<LaunchPads>,
) {
    // Spawn 3D camera with orthographic projection
    let mut orthographic = OrthographicProjection::default_3d();
    orthographic.scale = 0.8;

    commands.spawn((
        Camera3d::default(),
        Camera {
            order: 0,
            ..default()
        },
        Projection::Orthographic(orthographic),
        Transform::from_xyz(0.0, 300.0, 400.0).looking_at(Vec3::ZERO, Vec3::Y),
        GameCamera,
    ));

    // Add directional light
    commands.spawn((
        DirectionalLight {
            illuminance: 10000.0,
            shadows_enabled: false,
            ..default()
        },
        Transform::from_xyz(4.0, 8.0, 4.0).looking_at(Vec3::ZERO, Vec3::Y),
    ));

    // Add ambient light for better illumination
    commands.spawn(AmbientLight {
        color: Color::srgb(1.0, 1.0, 1.0),
        brightness: 500.0,
        affects_lightmapped_meshes: false,
    });

    let prism_height = 20.0;

    // Reuse meshes
    let _hex_mesh = meshes.add(create_hexagon_prism_mesh(prism_height));
    let filled_hex_mesh = meshes.add(create_filled_hexagon_mesh());
    let hex_border_mesh = meshes.add(create_filled_hexagon_border_mesh());
    let hover_outline_mesh = meshes.add(create_hexagon_outline_mesh(63.0, 4.0)); // Same as destination ring
    let billboard_mesh = meshes.add(create_billboard_mesh(HEX_WIDTH - 32.0, HEX_WIDTH - 32.0));

    // Load obstacle sprite texture
    let obstacle_texture = asset_server.load("details.png");

    // Create parent HexMap entity
    commands.spawn((
        HexMap,
        Transform::default(),
        Visibility::default(),
        Name::new("HexMap"),
    )).with_children(|parent| {
        for q in -config.map_radius..=config.map_radius {
            let r1 = (-config.map_radius).max(-q - config.map_radius);
            let r2 = config.map_radius.min(-q + config.map_radius);

            for r in r1..=r2 {
                let height = prism_height;
                let world_pos = axial_to_world_pos(q, r);

                let is_obstacle = obstacles.positions.contains(&(q, r));

                // Check if this position is part of any launch platform and get its index
                let pad_index = launch_pads.pads.iter().position(|platform| {
                    platform.contains(&(q, r))
                });
                let is_launch_pad = pad_index.is_some();

                // Alternate tile colors based on hex coordinates
                let color = if is_launch_pad {
                    Color::srgb(0.3, 0.3, 0.3) // Dark grey for launch pads
                } else if (q + r) % 2 == 0 {
                    Color::srgb(0.35, 0.75, 0.35) // Light green
                } else {
                    Color::srgb(0.3, 0.65, 0.3) // Lighter medium green
                };

                let hex_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

                // Spawn border hexagon (light gray, slightly larger and below the tile)
                let border_pos = world_pos + Vec3::new(0.0, 0.4, 0.0);
                parent.spawn((
                    Mesh3d(hex_border_mesh.clone()),
                    MeshMaterial3d(materials.add(StandardMaterial {
                        base_color: Color::srgb(0.7, 0.7, 0.7),
                        emissive: Color::srgb(0.7, 0.7, 0.7).into(),
                        unlit: true,
                        double_sided: true,
                        cull_mode: None,
                        ..default()
                    })),
                    Transform::from_translation(border_pos).with_rotation(hex_rotation),
                ));

                // Spawn filled hexagon (no prism for now)
                let filled_hex_pos = world_pos + Vec3::new(0.0, 0.5, 0.0);
                let mut hex_entity_commands = parent.spawn((
                    Mesh3d(filled_hex_mesh.clone()),
                    MeshMaterial3d(materials.add(StandardMaterial {
                        base_color: color,
                        emissive: color.into(),
                        unlit: true,
                        double_sided: true,
                        cull_mode: None,
                        ..default()
                    })),
                    Transform::from_translation(filled_hex_pos).with_rotation(hex_rotation),
                    HexTile { q, r, _height: height },
                    Name::new(format!("Hex ({}, {})", q, r)),
                ));

                // Add LaunchPadTile component if this is a launch pad
                if let Some(idx) = pad_index {
                    hex_entity_commands.insert(LaunchPadTile { pad_index: idx });
                }

                let hex_entity = hex_entity_commands.id();

                // Spawn hex outline (skip for obstacles since they use sprites)
                let base_outline_height = 1.0;
                let outline_pos = if is_launch_pad {
                    world_pos + Vec3::new(0.0, base_outline_height + 0.2, 0.0)
                } else {
                    world_pos + Vec3::new(0.0, base_outline_height, 0.0)
                };

                let outline_color = Color::srgb(1.0, 1.0, 1.0); // White
                let outline_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

                if is_obstacle {
                    // Spawn billboard mesh with texture for obstacles
                    let sprite_height = 10.0;
                    let sprite_pos = world_pos + Vec3::new(0.0, sprite_height, 0.0);

                    parent.spawn((
                        Mesh3d(billboard_mesh.clone()),
                        MeshMaterial3d(materials.add(StandardMaterial {
                            base_color_texture: Some(obstacle_texture.clone()),
                            alpha_mode: AlphaMode::Blend,
                            unlit: true,
                            ..default()
                        })),
                        Transform::from_translation(sprite_pos),
                        ObstacleSprite,
                    ));
                } else if is_launch_pad {
                    // Only process this once per pad (only for the first hex in the pad)
                    let pad_idx = pad_index.unwrap();
                    let pad_cells = &launch_pads.pads[pad_idx];
                    let is_first_cell = pad_cells.first() == Some(&(q, r));

                    if is_first_cell {
                        // Build hull by collecting all edges and removing duplicates
                        use std::collections::HashMap;

                        // Edge represented as sorted pair of axial coordinates
                        type Edge = ((i32, i32), (i32, i32));
                        let mut edge_counts: HashMap<Edge, usize> = HashMap::new();

                        // For each cell in the pad, add all 6 edges
                        for &(cell_q, cell_r) in pad_cells {
                            // 6 neighbors define the 6 edges
                            let neighbors = [
                                (cell_q + 1, cell_r),
                                (cell_q + 1, cell_r - 1),
                                (cell_q, cell_r - 1),
                                (cell_q - 1, cell_r),
                                (cell_q - 1, cell_r + 1),
                                (cell_q, cell_r + 1),
                            ];

                            // Each edge is between this cell and a neighbor
                            for neighbor in neighbors {
                                // Normalize edge representation (smaller coord first)
                                let edge = if (cell_q, cell_r) < neighbor {
                                    ((cell_q, cell_r), neighbor)
                                } else {
                                    (neighbor, (cell_q, cell_r))
                                };
                                *edge_counts.entry(edge).or_insert(0) += 1;
                            }
                        }

                        // Edges that appear exactly once are on the perimeter
                        let perimeter_edges: Vec<Edge> = edge_counts
                            .into_iter()
                            .filter(|(_, count)| *count == 1)
                            .map(|(edge, _)| edge)
                            .collect();

                        // Calculate the center of the launch pad
                        let mut center_x = 0.0;
                        let mut center_z = 0.0;
                        for &(cell_q, cell_r) in pad_cells {
                            let pos = axial_to_world_pos(cell_q, cell_r);
                            center_x += pos.x;
                            center_z += pos.z;
                        }
                        center_x /= pad_cells.len() as f32;
                        center_z /= pad_cells.len() as f32;
                        let pad_center = Vec3::new(center_x, 0.0, center_z);

                        // Create outline mesh from perimeter edges
                        println!("Launch pad {} has {} perimeter edges", pad_idx, perimeter_edges.len());
                        if !perimeter_edges.is_empty() {
                            let outline_mesh = create_launch_pad_outline_mesh(&perimeter_edges, pad_center);
                            let outline_mesh_handle = meshes.add(outline_mesh);

                            // Position at Y height
                            let outline_y = 1.2;

                            parent.spawn((
                                Mesh3d(outline_mesh_handle),
                                MeshMaterial3d(materials.add(StandardMaterial {
                                    base_color: Color::srgb(0.7, 0.7, 0.7), // Start with light gray
                                    emissive: Color::srgb(0.7, 0.7, 0.7).into(),
                                    unlit: true,
                                    double_sided: true,
                                    cull_mode: None,
                                    ..default()
                                })),
                                Transform::from_translation(Vec3::new(pad_center.x, outline_y, pad_center.z))
                                    .with_scale(Vec3::splat(1.0)),
                                LaunchPadOutline {
                                    pad_index: pad_idx,
                                },
                            ));
                        }
                    }
                }

                // Always spawn hover highlight for all tiles (not obstacles)
                if !is_obstacle {
                    // Hover highlight positioned higher to avoid z-fighting
                    let hover_pos = world_pos + Vec3::new(0.0, 2.5, 0.0);
                    let hover_color = Color::srgb(0.7, 0.7, 0.7); // Light grey to match destination ring
                    parent.spawn((
                        Mesh3d(hover_outline_mesh.clone()),
                        MeshMaterial3d(materials.add(StandardMaterial {
                            base_color: hover_color,
                            emissive: hover_color.into(),
                            unlit: true,
                            double_sided: true,
                            cull_mode: None,
                            ..default()
                        })),
                        Transform::from_translation(hover_pos)
                            .with_rotation(outline_rotation)
                            .with_scale(Vec3::splat(0.75)), // Match destination ring resting scale
                        HexOutline { hex_entity },
                        Visibility::Hidden,
                    ));
                }

                if is_obstacle {
                    println!("Creating RED OBSTACLE at ({}, {})", q, r);
                }
                if is_launch_pad {
                    println!("Creating BLUE LAUNCH PAD at ({}, {})", q, r);
                }
            }
        }

        // Spawn crystal fields at specific positions
        // 3 on each side of the battlefield, away from launch pads
        let crystal_positions = vec![
            // Left side (Red territory)
            (-4, 0),   // Far left, middle
            (-3, -2),  // Left, lower
            (-2, -3),  // Left, bottom
            // Right side (Blue territory)
            (4, 0),    // Far right, middle
            (3, -2),   // Right, lower
            (2, -3),   // Right, bottom
        ];

        for (q, r) in crystal_positions {
            let world_pos = axial_to_world_pos(q, r);

            // Load crystal model
            let crystal_scene: Handle<Scene> = asset_server.load("Lighthing Crystal.glb#Scene0");

            // Random crystal count between 200-400
            let crystals = 200 + (((q + r) * 73) % 201).abs(); // Pseudo-random based on position

            println!("Creating CRYSTAL FIELD at ({}, {}) with {} crystals", q, r, crystals);

            // Spawn parent entity for the crystal field
            parent.spawn((
                Transform::from_translation(world_pos),
                Visibility::default(),
                CrystalField {
                    q,
                    r,
                    crystals_remaining: crystals,
                    max_crystals: crystals,
                },
                Name::new(format!("Crystal Field ({}, {})", q, r)),
            )).with_children(|field_parent| {
                // Spawn 2-3 crystal models randomly positioned within the cell
                let num_crystals = 2 + (((q * 7 + r * 11) % 2).abs() as usize); // 2 or 3 crystals

                for i in 0..num_crystals {
                    let i_i32 = i as i32;
                    // Pseudo-random position within the hex (radius ~40)
                    let angle = (i as f32 * 2.5 + (q + r) as f32 * 0.7) * std::f32::consts::PI;
                    let radius = 25.0 + ((i_i32 * 13 + q * 7) % 20) as f32;
                    let offset_x = angle.cos() * radius;
                    let offset_z = angle.sin() * radius;

                    // Random rotation
                    let rotation_y = (i as f32 * 1.3 + (q + r) as f32 * 0.5) * std::f32::consts::PI;

                    // Random rotation speed and pulse offset for sparkle effect
                    let rotation_speed = 0.3 + (i as f32 * 0.1);
                    let pulse_offset = i as f32 * 2.0;

                    field_parent.spawn((
                        SceneRoot(crystal_scene.clone()),
                        Transform::from_translation(Vec3::new(offset_x, 8.0, offset_z))
                            .with_scale(Vec3::splat(4.0)) // Smaller crystals
                            .with_rotation(Quat::from_rotation_y(rotation_y)),
                        CrystalVisual {
                            rotation_speed,
                            pulse_offset,
                        },
                        Name::new(format!("Crystal {}", i)),
                    ));
                }
            });
        }
    });
}

fn hex_hover_system(
    camera_query: Query<(&Camera, &GlobalTransform), With<GameCamera>>,
    mut hovered_hex: ResMut<HoveredHex>,
    hex_query: Query<(Entity, &HexTile)>,
    windows: Query<&Window>,
    obstacles: Res<Obstacles>,
) {
    let Ok((camera, camera_transform)) = camera_query.single() else {
        return;
    };

    let Some(cursor_position) = windows.single().ok().and_then(|w| w.cursor_position()) else {
        hovered_hex.entity = None;
        return;
    };

    let Some(ray) = camera.viewport_to_world(camera_transform, cursor_position).ok() else {
        return;
    };

    let ground_plane_normal = Vec3::Y;
    let ground_plane_point = Vec3::ZERO;
    let denom = ground_plane_normal.dot(*ray.direction);

    if denom.abs() > 1e-6 {
        let t = (ground_plane_point - ray.origin).dot(ground_plane_normal) / denom;
        if t >= 0.0 {
            let world_pos = ray.origin + *ray.direction * t;

            let mut closest_hex: Option<(Entity, i32, i32, f32)> = None;
            for (entity, hex_tile) in hex_query.iter() {
                let hex_world_pos = axial_to_world_pos(hex_tile.q, hex_tile.r);
                let distance = (world_pos - hex_world_pos).length();

                if distance < HEX_RADIUS {
                    if let Some((_, _, _, closest_dist)) = closest_hex {
                        if distance < closest_dist {
                            closest_hex = Some((entity, hex_tile.q, hex_tile.r, distance));
                        }
                    } else {
                        closest_hex = Some((entity, hex_tile.q, hex_tile.r, distance));
                    }
                }
            }

            if let Some((entity, q, r, _)) = closest_hex {
                // if hovered_hex.entity != Some(entity) {
                //     let obstacle_marker = if obstacles.positions.contains(&(q, r)) {
                //         " [OBSTACLE]"
                //     } else {
                //         ""
                //     };
                //     println!("Hovering: ({}, {}){}", q, r, obstacle_marker);
                // }
                hovered_hex.entity = Some(entity);
                hovered_hex.q = q;
                hovered_hex.r = r;
            } else {
                hovered_hex.entity = None;
            }
            return;
        }
    }

    hovered_hex.entity = None;
}

fn update_outline_colors(
    hovered_hex: Res<HoveredHex>,
    mut outline_query: Query<
        (&HexOutline, &mut Visibility),
        Without<LaunchPadOutline>,
    >,
    unit_query: Query<(&Unit, Has<Selected>)>,
    hex_query: Query<&HexTile>,
) {
    // Find if there's an unselected unit at the hovered position
    let has_unselected_unit = if let Some(hovered_entity) = hovered_hex.entity {
        if let Ok(hex_tile) = hex_query.get(hovered_entity) {
            let (hovered_q, hovered_r) = (hex_tile.q, hex_tile.r);
            unit_query.iter().any(|(unit, is_selected)| {
                unit.q == hovered_q && unit.r == hovered_r && !is_selected
            })
        } else {
            false
        }
    } else {
        false
    };

    for (outline, mut visibility) in &mut outline_query {
        let is_hovered = hovered_hex.entity == Some(outline.hex_entity);

        // Only show outline when hovered AND there's an unselected unit on the hex
        *visibility = if is_hovered && has_unselected_unit {
            Visibility::Visible
        } else {
            Visibility::Hidden
        };
    }
}

fn update_launch_pad_colors(
    pad_ownership: Res<LaunchPadOwnership>,
    mut outline_query: Query<(&LaunchPadOutline, &MeshMaterial3d<StandardMaterial>)>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    for (outline, material_handle) in &mut outline_query {
        // Get the owner of this pad
        let owner = pad_ownership.owners.get(outline.pad_index)
            .copied()
            .unwrap_or(LaunchPadOwner::Neutral);

        // Update color based on owner
        let color = match owner {
            LaunchPadOwner::Red => Color::srgb(0.9, 0.2, 0.2),
            LaunchPadOwner::Blue => Color::srgb(0.2, 0.4, 0.9),
            LaunchPadOwner::Neutral => Color::srgb(0.7, 0.7, 0.7), // Light gray when no one owns it
            LaunchPadOwner::Contested => Color::srgb(0.8, 0.7, 0.0), // Yellow when both armies are on it
        };

        if let Some(material) = materials.get_mut(&material_handle.0) {
            material.base_color = color;
            material.emissive = color.into();
        }
    }
}

fn billboard_sprites(
    mut sprite_query: Query<&mut Transform, With<ObstacleSprite>>,
    camera_query: Query<&Transform, (With<GameCamera>, Without<ObstacleSprite>)>,
) {
    let Ok(camera_transform) = camera_query.single() else {
        return;
    };

    for mut sprite_transform in &mut sprite_query {
        // Make sprite face the camera
        sprite_transform.look_at(camera_transform.translation, Vec3::Y);
    }
}

fn apply_crystal_materials(
    mut commands: Commands,
    crystal_query: Query<(Entity, &Children), With<CrystalField>>,
    mesh_query: Query<(Entity, &MeshMaterial3d<StandardMaterial>), With<Mesh3d>>,
    children_query: Query<&Children>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    for (crystal_entity, children) in &crystal_query {
        // Recursively find all mesh entities in descendants
        let mut entities_to_check = Vec::new();
        for &child in children {
            entities_to_check.push(child);
        }

        while let Some(entity) = entities_to_check.pop() {
            // Check if this entity has a mesh with material
            if let Ok((mesh_entity, existing_material)) = mesh_query.get(entity) {
                // Check if it already has gold material by checking base color
                if let Some(mat) = materials.get(&existing_material.0) {
                    let is_gold = (mat.base_color.to_srgba().red - 1.0).abs() < 0.1
                        && (mat.base_color.to_srgba().green - 0.84).abs() < 0.1;

                    if !is_gold {
                        // Create gold material
                        let gold_material = materials.add(StandardMaterial {
                            base_color: Color::srgb(1.0, 0.84, 0.0), // Gold color
                            emissive: Color::srgb(1.0, 0.84, 0.0).into(), // Strong gold glow
                            metallic: 0.95,
                            perceptual_roughness: 0.05,
                            unlit: false,
                            ..default()
                        });

                        commands.entity(mesh_entity).insert(MeshMaterial3d(gold_material));
                    }
                }
            }

            // Add children to check list
            if let Ok(entity_children) = children_query.get(entity) {
                for &child in entity_children {
                    entities_to_check.push(child);
                }
            }
        }
    }
}

fn animate_crystal_sparkle(
    time: Res<Time>,
    mut crystal_query: Query<(&CrystalVisual, &mut Transform, &Children)>,
    mesh_query: Query<(Entity, &MeshMaterial3d<StandardMaterial>), With<Mesh3d>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    children_query: Query<&Children>,
) {
    let elapsed = time.elapsed_secs();

    for (crystal_visual, mut transform, children) in &mut crystal_query {
        // Rotate the crystal
        transform.rotate_y(crystal_visual.rotation_speed * time.delta_secs());

        // Pulse the emissive intensity of all child meshes
        let pulse = (elapsed * 2.0 + crystal_visual.pulse_offset).sin() * 0.5 + 0.5; // 0.0 to 1.0
        let intensity = 0.5 + pulse * 1.5; // 0.5 to 2.0

        // Find all mesh entities in descendants and update their emissive
        let mut entities_to_check = Vec::new();
        for &child in children {
            entities_to_check.push(child);
        }

        while let Some(entity) = entities_to_check.pop() {
            if let Ok((_, material_handle)) = mesh_query.get(entity) {
                if let Some(mat) = materials.get_mut(&material_handle.0) {
                    // Only update if it's gold (check base color)
                    let is_gold = (mat.base_color.to_srgba().red - 1.0).abs() < 0.1;
                    if is_gold {
                        mat.emissive = Color::srgb(1.0 * intensity, 0.84 * intensity, 0.0).into();
                    }
                }
            }

            if let Ok(entity_children) = children_query.get(entity) {
                for &child in entity_children {
                    entities_to_check.push(child);
                }
            }
        }
    }
}
