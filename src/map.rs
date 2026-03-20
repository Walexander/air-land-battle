use bevy::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::mesh::{Indices, PrimitiveTopology};

use crate::ui::GameCamera;
use crate::launch_pads::{LaunchPadOwner, LaunchPadOwnership};
use crate::selection::{create_hexagon_outline_mesh, Selected};
use crate::units::Unit;
use crate::loading::LoadingState;

// Hex grid constants
const HEX_WIDTH: f32 = 128.0;
const HEX_HEIGHT: f32 = HEX_WIDTH * 0.866_025_4; // width * sqrt(3)/2
const HEX_RADIUS: f32 = HEX_WIDTH / 2.0;

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
pub struct HexOutline {}

#[derive(Component)]
pub struct DebugOutline;

#[derive(Resource, Default)]
pub struct DebugOverlay(pub bool);

#[derive(Component)]
pub struct ObstacleSprite;

#[derive(Component)]
pub struct HQ {
    pub army: crate::units::Army,
    pub q: i32,
    pub r: i32,
}

#[derive(Component)]
pub struct LaunchPadOutline {
    pub pad_index: usize,
}

#[derive(Component)]
pub struct LaunchPadTile {}

#[derive(Component)]
pub struct FogOfWar {
    pub hex_q: i32,
    pub hex_r: i32,
}

#[derive(Component)]
pub struct CrystalField {
    pub q: i32,
    pub r: i32,
    pub crystals_remaining: i32,
    pub max_crystals: i32,
}


#[derive(Component)]
struct CrystalVisual {
    rotation_speed: f32,
    pulse_offset: f32,
    index: usize, // Index of this crystal visual (0, 1, 2, ...)
}

// Resources
#[derive(Resource, Default)]
pub struct HexMapConfig {
    pub valid_cells: std::collections::HashSet<(i32, i32)>,
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
        app.insert_resource(HexMapConfig::default())
            .insert_resource(HoveredHex::default())
            .insert_resource(Obstacles::default())
            .insert_resource(DebugOverlay::default())
            .insert_resource(ClearColor(Color::srgb(0.53, 0.81, 0.92))) // Light sky blue
            .add_systems(OnEnter(LoadingState::Playing), (setup_hex_map, crate::hex_pathfinding::setup_hex_pathfinding).chain())
            .add_systems(Update, (hex_hover_system, update_outline_colors, update_launch_pad_colors, billboard_sprites, apply_crystal_materials, animate_crystal_sparkle, update_fog_of_war, update_crystal_visuals, toggle_debug_overlay).run_if(in_state(LoadingState::Playing)));
    }
}

pub fn axial_to_world_pos(q: i32, r: i32) -> Vec3 {
    // Pointy-top hex coordinates
    let x = HEX_HEIGHT * (q as f32 + r as f32 * 0.5);
    let z = HEX_WIDTH * 0.75 * r as f32;
    Vec3::new(x, 0.0, z)
}

pub fn world_pos_to_axial(x: f32, z: f32) -> (i32, i32) {
    // Inverse of axial_to_world_pos for pointy-top hex coordinates
    // From: x = HEX_HEIGHT * (q + r * 0.5)
    //       z = HEX_WIDTH * 0.75 * r
    // Solve for q and r:
    let r = z / (HEX_WIDTH * 0.75);
    let q = (x / HEX_HEIGHT) - (r * 0.5);

    // Round to nearest integer coordinates
    (q.round() as i32, r.round() as i32)
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

/// Build a flat polygon outline mesh (in the XZ plane) from world-space (wx, wz) vertices.
/// Each edge becomes a thin quad with the given `line_width`. `y` sets the height.
fn create_polygon_outline_mesh(points: &[(f32, f32)], y: f32, line_width: f32) -> Mesh {
    let n = points.len();
    let mut positions: Vec<[f32; 3]> = Vec::new();
    let mut normals: Vec<[f32; 3]> = Vec::new();
    let mut indices: Vec<u32> = Vec::new();

    for i in 0..n {
        let (x0, z0) = points[i];
        let (x1, z1) = points[(i + 1) % n];
        let dx = x1 - x0;
        let dz = z1 - z0;
        let len = (dx * dx + dz * dz).sqrt();
        if len < 0.001 {
            continue;
        }
        // Perpendicular to edge direction (in XZ plane)
        let perp_x = -dz / len;
        let perp_z =  dx / len;
        let half = line_width * 0.5;
        let base = positions.len() as u32;
        positions.push([x0 + perp_x * half, y, z0 + perp_z * half]);
        positions.push([x0 - perp_x * half, y, z0 - perp_z * half]);
        positions.push([x1 + perp_x * half, y, z1 + perp_z * half]);
        positions.push([x1 - perp_x * half, y, z1 - perp_z * half]);
        for _ in 0..4 {
            normals.push([0.0, 1.0, 0.0]);
        }
        indices.extend_from_slice(&[base, base+1, base+2, base+1, base+3, base+2]);
    }

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default());
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.insert_indices(Indices::U32(indices));
    mesh
}

/// Ear-clipping triangulation for a simple (non-self-intersecting) polygon.
/// `points` must NOT repeat the first vertex at the end.
fn triangulate_polygon(points: &[(f32, f32)]) -> Vec<u32> {
    let n = points.len();
    if n < 3 { return vec![]; }
    if n == 3 { return vec![0, 1, 2]; }

    // Signed area via shoelace — positive = CCW in standard (y-up) coords.
    let area2: f32 = (0..n)
        .map(|i| {
            let (x0, z0) = points[i];
            let (x1, z1) = points[(i + 1) % n];
            x0 * z1 - x1 * z0
        })
        .sum();
    let ccw = area2 > 0.0;

    let mut idx: Vec<usize> = (0..n).collect();
    let mut tris: Vec<u32> = Vec::with_capacity((n - 2) * 3);
    let mut guard = n * n;

    'clip: while idx.len() > 3 && guard > 0 {
        guard -= 1;
        let m = idx.len();
        for i in 0..m {
            let ia = idx[(i + m - 1) % m];
            let ib = idx[i];
            let ic = idx[(i + 1) % m];
            let a = points[ia]; let b = points[ib]; let c = points[ic];
            let cross = (b.0 - a.0) * (c.1 - a.1) - (b.1 - a.1) * (c.0 - a.0);
            let convex = if ccw { cross > 0.0 } else { cross < 0.0 };
            if !convex { continue; }
            // Valid ear: no other polygon vertex lies strictly inside triangle abc.
            if idx.iter().filter(|&&j| j != ia && j != ib && j != ic)
                .all(|&j| !tri_contains_2d(points[j], a, b, c))
            {
                tris.extend_from_slice(&[ia as u32, ib as u32, ic as u32]);
                idx.remove(i);
                continue 'clip;
            }
        }
        break; // degenerate polygon
    }
    if idx.len() >= 3 {
        tris.extend_from_slice(&[idx[0] as u32, idx[1] as u32, idx[2] as u32]);
    }
    tris
}

/// Returns true if point `p` lies strictly inside triangle (a, b, c).
fn tri_contains_2d(p: (f32, f32), a: (f32, f32), b: (f32, f32), c: (f32, f32)) -> bool {
    let cross = |o: (f32, f32), u: (f32, f32), v: (f32, f32)| -> f32 {
        (u.0 - o.0) * (v.1 - o.1) - (u.1 - o.1) * (v.0 - o.0)
    };
    let d1 = cross(a, b, p);
    let d2 = cross(b, c, p);
    let d3 = cross(c, a, p);
    let has_neg = d1 < 0.0 || d2 < 0.0 || d3 < 0.0;
    let has_pos = d1 > 0.0 || d2 > 0.0 || d3 > 0.0;
    !(has_neg && has_pos)
}

/// Build a filled polygon mesh (XZ plane) using ear-clipping triangulation.
fn create_filled_polygon_mesh(points: &[(f32, f32)], y: f32) -> Mesh {
    let n = points.len();
    let positions: Vec<[f32; 3]> = points.iter().map(|&(x, z)| [x, y, z]).collect();
    let normals: Vec<[f32; 3]> = vec![[0.0, 1.0, 0.0]; n];
    let indices = triangulate_polygon(points);

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default());
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.insert_indices(Indices::U32(indices));
    mesh
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

fn setup_hex_map(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    asset_server: Res<AssetServer>,
    mut obstacles: ResMut<Obstacles>,
    mut map_config: ResMut<HexMapConfig>,
    map_def: Res<crate::map_loader::MapDefinition>,
) {
    // Populate obstacles from the loaded map definition
    obstacles.positions = map_def.obstacles.clone();
    // Build valid_cells from all rendered tiles (tile_map + launch_pad_cells).
    map_config.valid_cells = map_def.tile_map.keys().cloned()
        .chain(map_def.launch_pad_cells.iter().cloned())
        .collect();
    // Spawn 3D camera with orthographic projection
    let mut orthographic = OrthographicProjection::default_3d();
    orthographic.scale = 0.8;
    // Increase far plane to prevent clipping when camera pans
    orthographic.far = 2000.0;

    commands.spawn((
        Camera3d::default(),
        Camera {
            order: 0,
            ..default()
        },
        Projection::Orthographic(orthographic),
        Transform::from_xyz(0.0, 300.0, 500.0).looking_at(Vec3::ZERO, Vec3::Y),
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

    // Load mountain 3D model
    let mountain_model = asset_server.load("mountains.glb#Scene0");

    // Create parent HexMap entity
    commands.spawn((
        HexMap,
        Transform::default(),
        Visibility::default(),
        Name::new("HexMap"),
    )).with_children(|parent| {
        // Iterate all tiles in the TMX tile layer, plus any launch-pad cells that
        // weren't present in the tile layer (polygon-derived, GID=0 in the CSV).
        let mut tile_map = map_def.tile_map.clone();
        for &cell in &map_def.launch_pad_cells {
            tile_map.entry(cell).or_insert(0);
        }
        for (&(q, r), &gid) in &tile_map {
                let world_pos = axial_to_world_pos(q, r);

                let height = prism_height;

                let is_obstacle = obstacles.positions.contains(&(q, r));

                // A tile belongs to a launch pad if the polygon step assigned it to a group,
                // regardless of its GID (the polygon is authoritative).
                let pad_index = map_def.launch_pads.iter().position(|platform| platform.contains(&(q, r)));
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
                if pad_index.is_some() {
                    hex_entity_commands.insert(LaunchPadTile {});
                }

                // Spawn hex outline (skip for obstacles since they use sprites)
                let base_outline_height = 1.0;
                let _outline_pos = if is_launch_pad {
                    world_pos + Vec3::new(0.0, base_outline_height + 0.2, 0.0)
                } else {
                    world_pos + Vec3::new(0.0, base_outline_height, 0.0)
                };

                let _outline_color = Color::srgb(1.0, 1.0, 1.0); // White
                let outline_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

                if is_obstacle {
                    // Check if this is an HQ position using loaded map definition
                    let is_red_hq = map_def.hq_red == Some((q, r));
                    let is_blue_hq = map_def.hq_blue == Some((q, r));
                    let is_hq = is_red_hq || is_blue_hq;

                    if is_hq {
                        // Spawn HQ building using HexBase from JustBuildings.glb
                        let hq_model: Handle<Scene> = asset_server.load("JustBuildings.glb#Scene0");
                        let hq_scale = 24.0; // 20% larger than original 20.0
                        let hq_pos = world_pos + Vec3::new(0.0, 10.0, 0.0);
                        let hq_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

                        let army = if is_red_hq {
                            crate::units::Army::Red
                        } else {
                            crate::units::Army::Blue
                        };

                        parent.spawn((
                            SceneRoot(hq_model),
                            Transform::from_translation(hq_pos)
                                .with_rotation(hq_rotation)
                                .with_scale(Vec3::splat(hq_scale)),
                            HQ { army, q, r },
                            Name::new(format!("{:?} HQ", army)),
                        ));

                        println!("Creating {:?} HQ at ({}, {})", army, q, r);
                    } else {
                        // Spawn 3D mountain model for regular obstacles
                        // Mountain is 4x4 in Blender, scale to fill hex cell
                        let mountain_scale = 21.25; // 25.0 * 0.85
                        // Raise the mountain so its base sits above the tile, offset slightly in tile
                        let mountain_pos = world_pos + Vec3::new(0.0, 10.0, 12.0);
                        // Rotate to align with hex grid
                        let mountain_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

                        parent.spawn((
                            SceneRoot(mountain_model.clone()),
                            Transform::from_translation(mountain_pos)
                                .with_rotation(mountain_rotation)
                                .with_scale(Vec3::splat(mountain_scale)),
                        ));
                    }
                } else if is_launch_pad {
                    // Only process this once per pad (only for the first hex in the pad)
                    let pad_idx = pad_index.unwrap();
                    let pad_cells = &map_def.launch_pads[pad_idx];
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
                    // Hover highlight positioned higher to be visible above UI
                    let hover_pos = world_pos + Vec3::new(0.0, 8.0, 0.0);
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
                        HexOutline {},
                        Visibility::Hidden,
                    ));

                    // Spawn fog of war overlay (visible by default until units reveal the area)
                    let fog_pos = world_pos + Vec3::new(0.0, 3.0, 0.0);
                    let fog_color = Color::srgba(0.0, 0.0, 0.0, 0.7); // Dark semi-transparent overlay
                    parent.spawn((
                        Mesh3d(filled_hex_mesh.clone()),
                        MeshMaterial3d(materials.add(StandardMaterial {
                            base_color: fog_color,
                            alpha_mode: AlphaMode::Blend,
                            unlit: true,
                            double_sided: true,
                            cull_mode: None,
                            ..default()
                        })),
                        Transform::from_translation(fog_pos)
                            .with_rotation(hex_rotation),
                        FogOfWar {
                            hex_q: q,
                            hex_r: r,
                        },
                        Visibility::Visible,
                    ));
                }

                // Debug outline — only for obstacles and crystal fields (no green on regular tiles).
                let debug_color = if is_obstacle {
                    Some(Color::srgb(0.0, 0.0, 0.0)) // black for obstacles
                } else if map_def.crystal_fields.contains(&(q, r)) {
                    Some(Color::srgb(0.6, 0.0, 0.8)) // purple for crystal fields
                } else {
                    None
                };
                if let Some(debug_color) = debug_color {
                    parent.spawn((
                        Mesh3d(hover_outline_mesh.clone()),
                        MeshMaterial3d(materials.add(StandardMaterial {
                            base_color: debug_color,
                            emissive: debug_color.into(),
                            unlit: true,
                            double_sided: true,
                            cull_mode: None,
                            ..default()
                        })),
                        Transform::from_translation(world_pos + Vec3::new(0.0, 9.0, 0.0))
                            .with_rotation(outline_rotation)
                            .with_scale(Vec3::splat(0.75)),
                        DebugOutline,
                        Visibility::Hidden,
                    ));
                }

        }

        // Spawn obstacle tiles — their positions have GID=0 in the tile layer so they
        // never appear in the tile_map loop; we handle them here instead.
        let obstacle_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
        for &(q, r) in &map_def.obstacles {
            let world_pos = axial_to_world_pos(q, r);

            // Backing hex tile (dark grey, same visual language as obstacles)
            parent.spawn((
                Mesh3d(filled_hex_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.25, 0.22, 0.20),
                    emissive: Color::srgb(0.05, 0.04, 0.04).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(world_pos + Vec3::new(0.0, 0.5, 0.0))
                    .with_rotation(obstacle_rotation),
            ));

            // Mountain/obstacle model
            let is_red_hq = map_def.hq_red == Some((q, r));
            let is_blue_hq = map_def.hq_blue == Some((q, r));
            if is_red_hq || is_blue_hq {
                let hq_model: Handle<Scene> = asset_server.load("JustBuildings.glb#Scene0");
                let army = if is_red_hq { crate::units::Army::Red } else { crate::units::Army::Blue };
                parent.spawn((
                    SceneRoot(hq_model),
                    Transform::from_translation(world_pos + Vec3::new(0.0, 10.0, 0.0))
                        .with_rotation(Quat::from_rotation_y(std::f32::consts::PI / 2.0))
                        .with_scale(Vec3::splat(24.0)),
                    HQ { army, q, r },
                ));
            } else {
                parent.spawn((
                    SceneRoot(mountain_model.clone()),
                    Transform::from_translation(world_pos + Vec3::new(0.0, 10.0, 12.0))
                        .with_rotation(Quat::from_rotation_y(std::f32::consts::PI / 2.0))
                        .with_scale(Vec3::splat(21.25)),
                ));
            }

            // Debug outline (black, same as tile-loop obstacles)
            parent.spawn((
                Mesh3d(hover_outline_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.0, 0.0, 0.0),
                    emissive: Color::srgb(0.0, 0.0, 0.0).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(world_pos + Vec3::new(0.0, 9.0, 0.0))
                    .with_rotation(obstacle_rotation)
                    .with_scale(Vec3::splat(0.75)),
                DebugOutline,
                Visibility::Hidden,
            ));
        }

        // Spawn crystal fields from loaded map definition
        let crystal_positions = map_def.crystal_fields.clone();

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
                // Spawn crystal models - number based on max_crystals (1 visual per 10 crystals)
                let num_crystals = ((crystals as f32 / 10.0).ceil() as usize).max(1).min(8);

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
                            index: i,
                        },
                        Name::new(format!("Crystal {}", i)),
                    ));
                }
            });
        }

        // Boundary filled polygons — always visible, black, cover non-playable regions.
        for poly in &map_def.boundary_polygons {
            let mesh = create_filled_polygon_mesh(poly, 0.5);
            parent.spawn((
                Mesh3d(meshes.add(mesh)),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.0, 0.0, 0.0),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::default(),
                Name::new("Boundary"),
            ));
        }

        // Debug outlines for spawn cells — raised to y=11 to sit above tile outlines (y=9).
        info!("debug: spawn_red={} spawn_blue={}", map_def.spawn_red.len(), map_def.spawn_blue.len());
        let outline_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
        for &(q, r) in &map_def.spawn_red {
            let world_pos = axial_to_world_pos(q, r);
            parent.spawn((
                Mesh3d(hover_outline_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(1.0, 0.1, 0.1),
                    emissive: Color::srgb(1.0, 0.1, 0.1).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(world_pos + Vec3::new(0.0, 11.0, 0.0))
                    .with_rotation(outline_rotation)
                    .with_scale(Vec3::splat(0.75)),
                DebugOutline,
                Visibility::Hidden,
            ));
        }
        for &(q, r) in &map_def.spawn_blue {
            let world_pos = axial_to_world_pos(q, r);
            parent.spawn((
                Mesh3d(hover_outline_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.1, 0.3, 1.0),
                    emissive: Color::srgb(0.1, 0.3, 1.0).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(world_pos + Vec3::new(0.0, 11.0, 0.0))
                    .with_rotation(outline_rotation)
                    .with_scale(Vec3::splat(0.75)),
                DebugOutline,
                Visibility::Hidden,
            ));
        }

        // Launch pad polygon outlines (black, toggled by F1 debug overlay).
        for poly in &map_def.launch_pad_polygons {
            let mesh = create_polygon_outline_mesh(poly, 13.0, 8.0);
            parent.spawn((
                Mesh3d(meshes.add(mesh)),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.0, 0.0, 0.0),
                    emissive: Color::srgb(0.0, 0.0, 0.0).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::default(),
                DebugOutline,
                Visibility::Hidden,
            ));
        }

        // Base polygon outlines (toggled by F1 debug overlay).
        if !map_def.base_red_polygon.is_empty() {
            let mesh = create_polygon_outline_mesh(&map_def.base_red_polygon, 13.0, 8.0);
            parent.spawn((
                Mesh3d(meshes.add(mesh)),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(1.0, 0.1, 0.1),
                    emissive: Color::srgb(1.0, 0.1, 0.1).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::default(),
                DebugOutline,
                Visibility::Hidden,
            ));
        }
        if !map_def.base_blue_polygon.is_empty() {
            let mesh = create_polygon_outline_mesh(&map_def.base_blue_polygon, 13.0, 8.0);
            parent.spawn((
                Mesh3d(meshes.add(mesh)),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(0.1, 0.3, 1.0),
                    emissive: Color::srgb(0.1, 0.3, 1.0).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::default(),
                DebugOutline,
                Visibility::Hidden,
            ));
        }
    });
}

fn hex_hover_system(
    camera_query: Query<(&Camera, &GlobalTransform), With<GameCamera>>,
    mut hovered_hex: ResMut<HoveredHex>,
    hex_query: Query<(Entity, &HexTile)>,
    windows: Query<&Window>,
    _obstacles: Res<Obstacles>,
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
            // Use a slightly larger radius for more forgiving hex detection
            let detection_radius = HEX_RADIUS * 1.15;

            for (entity, hex_tile) in hex_query.iter() {
                let hex_world_pos = axial_to_world_pos(hex_tile.q, hex_tile.r);
                let distance = (world_pos - hex_world_pos).length();

                if distance < detection_radius {
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
        (&HexOutline, &mut Visibility, &MeshMaterial3d<StandardMaterial>),
        Without<LaunchPadOutline>,
    >,
    unit_query: Query<(&Unit, Has<Selected>)>,
    hex_query: Query<&HexTile>,
    _materials: ResMut<Assets<StandardMaterial>>,
) {
    // Find if there's an unselected unit at the hovered position
    let _hovered_unit_info = if let Some(hovered_entity) = hovered_hex.entity {
        if let Ok(hex_tile) = hex_query.get(hovered_entity) {
            let (hovered_q, hovered_r) = (hex_tile.q, hex_tile.r);
            unit_query.iter()
                .find(|(unit, is_selected)| {
                    unit.q == hovered_q && unit.r == hovered_r && !is_selected
                })
                .map(|(unit, _)| unit.army)
        } else {
            None
        }
    } else {
        None
    };

    for (_outline, mut visibility, _material_handle) in &mut outline_query {
        // Disabled: hover ring on units provides all feedback needed
        *visibility = Visibility::Hidden;
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

fn update_fog_of_war(
    unit_query: Query<(Entity, &Unit)>,
    hq_query: Query<&HQ>,
    mut visibility_query: Query<&mut Visibility>,
    fog_query: Query<(Entity, &FogOfWar)>,
    health_bar_query: Query<(&crate::units::HealthBar, Entity)>,
    map_def: Res<crate::map_loader::MapDefinition>,
) {
    use std::collections::HashSet;

    // Collect all visible hex positions (within 2 hexes of any Red unit, 3 hexes of Red HQs)
    let mut visible_hexes: HashSet<(i32, i32)> = HashSet::new();

    // Always reveal 2-tile radius around Red spawn points so the map isn't dark at start.
    for &(sq, sr) in &map_def.spawn_red {
        for dq in -2i32..=2i32 {
            for dr in -2i32..=2i32 {
                if dq.abs().max(dr.abs()).max((-dq - dr).abs()) <= 2 {
                    visible_hexes.insert((sq + dq, sr + dr));
                }
            }
        }
    }

    for (_, unit) in &unit_query {
        // Only Red (player) units reveal fog
        if unit.army != crate::units::Army::Red {
            continue;
        }

        let unit_pos = (unit.q, unit.r);

        // Add all hexes within 2-hex radius
        for dq in -2i32..=2i32 {
            for dr in -2i32..=2i32 {
                let ds = -dq - dr;
                // Cube coordinate constraint: |dq| + |dr| + |ds| must be even
                // For hex distance <= 2: max(|dq|, |dr|, |ds|) <= 2
                if dq.abs().max(dr.abs()).max(ds.abs()) <= 2 {
                    visible_hexes.insert((unit_pos.0 + dq, unit_pos.1 + dr));
                }
            }
        }
    }

    // Add visibility from Red HQs (3-hex radius)
    for hq in &hq_query {
        // Only Red (player) HQs reveal fog
        if hq.army != crate::units::Army::Red {
            continue;
        }

        let hq_pos = (hq.q, hq.r);

        // Add all hexes within 3-hex radius
        for dq in -3i32..=3i32 {
            for dr in -3i32..=3i32 {
                let ds = -dq - dr;
                // For hex distance <= 3: max(|dq|, |dr|, |ds|) <= 3
                if dq.abs().max(dr.abs()).max(ds.abs()) <= 3 {
                    visible_hexes.insert((hq_pos.0 + dq, hq_pos.1 + dr));
                }
            }
        }
    }

    // Update fog visibility
    for (fog_entity, fog) in &fog_query {
        let hex_pos = (fog.hex_q, fog.hex_r);
        if let Ok(mut visibility) = visibility_query.get_mut(fog_entity) {
            if visible_hexes.contains(&hex_pos) {
                *visibility = Visibility::Hidden; // Hide fog where units can see
            } else {
                *visibility = Visibility::Visible; // Show fog where units can't see
            }
        }
    }

    // Track which enemy units should be visible
    let mut visible_enemy_units: HashSet<Entity> = HashSet::new();

    // Check each unit and update visibility
    for (entity, unit) in &unit_query {
        // Only hide enemy units (not player's Red units)
        if unit.army == crate::units::Army::Red {
            continue;
        }

        let unit_pos = (unit.q, unit.r);
        let should_be_visible = visible_hexes.contains(&unit_pos);

        if let Ok(mut visibility) = visibility_query.get_mut(entity) {
            if should_be_visible {
                *visibility = Visibility::Visible;
                visible_enemy_units.insert(entity);
            } else {
                *visibility = Visibility::Hidden;
                // println!("Hiding enemy unit at ({}, {})", unit_pos.0, unit_pos.1);
            }
        }
    }

    // Update health/progress bar visibility for enemy units
    for (health_bar, bar_entity) in &health_bar_query {
        // Check if this bar belongs to an enemy unit
        if let Ok((_, unit)) = unit_query.get(health_bar.unit_entity)
            && unit.army != crate::units::Army::Red {
                // This is an enemy unit's bar
                if let Ok(mut visibility) = visibility_query.get_mut(bar_entity) {
                    if visible_enemy_units.contains(&health_bar.unit_entity) {
                        *visibility = Visibility::Visible;
                    } else {
                        *visibility = Visibility::Hidden;
                    }
                }
            }
    }
}

fn apply_crystal_materials(
    mut commands: Commands,
    crystal_query: Query<(Entity, &Children), With<CrystalField>>,
    mesh_query: Query<(Entity, &MeshMaterial3d<StandardMaterial>), With<Mesh3d>>,
    children_query: Query<&Children>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    for (_crystal_entity, children) in &crystal_query {
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
            if let Ok((_, material_handle)) = mesh_query.get(entity)
                && let Some(mat) = materials.get_mut(&material_handle.0) {
                    // Only update if it's gold (check base color)
                    let is_gold = (mat.base_color.to_srgba().red - 1.0).abs() < 0.1;
                    if is_gold {
                        mat.emissive = Color::srgb(1.0 * intensity, 0.84 * intensity, 0.0).into();
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

fn update_crystal_visuals(
    mut commands: Commands,
    crystal_field_query: Query<(&CrystalField, &Children), Changed<CrystalField>>,
    crystal_visual_query: Query<(Entity, &CrystalVisual)>,
) {
    for (field, children) in &crystal_field_query {
        // Calculate how many crystals should be visible based on remaining/max ratio
        let ratio = field.crystals_remaining as f32 / field.max_crystals as f32;
        let total_visuals = ((field.max_crystals as f32 / 10.0).ceil() as usize).max(1).min(8);
        let visible_count = (ratio * total_visuals as f32).ceil() as usize;

        // Find all crystal visual children and despawn those beyond visible_count
        for &child in children {
            if let Ok((entity, visual)) = crystal_visual_query.get(child)
                && visual.index >= visible_count {
                    commands.entity(entity).despawn();
                }
        }
    }
}

fn toggle_debug_overlay(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut overlay: ResMut<DebugOverlay>,
    mut query: Query<&mut Visibility, With<DebugOutline>>,
) {
    if keyboard.just_pressed(KeyCode::F1) {
        overlay.0 = !overlay.0;
        let vis = if overlay.0 { Visibility::Visible } else { Visibility::Hidden };
        for mut v in &mut query {
            *v = vis;
        }
    }
}

