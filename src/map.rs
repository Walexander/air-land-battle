use std::f32::consts::PI;

use bevy::light::CascadeShadowConfigBuilder;
use bevy::light::light_consts::lux;
use bevy::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::mesh::{Indices, PrimitiveTopology};

use crate::ui::{CameraSettings, GameCamera};
use crate::launch_pads::{LaunchPadOwner, LaunchPadOwnership, GameTimer, GameState};
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
pub struct SiloRoot;

#[derive(Component)]
pub struct SiloCover;

#[derive(Component)]
pub struct SiloMissile {
    base_translation: Vec3,
    base_rotation: Quat,
}

#[derive(Default, PartialEq, Clone, Copy)]
enum MissilePhase {
    #[default]
    Rising,
    Flying,
    Done,
}

#[derive(Component, Default)]
struct MissileLaunch {
    phase: MissilePhase,
    elapsed: f32,
    launch_world_pos: Vec3,
    launch_world_rot: Quat,
    target_pos: Vec3,
}

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

/// Hex cells currently visible to the Red (player) army.
/// Updated every frame by `update_fog_of_war`. Used by pathfinding to
/// avoid routing around enemy units that are hidden in fog.
#[derive(Resource, Default)]
pub struct VisibleHexes(pub std::collections::HashSet<(i32, i32)>);

/// Shared material handle for all fog-of-war overlay tiles.
/// Stored as a resource so bevy_inspector_egui can find it by name.
#[derive(Resource)]
pub struct FogMaterial(pub Handle<StandardMaterial>);

pub struct MapPlugin;

impl Plugin for MapPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(HexMapConfig::default())
            .insert_resource(HoveredHex::default())
            .insert_resource(Obstacles::default())
            .insert_resource(VisibleHexes::default())
            .insert_resource(DebugOverlay::default())
            .insert_resource(ClearColor(Color::srgb(0.53, 0.81, 0.92))) // Light sky blue
            .add_systems(OnEnter(LoadingState::Playing), (setup_hex_map, crate::hex_pathfinding::setup_hex_pathfinding).chain())
            .add_systems(Update, (hex_hover_system, update_outline_colors, update_launch_pad_colors, billboard_sprites, apply_crystal_materials, animate_crystal_sparkle, update_fog_of_war, update_crystal_visuals, toggle_debug_overlay, tag_silo_missiles, tag_silo_covers, rotate_silo_missiles, trigger_missile_launch, debug_trigger_missile_launch, animate_missile_launch, check_missile_animation_complete).run_if(in_state(LoadingState::Playing)));
    }
}

pub fn axial_to_world_pos(q: i32, r: i32) -> Vec3 {
    // Pointy-top hex coordinates
    let x = HEX_HEIGHT * (q as f32 + r as f32 * 0.5);
    let z = HEX_WIDTH * 0.75 * r as f32;
    Vec3::new(x, 0.0, z)
}

pub fn world_pos_to_axial(x: f32, z: f32) -> (i32, i32) {
    let r = z / (HEX_WIDTH * 0.75);
    let q = (x / HEX_HEIGHT) - (r * 0.5);
    (q.round() as i32, r.round() as i32)
}

/// Converts world position to axial cell with a small hysteresis dead-zone.
/// Requires the unit's current cell (prev_q, prev_r) so we only cross a
/// cell boundary when clearly past it, preventing floating-point oscillation
/// near exact boundary positions.  The threshold is intentionally tiny (3%)
/// to avoid false conflicts with units that legitimately enter the old cell.
pub fn world_pos_to_axial_hysteresis(x: f32, z: f32, prev_q: i32, prev_r: i32) -> (i32, i32) {
    const HYSTERESIS: f32 = 0.03;

    let r_frac = z / (HEX_WIDTH * 0.75);
    let final_r = if (r_frac - prev_r as f32).abs() > 0.5 + HYSTERESIS {
        r_frac.round() as i32
    } else {
        prev_r
    };

    // Recompute q in the frame of the committed row.
    let q_frac = (x / HEX_HEIGHT) - (final_r as f32 * 0.5);
    let final_q = if (q_frac - prev_q as f32).abs() > 0.5 + HYSTERESIS {
        q_frac.round() as i32
    } else {
        prev_q
    };

    (final_q, final_r)
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

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
        .with_inserted_indices(indices);
    let _ = mesh.generate_tangents();
    mesh
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
    // Proper polygon offset: shift each edge inward by INSET along its own perpendicular,
    // then find corner vertices by intersecting adjacent shifted edge lines.
    const INSET: f32 = 10.0;
    const HALF_THICK: f32 = 3.0;
    const EPS: f32 = 2.0; // endpoint matching tolerance

    let n = perimeter_edges.len();
    if n == 0 {
        return Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default());
    }

    let half_len = HEX_RADIUS / 2.0;

    // Per-edge geometry
    let mut mids      = vec![Vec3::ZERO; n];
    let mut perp_dirs = vec![Vec3::ZERO; n]; // direction along the hex edge
    let mut inward    = vec![Vec3::ZERO; n]; // perpendicular, pointing toward pad interior
    let mut ep_a      = vec![Vec3::ZERO; n]; // one endpoint of the edge segment
    let mut ep_b      = vec![Vec3::ZERO; n]; // other endpoint

    for (i, (cell1, cell2)) in perimeter_edges.iter().enumerate() {
        let pos1 = axial_to_world_pos(cell1.0, cell1.1);
        let pos2 = axial_to_world_pos(cell2.0, cell2.1);
        let mid = Vec3::new((pos1.x + pos2.x) * 0.5, 0.0, (pos1.z + pos2.z) * 0.5);
        let ev_raw = Vec3::new(pos2.x - pos1.x, 0.0, pos2.z - pos1.z);
        let ev_len = ev_raw.length();
        if ev_len < 0.001 { continue; }
        let ev = ev_raw / ev_len;
        let perp = Vec3::new(-ev.z, 0.0, ev.x);
        // Inward normal: whichever of ±ev points toward the pad center
        let inw = if ev.dot(pad_center - mid) >= 0.0 { ev } else { -ev };
        mids[i]      = mid;
        perp_dirs[i] = perp;
        inward[i]    = inw;
        ep_a[i]      = mid + perp * half_len;
        ep_b[i]      = mid - perp * half_len;
    }

    // Intersect two lines (XZ plane): p1 + t*d1 = p2 + s*d2 → returns intersection point
    let line_isect = |p1: Vec3, d1: Vec3, p2: Vec3, d2: Vec3| -> Vec3 {
        let cross = d1.x * d2.z - d1.z * d2.x;
        if cross.abs() < 1e-6 { return (p1 + p2) * 0.5; }
        let dp = p2 - p1;
        let t = (dp.x * d2.z - dp.z * d2.x) / cross;
        p1 + d1 * t
    };

    // For edge i at endpoint ep, find the neighbor edge that shares that endpoint
    let find_neighbor = |i: usize, ep: Vec3| -> Option<usize> {
        for j in 0..n {
            if j == i { continue; }
            if ep.distance(ep_a[j]) < EPS || ep.distance(ep_b[j]) < EPS {
                return Some(j);
            }
        }
        None
    };

    let mut positions: Vec<[f32; 3]> = Vec::new();
    let mut normals:   Vec<[f32; 3]> = Vec::new();
    let mut uvs:       Vec<[f32; 2]> = Vec::new();
    let mut indices:   Vec<u32>      = Vec::new();

    // Compute miter-corrected corner vertices for each (edge, endpoint) pair.
    // Using the bisector of the two adjacent inward normals ensures both quads
    // that share a corner land on exactly the same outer/inner vertex.
    let miter_corner = |i: usize, ep: Vec3, sign: f32| -> (Vec3, Vec3) {
        let inset_mid_i = mids[i] + inward[i] * INSET;
        let center = match find_neighbor(i, ep) {
            Some(j) => {
                let inset_mid_j = mids[j] + inward[j] * INSET;
                line_isect(inset_mid_i, perp_dirs[i], inset_mid_j, perp_dirs[j])
            }
            None => inset_mid_i + perp_dirs[i] * (half_len * sign),
        };
        let bisector = match find_neighbor(i, ep) {
            Some(j) => {
                let b = (inward[i] + inward[j]).normalize_or_zero();
                // Miter length: HALF_THICK / cos(angle/2) = HALF_THICK / dot(bisector, inward[i])
                let cos_half = b.dot(inward[i]).max(0.2); // clamp to avoid blowup
                let miter_len = HALF_THICK / cos_half;
                (b, miter_len)
            }
            None => (inward[i], HALF_THICK),
        };
        let outer = center - bisector.0 * bisector.1;
        let inner = center + bisector.0 * bisector.1;
        (outer - pad_center, inner - pad_center)
    };

    for i in 0..n {
        let (outer_a, inner_a) = miter_corner(i, ep_a[i],  1.0);
        let (outer_b, inner_b) = miter_corner(i, ep_b[i], -1.0);

        let base = positions.len() as u32;
        positions.push([outer_a.x, 0.0, outer_a.z]);
        positions.push([inner_a.x, 0.0, inner_a.z]);
        positions.push([outer_b.x, 0.0, outer_b.z]);
        positions.push([inner_b.x, 0.0, inner_b.z]);
        for _ in 0..4 {
            normals.push([0.0, 1.0, 0.0]);
            uvs.push([0.0, 0.0]);
        }
        indices.extend_from_slice(&[base, base+1, base+2, base+1, base+3, base+2]);
    }

    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
        .with_inserted_indices(Indices::U32(indices))
}

/// Shared mesh/texture/model handles bundled so helpers don't need a dozen individual parameters.
struct TileAssets {
    filled_hex_mesh: Handle<Mesh>,
    hex_border_mesh: Handle<Mesh>,
    fog_hex_mesh: Handle<Mesh>,
    hover_outline_mesh: Handle<Mesh>,
    spawn_center_mesh: Handle<Mesh>,
    sand_texture: Handle<Image>,
    cement_texture: Handle<Image>,
    fog_material: Handle<StandardMaterial>,
    mountain_model: Handle<Scene>,
    silo_model: Handle<Scene>,
    hq_model: Handle<Scene>,
}

pub fn setup_hex_map(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    asset_server: Res<AssetServer>,
    mut obstacles: ResMut<Obstacles>,
    mut map_config: ResMut<HexMapConfig>,
    mut camera_settings: ResMut<CameraSettings>,
    map_def: Res<crate::map_loader::MapDefinition>,
) {
    // Create the shared fog material once and store it as a named resource
    // so bevy_inspector_egui can find it under "FogMaterial" in the Resources panel.
    let fog_material_handle = materials.add(StandardMaterial {
        base_color: Color::srgba(0.0, 0.0, 0.0, 0.55),
        alpha_mode: AlphaMode::Blend,
        unlit: true,

        double_sided: true,
        cull_mode: None,
        ..default()
    });
    commands.insert_resource(FogMaterial(fog_material_handle.clone()));
    // Populate obstacles from the loaded map definition
    obstacles.positions = map_def.obstacles.clone();
    // Build valid_cells from all rendered tiles (tile_map + launch_pad_cells).
    map_config.valid_cells = map_def.tile_map.keys().cloned()
        .chain(map_def.launch_pad_cells.iter().cloned())
        .collect();
    // Compute map center from all valid cells to center the camera.
    let mut min_x = f32::INFINITY;
    let mut max_x = f32::NEG_INFINITY;
    let mut min_z = f32::INFINITY;
    let mut max_z = f32::NEG_INFINITY;
    for &(q, r) in &map_config.valid_cells {
        let wx = HEX_HEIGHT * (q as f32 + r as f32 * 0.5);
        let wz = HEX_WIDTH * 0.75 * r as f32;
        min_x = min_x.min(wx);
        max_x = max_x.max(wx);
        min_z = min_z.min(wz);
        max_z = max_z.max(wz);
    }
    let center_x = (min_x + max_x) * 0.5;
    let center_z = (min_z + max_z) * 0.5;
    info!("Camera: center=({center_x:.1},{center_z:.1}) map_span=({:.0}x{:.0})", max_x - min_x, max_z - min_z);

    // Push the computed center into CameraSettings so update_camera_from_settings uses it.
    camera_settings.x = center_x;
    camera_settings.z = center_z + 350.0;
    camera_settings.look_at_x = center_x;
    camera_settings.look_at_z = center_z;
    camera_settings.home_x = center_x;
    camera_settings.home_z = center_z;

    // Spawn 3D camera with orthographic projection
    let mut orthographic = OrthographicProjection::default_3d();
    orthographic.scale = camera_settings.scale;
    // Increase far plane to prevent clipping when camera pans
    orthographic.far = 2000.0;

    commands.spawn((
        Camera3d::default(),
        Camera {
            order: 0,
            ..default()
        },
        Projection::Orthographic(orthographic),
        Transform::from_xyz(center_x, 300.0, center_z + 500.0)
            .looking_at(Vec3::new(center_x, 0.0, center_z), Vec3::Y),
        GameCamera,
        DespawnOnExit(LoadingState::Playing),
    ));

    // Add point light
    commands.spawn((
        DirectionalLight {
            illuminance: 8000.0,
            shadows_enabled: true,
            shadow_depth_bias: 0.0,
            shadow_normal_bias: 0.0,
            ..default()
        },
        // Nearly overhead, slight left lean — shadows fall right and slightly down
        Transform::from_xyz(-0.4, 4.0, -0.3).looking_at(Vec3::ZERO, Vec3::Y),
        CascadeShadowConfigBuilder {
            num_cascades: 1,
            maximum_distance: 1200.0,
            ..default()
        }
        .build(),
        DespawnOnExit(LoadingState::Playing),
    ));

    // Add ambient light for fill/shadow softness
    commands.spawn((
        AmbientLight {
            color: Color::srgb(1.0, 1.0, 1.0),
            brightness: 1200.0,
            affects_lightmapped_meshes: false,
        },
        DespawnOnExit(LoadingState::Playing),
    ));

    let prism_height = 20.0;

    // Build shared handles bundled into TileAssets.
    let _hex_mesh = meshes.add(create_hexagon_prism_mesh(prism_height));
    let assets = TileAssets {
        filled_hex_mesh: meshes.add(create_filled_hexagon_mesh()),
        hex_border_mesh: meshes.add(create_filled_hexagon_border_mesh()),
        fog_hex_mesh: meshes.add(create_filled_hexagon_border_mesh()), // same size as the border
        hover_outline_mesh: meshes.add(create_hexagon_outline_mesh(63.0, 4.0)), // Same as destination ring
        spawn_center_mesh: meshes.add(create_filled_hexagon_mesh_with_radius(HEX_RADIUS * 0.45)),
        sand_texture: asset_server.load("maps/Tiles/sand_01.png"),
        cement_texture: asset_server.load("maps/Tiles/cement_01.jpg"),
        fog_material: fog_material_handle,
        mountain_model: asset_server.load("mountains.glb#Scene0"),
        silo_model: asset_server.load("Missile Silo.glb#Scene0"),
        hq_model: asset_server.load("JustBuildings.glb#Scene0"),
    };

    // Create parent HexMap entity; delegate children to helpers.
    commands.spawn((
        HexMap,
        Transform::default(),
        Visibility::default(),
        Name::new("HexMap"),
        DespawnOnExit(LoadingState::Playing),
    )).with_children(|parent| {
        setup_tiles(parent, &map_def, &assets, &mut *materials, &mut *meshes, &obstacles);
        setup_hq(parent, &map_def, &assets, &mut *materials);
        setup_obstacles(parent, &map_def, &assets, &mut *materials);
        setup_crystals(parent, &map_def, &asset_server);
    });
}

/// Draws all non-obstacle hex tiles from the tile_map + launch_pad_cells.
/// For each tile: border ring, filled hex, fog of war, hover outline, optional spawn dot,
/// optional launch-pad outline (first cell only), and debug outlines.
/// Also emits the spawn-cell debug outlines and polygon outlines at the end.
fn setup_tiles(
    parent: &mut bevy::ecs::hierarchy::ChildSpawnerCommands,
    map_def: &crate::map_loader::MapDefinition,
    assets: &TileAssets,
    materials: &mut Assets<StandardMaterial>,
    meshes: &mut Assets<Mesh>,
    obstacles: &Obstacles,
) {
    // Merge tile_map with any launch-pad cells not already present.
    let mut tile_map = map_def.tile_map.clone();
    for &cell in &map_def.launch_pad_cells {
        tile_map.entry(cell).or_insert(0);
    }

    let prism_height = 20.0;
    let hex_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
    let outline_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

    for (&(q, r), &_gid) in &tile_map {
        let world_pos = axial_to_world_pos(q, r);
        let height = prism_height;

        let is_obstacle = obstacles.positions.contains(&(q, r));

        // A tile belongs to a launch pad if the polygon step assigned it to a group,
        // regardless of its GID (the polygon is authoritative).
        let pad_index = map_def.launch_pads.iter().position(|platform| platform.contains(&(q, r)));
        let is_launch_pad = pad_index.is_some();

        let is_spawn = map_def.spawn_red.contains(&(q, r))
            || map_def.spawn_blue.contains(&(q, r));

        let wx = HEX_HEIGHT * (q as f32 + r as f32 * 0.5);
        let wz = HEX_WIDTH * 0.75 * r as f32;
        let is_red_base = map_def.base_red_polygon.len() >= 3
            && crate::map_loader::point_in_polygon(wx, wz, &map_def.base_red_polygon);
        let is_blue_base = map_def.base_blue_polygon.len() >= 3
            && crate::map_loader::point_in_polygon(wx, wz, &map_def.base_blue_polygon);

        // Tile fill color.
        let color = if is_red_base {
            Color::srgb(0.75, 0.2, 0.2)
        } else if is_blue_base {
            Color::srgb(0.2, 0.35, 0.75)
        } else if is_launch_pad {
            Color::WHITE
        } else {
            Color::WHITE
        };
        let emissive = if is_red_base || is_blue_base {
            LinearRgba::from(color) * 0.5
        } else {
            LinearRgba::BLACK
        };

        // --- Border ring ---
        let border_pos = world_pos + Vec3::new(0.0, 0.4, 0.0);
        parent.spawn((
            Mesh3d(assets.hex_border_mesh.clone()),
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

        // --- Filled hexagon ---
        let filled_hex_pos = world_pos + Vec3::new(0.0, 0.5, 0.0);
        let mut hex_entity_commands = parent.spawn((
            Mesh3d(assets.filled_hex_mesh.clone()),
            MeshMaterial3d(materials.add(StandardMaterial {
                base_color: color,
                base_color_texture: Some(if is_launch_pad {
                    assets.cement_texture.clone()
                } else {
                    assets.sand_texture.clone()
                }),
                emissive,
                unlit: false,
                perceptual_roughness: 1.0,
                metallic: 0.0,
                double_sided: true,
                cull_mode: None,
                ..default()
            })),
            bevy::light::NotShadowCaster,
            Transform::from_translation(filled_hex_pos).with_rotation(hex_rotation),
            HexTile { q, r, _height: height },
            Name::new(format!("Hex ({}, {})", q, r)),
        ));

        // Add LaunchPadTile component if this is a launch pad.
        if pad_index.is_some() {
            hex_entity_commands.insert(LaunchPadTile {});
        }

        // --- White centre dot for spawn tiles ---
        if is_spawn {
            parent.spawn((
                Mesh3d(assets.spawn_center_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: Color::srgb(1.0, 1.0, 1.0),
                    emissive: Color::srgb(1.0, 1.0, 1.0).into(),
                    unlit: true,
                    double_sided: true,
                    cull_mode: None,
                    ..default()
                })),
                Transform::from_translation(filled_hex_pos + Vec3::new(0.0, 0.2, 0.0))
                    .with_rotation(hex_rotation),
            ));
        }

        // Unused outline position variables kept for parity with original.
        let base_outline_height = 1.0;
        let _outline_pos = if is_launch_pad {
            world_pos + Vec3::new(0.0, base_outline_height + 0.2, 0.0)
        } else {
            world_pos + Vec3::new(0.0, base_outline_height, 0.0)
        };
        let _outline_color = Color::srgb(1.0, 1.0, 1.0); // White

        // --- Launch-pad perimeter outline (first cell of each pad only) ---
        if !is_obstacle {
            if let Some(pad_idx) = pad_index {
                let pad_cells = &map_def.launch_pads[pad_idx];
                if pad_cells.first() == Some(&(q, r)) {
                    spawn_launch_pad_outline(parent, pad_idx, pad_cells, materials, meshes);
                }
            }
        }

        // --- Hover highlight and fog (non-obstacle tiles only) ---
        if !is_obstacle {
            let hover_pos = world_pos + Vec3::new(0.0, 8.0, 0.0);
            let hover_color = Color::srgb(0.7, 0.7, 0.7);
            parent.spawn((
                Mesh3d(assets.hover_outline_mesh.clone()),
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
                    .with_scale(Vec3::splat(0.75)),
                HexOutline {},
                Visibility::Hidden,
            ));

            // Fog sits just above the colored tile (Y+0.51 vs tile Y+0.5).
            let fog_pos = world_pos + Vec3::new(0.0, 0.51, 0.0);
            parent.spawn((
                Name::new("FogOfWar"),
                Mesh3d(assets.fog_hex_mesh.clone()),
                MeshMaterial3d(assets.fog_material.clone()),
                Transform::from_translation(fog_pos).with_rotation(hex_rotation),
                FogOfWar { hex_q: q, hex_r: r },
                Visibility::Visible,
            ));
        }

        // --- Debug outline (obstacles → black, crystal fields → purple) ---
        let debug_color = if is_obstacle {
            Some(Color::srgb(0.0, 0.0, 0.0))
        } else if map_def.crystal_fields.contains(&(q, r)) {
            Some(Color::srgb(0.6, 0.0, 0.8))
        } else {
            None
        };
        if let Some(debug_color) = debug_color {
            parent.spawn((
                Mesh3d(assets.hover_outline_mesh.clone()),
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

    // --- Debug outlines for spawn cells (raised to y=11, above tile outlines at y=9) ---
    info!("debug: spawn_red={} spawn_blue={}", map_def.spawn_red.len(), map_def.spawn_blue.len());
    for &(q, r) in &map_def.spawn_red {
        let world_pos = axial_to_world_pos(q, r);
        parent.spawn((
            Mesh3d(assets.hover_outline_mesh.clone()),
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
            Mesh3d(assets.hover_outline_mesh.clone()),
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

    // --- Launch pad polygon outlines (black, toggled by F1 debug overlay) ---
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

    // --- Base polygon outlines (toggled by F1 debug overlay) ---
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
}

/// Builds the perimeter outline mesh for a single launch pad and spawns it.
/// Extracted from the tile loop to reduce nesting; called only for the first cell of each pad.
fn spawn_launch_pad_outline(
    parent: &mut bevy::ecs::hierarchy::ChildSpawnerCommands,
    pad_idx: usize,
    pad_cells: &[(i32, i32)],
    materials: &mut Assets<StandardMaterial>,
    meshes: &mut Assets<Mesh>,
) {
    use std::collections::HashMap;

    type Edge = ((i32, i32), (i32, i32));
    let mut edge_counts: HashMap<Edge, usize> = HashMap::new();

    for &(cell_q, cell_r) in pad_cells {
        let neighbors = [
            (cell_q + 1, cell_r),
            (cell_q + 1, cell_r - 1),
            (cell_q, cell_r - 1),
            (cell_q - 1, cell_r),
            (cell_q - 1, cell_r + 1),
            (cell_q, cell_r + 1),
        ];
        for neighbor in neighbors {
            let edge = if (cell_q, cell_r) < neighbor {
                ((cell_q, cell_r), neighbor)
            } else {
                (neighbor, (cell_q, cell_r))
            };
            *edge_counts.entry(edge).or_insert(0) += 1;
        }
    }

    let perimeter_edges: Vec<Edge> = edge_counts
        .into_iter()
        .filter(|(_, count)| *count == 1)
        .map(|(edge, _)| edge)
        .collect();

    let mut center_x = 0.0f32;
    let mut center_z = 0.0f32;
    for &(cell_q, cell_r) in pad_cells {
        let pos = axial_to_world_pos(cell_q, cell_r);
        center_x += pos.x;
        center_z += pos.z;
    }
    center_x /= pad_cells.len() as f32;
    center_z /= pad_cells.len() as f32;
    let pad_center = Vec3::new(center_x, 0.0, center_z);

    println!("Launch pad {} has {} perimeter edges", pad_idx, perimeter_edges.len());
    if !perimeter_edges.is_empty() {
        let outline_mesh = create_launch_pad_outline_mesh(&perimeter_edges, pad_center);
        let outline_mesh_handle = meshes.add(outline_mesh);
        let outline_y = 1.2;

        parent.spawn((
            Mesh3d(outline_mesh_handle),
            MeshMaterial3d(materials.add(StandardMaterial {
                base_color: Color::srgb(0.7, 0.7, 0.7),
                emissive: Color::srgb(0.7, 0.7, 0.7).into(),
                unlit: true,
                double_sided: true,
                cull_mode: None,
                ..default()
            })),
            Transform::from_translation(Vec3::new(pad_center.x, outline_y, pad_center.z)),
            LaunchPadOutline { pad_index: pad_idx },
        ));
    }
}

/// Draws tile floor (border + filled hex) and HQ model for each HQ position.
/// HQ tiles may or may not be present in tile_map; this handles both cases uniformly.
fn setup_hq(
    parent: &mut bevy::ecs::hierarchy::ChildSpawnerCommands,
    map_def: &crate::map_loader::MapDefinition,
    assets: &TileAssets,
    materials: &mut Assets<StandardMaterial>,
) {
    let hq_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

    let hq_entries: &[(Option<(i32, i32)>, crate::units::Army)] = &[
        (map_def.hq_red, crate::units::Army::Red),
        (map_def.hq_blue, crate::units::Army::Blue),
    ];

    for &(pos_opt, army) in hq_entries {
        let Some((q, r)) = pos_opt else { continue };

        let world_pos = axial_to_world_pos(q, r);
        let wx = HEX_HEIGHT * (q as f32 + r as f32 * 0.5);
        let wz = HEX_WIDTH * 0.75 * r as f32;

        let is_red_base = map_def.base_red_polygon.len() >= 3
            && crate::map_loader::point_in_polygon(wx, wz, &map_def.base_red_polygon);
        let is_blue_base = map_def.base_blue_polygon.len() >= 3
            && crate::map_loader::point_in_polygon(wx, wz, &map_def.base_blue_polygon);

        let tile_color = if is_red_base {
            Color::srgb(0.75, 0.2, 0.2)
        } else if is_blue_base {
            Color::srgb(0.2, 0.35, 0.75)
        } else {
            Color::WHITE
        };
        let emissive = if is_red_base || is_blue_base {
            LinearRgba::from(tile_color) * 0.5
        } else {
            LinearRgba::BLACK
        };

        // Border ring
        parent.spawn((
            Mesh3d(assets.hex_border_mesh.clone()),
            MeshMaterial3d(materials.add(StandardMaterial {
                base_color: Color::srgb(0.7, 0.7, 0.7),
                emissive: Color::srgb(0.7, 0.7, 0.7).into(),
                unlit: true,
                double_sided: true,
                cull_mode: None,
                ..default()
            })),
            Transform::from_translation(world_pos + Vec3::new(0.0, 0.4, 0.0))
                .with_rotation(hq_rotation),
        ));

        // Filled hex floor
        parent.spawn((
            Mesh3d(assets.filled_hex_mesh.clone()),
            MeshMaterial3d(materials.add(StandardMaterial {
                base_color: tile_color,
                base_color_texture: Some(assets.sand_texture.clone()),
                emissive,
                unlit: false,
                perceptual_roughness: 1.0,
                metallic: 0.0,
                double_sided: true,
                cull_mode: None,
                ..default()
            })),
            bevy::light::NotShadowCaster,
            Transform::from_translation(world_pos + Vec3::new(0.0, 0.5, 0.0))
                .with_rotation(hq_rotation),
        ));

        // HQ model
        parent.spawn((
            SceneRoot(assets.hq_model.clone()),
            Transform::from_translation(world_pos + Vec3::new(0.0, 10.0, 0.0))
                .with_rotation(hq_rotation)
                .with_scale(Vec3::splat(24.0)),
            HQ { army, q, r },
            Name::new(format!("{:?} HQ", army)),
        ));

        println!("Creating {:?} HQ at ({}, {})", army, q, r);
    }
}

/// Draws tile floor + silo/mountain model for every obstacle that is not an HQ.
/// Skips positions already rendered by setup_tiles (those in tile_map).
/// Also spawns black debug outlines for each obstacle.
fn setup_obstacles(
    parent: &mut bevy::ecs::hierarchy::ChildSpawnerCommands,
    map_def: &crate::map_loader::MapDefinition,
    assets: &TileAssets,
    materials: &mut Assets<StandardMaterial>,
) {
    let obstacle_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

    for &(q, r) in &map_def.obstacles {
        // HQ positions are handled entirely by setup_hq.
        if map_def.hq_red == Some((q, r)) || map_def.hq_blue == Some((q, r)) {
            continue;
        }

        let world_pos = axial_to_world_pos(q, r);
        let wx = HEX_HEIGHT * (q as f32 + r as f32 * 0.5);
        let wz = HEX_WIDTH * 0.75 * r as f32;

        let is_silo = map_def.silos.contains(&(q, r));
        let is_red_base = map_def.base_red_polygon.len() >= 3
            && crate::map_loader::point_in_polygon(wx, wz, &map_def.base_red_polygon);
        let is_blue_base = map_def.base_blue_polygon.len() >= 3
            && crate::map_loader::point_in_polygon(wx, wz, &map_def.base_blue_polygon);

        let tile_color = if is_red_base {
            Color::srgb(0.75, 0.2, 0.2)
        } else if is_blue_base {
            Color::srgb(0.2, 0.35, 0.75)
        } else {
            Color::WHITE
        };

        // Tile floor
        parent.spawn((
            Mesh3d(assets.filled_hex_mesh.clone()),
            MeshMaterial3d(materials.add(StandardMaterial {
                base_color: tile_color,
                base_color_texture: if is_silo { Some(assets.cement_texture.clone()) } else { None },
                unlit: false,
                perceptual_roughness: 1.0,
                double_sided: true,
                cull_mode: None,
                ..default()
            })),
            Transform::from_translation(world_pos + Vec3::new(0.0, 0.5, 0.0))
                .with_rotation(obstacle_rotation),
        ));

        if !is_red_base && !is_blue_base {
            if is_silo {
                parent.spawn((
                    SceneRoot(assets.silo_model.clone()),
                    Transform::from_translation(world_pos + Vec3::new(0.0, 0.5, 0.0))
                        .with_rotation(Quat::from_rotation_y(std::f32::consts::PI / 2.0))
                        .with_scale(Vec3::splat(20.0)),
                    SiloRoot,
                ));
            } else {
                parent.spawn((
                    SceneRoot(assets.mountain_model.clone()),
                    Transform::from_translation(world_pos + Vec3::new(0.0, 10.0, 12.0))
                        .with_rotation(Quat::from_rotation_y(std::f32::consts::PI / 2.0))
                        .with_scale(Vec3::splat(21.25)),
                ));
            }
        }

        // Debug outline (black)
        parent.spawn((
            Mesh3d(assets.hover_outline_mesh.clone()),
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
}

/// Spawns CrystalField entities with their crystal-model children.
fn setup_crystals(
    parent: &mut bevy::ecs::hierarchy::ChildSpawnerCommands,
    map_def: &crate::map_loader::MapDefinition,
    asset_server: &AssetServer,
) {
    for &(q, r) in &map_def.crystal_fields {
        let world_pos = axial_to_world_pos(q, r);

        // Load crystal model
        let crystal_scene: Handle<Scene> = asset_server.load("Lighthing Crystal.glb#Scene0");

        // Pseudo-random crystal count between 200–400 based on position.
        let crystals = 200 + (((q + r) * 73) % 201).abs();

        println!("Creating CRYSTAL FIELD at ({}, {}) with {} crystals", q, r, crystals);

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
        )).with_children(|field_parent: &mut bevy::ecs::hierarchy::ChildSpawnerCommands| {
            // 1 visual crystal per 10 crystals, clamped 1–8.
            let num_crystals = ((crystals as f32 / 10.0).ceil() as usize).max(1).min(8);

            for i in 0..num_crystals {
                let i_i32 = i as i32;
                let angle = (i as f32 * 2.5 + (q + r) as f32 * 0.7) * std::f32::consts::PI;
                let radius = 25.0 + ((i_i32 * 13 + q * 7) % 20) as f32;
                let offset_x = angle.cos() * radius;
                let offset_z = angle.sin() * radius;

                let rotation_y = (i as f32 * 1.3 + (q + r) as f32 * 0.5) * std::f32::consts::PI;
                let rotation_speed = 0.3 + (i as f32 * 0.1);
                let pulse_offset = i as f32 * 2.0;

                field_parent.spawn((
                    SceneRoot(crystal_scene.clone()),
                    Transform::from_translation(Vec3::new(offset_x, 8.0, offset_z))
                        .with_scale(Vec3::splat(4.0))
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
    mut visible_hexes_res: ResMut<VisibleHexes>,
    player_army: Res<crate::units::LocalPlayerArmy>,
) {
    use std::collections::HashSet;
    let player = player_army.0;

    let mut visible_hexes: HashSet<(i32, i32)> = HashSet::new();

    // Reveal 2-tile radius around player spawn points at start.
    let player_spawns = match player {
        crate::units::Army::Red => &map_def.spawn_red,
        crate::units::Army::Blue => &map_def.spawn_blue,
    };
    for &(sq, sr) in player_spawns {
        for dq in -2i32..=2i32 {
            for dr in -2i32..=2i32 {
                if dq.abs().max(dr.abs()).max((-dq - dr).abs()) <= 2 {
                    visible_hexes.insert((sq + dq, sr + dr));
                }
            }
        }
    }

    for (_, unit) in &unit_query {
        // Only player units reveal fog
        if unit.army != player {
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

    // Add visibility from player HQs (3-hex radius)
    for hq in &hq_query {
        if hq.army != player {
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
        // Only hide enemy units, not the player's own units
        if unit.army == player {
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
            && unit.army != player {
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

    // Publish the visible hex set for pathfinding systems to consume.
    visible_hexes_res.0 = visible_hexes;
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


fn tag_silo_missiles(
    mut commands: Commands,
    names: Query<(Entity, &Name, &Transform), Without<SiloMissile>>,
) {
    for (entity, name, transform) in &names {
        if name.as_str() == "Missile" {
            commands.entity(entity).insert(SiloMissile {
                base_translation: transform.translation,
                base_rotation: transform.rotation,
            });
        }
    }
}

fn tag_silo_covers(
    mut commands: Commands,
    names: Query<(Entity, &Name), Without<SiloCover>>,
) {
    for (entity, name) in &names {
        if name.as_str() == "Cover" {
            commands.entity(entity).insert(SiloCover);
        }
    }
}

fn rotate_silo_missiles(
    mut missiles: Query<(&mut Transform, &SiloMissile)>,
    game_timer: Res<crate::launch_pads::GameTimer>,
    time: Res<Time>,
) {
    let target_angle = match game_timer.winning_army {
        Some(crate::units::Army::Red) => 15.0_f32.to_radians(),
        Some(crate::units::Army::Blue) => -15.0_f32.to_radians(),
        None => 0.0,
    };

    let x_rot = Quat::from_rotation_x(target_angle);
    for (mut transform, missile) in &mut missiles {
        let target = missile.base_rotation * x_rot;
        transform.rotation = transform.rotation.slerp(target, time.delta_secs() * 2.0);
    }
}

fn trigger_missile_launch(
    mut commands: Commands,
    mut game_state: ResMut<GameState>,
    map_def: Res<crate::map_loader::MapDefinition>,
    untriggered: Query<Entity, (With<SiloMissile>, Without<MissileLaunch>)>,
    active: Query<&MissileLaunch>,
    _covers: Query<Entity, With<SiloCover>>,
) {
    if !game_state.game_over || game_state.missile_animation_complete { return; }

    let (hq, base_polygon) = match game_state.winner {
        Some(crate::units::Army::Red) => (map_def.hq_blue, &map_def.base_blue_polygon),
        Some(crate::units::Army::Blue) => (map_def.hq_red, &map_def.base_red_polygon),
        None => {
            game_state.missile_animation_complete = true;
            return;
        }
    };

    let target_pos = if let Some((tq, tr)) = hq {
        axial_to_world_pos(tq, tr)
    } else if base_polygon.len() >= 3 {
        let n = base_polygon.len() as f32;
        let cx = base_polygon.iter().map(|(x, _)| x).sum::<f32>() / n;
        let cz = base_polygon.iter().map(|(_, z)| z).sum::<f32>() / n;
        Vec3::new(cx, 0.0, cz)
    } else {
        game_state.missile_animation_complete = true;
        return;
    };

    let mut launched = false;
    for entity in &untriggered {
        commands.entity(entity).insert(MissileLaunch { target_pos, ..default() });
        launched = true;
    }


    if !launched && active.is_empty() {
        game_state.missile_animation_complete = true;
    }
}

fn debug_trigger_missile_launch(
    mut commands: Commands,
    keyboard: Res<ButtonInput<KeyCode>>,
    player_army: Res<crate::units::LocalPlayerArmy>,
    map_def: Res<crate::map_loader::MapDefinition>,
    untriggered: Query<Entity, (With<SiloMissile>, Without<MissileLaunch>)>,
    mut existing: Query<&mut MissileLaunch>,
) {
    if !keyboard.just_pressed(KeyCode::F3) { return; }

    let enemy_hq = match player_army.0 {
        crate::units::Army::Red => map_def.hq_blue,
        crate::units::Army::Blue => map_def.hq_red,
    };
    let target_pos = if let Some((q, r)) = enemy_hq {
        axial_to_world_pos(q, r)
    } else {
        Vec3::ZERO
    };

    for entity in &untriggered {
        commands.entity(entity).insert(MissileLaunch { target_pos, ..default() });
    }
    // If already launched, reset to Rising so it plays again
    for mut launch in &mut existing {
        launch.phase = MissilePhase::Rising;
        launch.elapsed = 0.0;
        launch.target_pos = target_pos;
    }
}

fn check_missile_animation_complete(
    mut game_state: ResMut<GameState>,
    missiles: Query<&MissileLaunch>,
) {
    if !game_state.game_over || game_state.missile_animation_complete { return; }
    if missiles.iter().all(|m| m.phase == MissilePhase::Done) && !missiles.is_empty() {
        game_state.missile_animation_complete = true;
    }
}

fn set_shadow_caster_recursive(
    commands: &mut Commands,
    entity: Entity,
    children_query: &Query<&Children>,
    cast: bool,
) {
    if cast {
        commands.entity(entity).remove::<bevy::light::NotShadowCaster>();
    } else {
        commands.entity(entity).insert(bevy::light::NotShadowCaster);
    }
    if let Ok(children) = children_query.get(entity) {
        for child in children.iter() {
            set_shadow_caster_recursive(commands, child, children_query, cast);
        }
    }
}

fn animate_missile_launch(
    mut commands: Commands,
    mut missiles: Query<(Entity, &mut Transform, &GlobalTransform, &mut MissileLaunch, &bevy::ecs::hierarchy::ChildOf, &SiloMissile)>,
    parent_transforms: Query<&GlobalTransform, Without<SiloMissile>>,
    children_query: Query<&Children>,
    time: Res<Time>,
) {
    for (entity, mut transform, global_tf, mut launch, child_of, missile) in &mut missiles {
        let dt = time.delta_secs();
        let Ok(parent_gt) = parent_transforms.get(child_of.0) else { continue; };

        match launch.phase {
            MissilePhase::Rising => {
                transform.translation.y += dt * 40.0;
                launch.elapsed += dt;
                if launch.elapsed >= 1.2 {
                    launch.launch_world_pos = global_tf.translation();
                    launch.launch_world_rot = global_tf.to_scale_rotation_translation().1;
                    launch.elapsed = 0.0;
                    launch.phase = MissilePhase::Flying;
                    set_shadow_caster_recursive(&mut commands, entity, &children_query, false);
                }
            }
            MissilePhase::Flying => {
                // First frame: missile teleports to directly above the target (off-screen).
                // Subsequent frames: falls straight down with ease-in (slow start, fast impact).
                let fall_height = 800.0; // world units above target — well above the camera
                let duration = 0.8;
                launch.elapsed = (launch.elapsed + dt).min(duration);
                let t = launch.elapsed / duration;
                let t_eased = t * t * t; // ease-in cubic: accelerates toward impact

                let world_start = launch.target_pos + Vec3::Y * fall_height;
                let world_pos = world_start.lerp(launch.target_pos, t_eased);
                let local_pos = parent_gt.affine().inverse().transform_point3(world_pos);
                transform.translation = local_pos;

                // Point nose straight down for re-entry
                let parent_rot = parent_gt.to_scale_rotation_translation().1;
                let world_rot = Quat::from_rotation_arc(Vec3::Y, Vec3::NEG_Y);
                transform.rotation = parent_rot.inverse() * world_rot;

                if launch.elapsed >= duration {
                    launch.phase = MissilePhase::Done;
                    set_shadow_caster_recursive(&mut commands, entity, &children_query, true);
                }
            }
            MissilePhase::Done => {
                transform.translation = missile.base_translation;
                transform.rotation = missile.base_rotation;
            }
        }
    }
}
