use bevy::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::mesh::{Indices, PrimitiveTopology};

use crate::ui::GameCamera;

// Hex grid constants
const HEX_WIDTH: f32 = 128.0;
const HEX_HEIGHT: f32 = HEX_WIDTH * 0.866025404; // width * sqrt(3)/2
const HEX_RADIUS: f32 = HEX_WIDTH / 2.0;
const OUTLINE_WIDTH: f32 = 8.0;

// Components
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
pub struct ObstacleOutline;

#[derive(Component)]
pub struct LaunchPadOutline;

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
        obstacles.positions.insert((2, 0));
        obstacles.positions.insert((0, 2));
        obstacles.positions.insert((-2, 0));

        app.insert_resource(HexMapConfig { map_radius: 5 })
            .insert_resource(HoveredHex::default())
            .insert_resource(obstacles)
            .add_systems(Startup, setup_hex_map)
            .add_systems(Update, (hex_hover_system, update_outline_colors));
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

fn create_hexagon_outline_mesh() -> Mesh {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    let y = 0.0;
    let outer_radius = HEX_RADIUS + (OUTLINE_WIDTH / 2.0);
    let inner_radius = HEX_RADIUS - (OUTLINE_WIDTH / 2.0);

    // Create 6 pairs of vertices around the hexagon
    for i in 0..6 {
        let angle = std::f32::consts::PI / 3.0 * i as f32;
        let cos = angle.cos();
        let sin = angle.sin();

        let outer_x = outer_radius * cos;
        let outer_z = outer_radius * sin;
        let inner_x = inner_radius * cos;
        let inner_z = inner_radius * sin;

        // Outer vertex
        positions.push([outer_x, y, outer_z]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([1.0, 0.0]);

        // Inner vertex
        positions.push([inner_x, y, inner_z]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.0, 0.0]);
    }

    // Create triangles
    for i in 0..6 {
        let next_i = (i + 1) % 6;

        let outer_current = (i * 2) as u32;
        let inner_current = (i * 2 + 1) as u32;
        let outer_next = (next_i * 2) as u32;
        let inner_next = (next_i * 2 + 1) as u32;

        // First triangle
        indices.push(outer_current);
        indices.push(inner_current);
        indices.push(outer_next);

        // Second triangle
        indices.push(outer_next);
        indices.push(inner_current);
        indices.push(inner_next);
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
    config: Res<HexMapConfig>,
    obstacles: Res<Obstacles>,
) {
    // Spawn 3D camera with orthographic projection
    let mut orthographic = OrthographicProjection::default_3d();
    orthographic.scale = 0.8;

    commands.spawn((
        Camera3d::default(),
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

    let prism_height = 5.0;

    // Reuse meshes
    let hex_mesh = meshes.add(create_hexagon_prism_mesh(prism_height));
    let outline_mesh = meshes.add(create_hexagon_outline_mesh());

    for q in -config.map_radius..=config.map_radius {
        let r1 = (-config.map_radius).max(-q - config.map_radius);
        let r2 = config.map_radius.min(-q + config.map_radius);

        for r in r1..=r2 {
            let height = prism_height;
            let world_pos = axial_to_world_pos(q, r);

            let is_obstacle = obstacles.positions.contains(&(q, r));
            let is_launch_pad = (q == -2 && r == 1) || (q == -1 && r == 1) || (q == -1 && r == 0);

            let color = if is_obstacle {
                Color::srgb(1.0, 0.0, 0.0)
            } else {
                Color::srgb(0.0, 0.0, 0.0)
            };

            let hex_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
            let hex_entity = commands.spawn((
                Mesh3d(hex_mesh.clone()),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: color,
                    unlit: is_obstacle,
                    ..default()
                })),
                Transform::from_translation(world_pos).with_rotation(hex_rotation),
                HexTile { q, r, _height: height },
                Name::new(format!("Hex ({}, {})", q, r)),
            )).id();

            // Spawn hex outline
            let base_outline_height = prism_height + 0.5;
            let outline_pos = if is_launch_pad {
                world_pos + Vec3::new(0.0, base_outline_height + 0.2, 0.0)
            } else if is_obstacle {
                world_pos + Vec3::new(0.0, base_outline_height + 0.1, 0.0)
            } else {
                world_pos + Vec3::new(0.0, base_outline_height, 0.0)
            };

            let outline_color = if is_obstacle {
                Color::srgb(1.0, 0.0, 0.0)
            } else {
                Color::srgb(0.5, 0.5, 0.5)
            };

            let outline_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

            if is_obstacle {
                commands.spawn((
                    Mesh3d(outline_mesh.clone()),
                    MeshMaterial3d(materials.add(StandardMaterial {
                        base_color: outline_color,
                        unlit: true,
                        ..default()
                    })),
                    Transform::from_translation(outline_pos).with_rotation(outline_rotation),
                    HexOutline { hex_entity },
                    ObstacleOutline,
                ));
            } else if is_launch_pad {
                commands.spawn((
                    Mesh3d(outline_mesh.clone()),
                    MeshMaterial3d(materials.add(StandardMaterial {
                        base_color: Color::srgb(0.2, 0.6, 1.0),
                        emissive: Color::srgb(0.2, 0.6, 1.0).into(),
                        unlit: true,
                        ..default()
                    })),
                    Transform::from_translation(outline_pos).with_rotation(outline_rotation),
                    HexOutline { hex_entity },
                    LaunchPadOutline,
                ));
            } else {
                commands.spawn((
                    Mesh3d(outline_mesh.clone()),
                    MeshMaterial3d(materials.add(StandardMaterial {
                        base_color: outline_color,
                        unlit: true,
                        ..default()
                    })),
                    Transform::from_translation(outline_pos).with_rotation(outline_rotation),
                    HexOutline { hex_entity },
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
}

fn hex_hover_system(
    camera_query: Query<(&Camera, &GlobalTransform)>,
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
                if hovered_hex.entity != Some(entity) {
                    let obstacle_marker = if obstacles.positions.contains(&(q, r)) {
                        " [OBSTACLE]"
                    } else {
                        ""
                    };
                    println!("Hovering: ({}, {}){}", q, r, obstacle_marker);
                }
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
        (&HexOutline, &MeshMaterial3d<StandardMaterial>, &mut Transform),
        (Without<ObstacleOutline>, Without<LaunchPadOutline>),
    >,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    for (outline, material_handle, mut transform) in &mut outline_query {
        if let Some(material) = materials.get_mut(&material_handle.0) {
            let is_hovered = hovered_hex.entity == Some(outline.hex_entity);

            if is_hovered {
                material.base_color = Color::srgb(1.0, 1.0, 1.0);
                transform.scale = Vec3::splat(1.05);
            } else {
                material.base_color = Color::srgb(0.5, 0.5, 0.5);
                transform.scale = Vec3::splat(1.0);
            }
        }
    }
}
