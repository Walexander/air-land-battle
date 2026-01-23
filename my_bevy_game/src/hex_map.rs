use bevy::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::mesh::{Indices, PrimitiveTopology};
use bevy::gltf::GltfAssetLabel;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering;

const HEX_WIDTH: f32 = 128.0;
const HEX_HEIGHT: f32 = HEX_WIDTH * 0.866025404; // width * sqrt(3)/2
const HEX_RADIUS: f32 = HEX_WIDTH / 2.0;
const OUTLINE_WIDTH: f32 = 4.0;

#[derive(Component)]
pub struct HexTile {
    pub q: i32,
    pub r: i32,
    pub _height: i32,
}

#[derive(Component)]
pub struct HexOutline {
    pub hex_entity: Entity,
}

#[derive(Component)]
pub struct ObstacleOutline;

#[derive(Component)]
pub struct OccupiedOutline;

#[derive(Component)]
pub struct LaunchPadOutline;

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
    pub path: Vec<(i32, i32)>, // List of (q, r) waypoints to follow
    pub current_waypoint: usize, // Index of current target waypoint
    pub progress: f32,   // 0.0 to 1.0 progress to current waypoint
    pub speed: f32,      // world units per second
}

#[derive(Component)]
pub struct Selected;

#[derive(Component)]
pub struct SelectionRing {
    pub unit_entity: Entity,
    pub animation_timer: f32, // Track animation progress
}

#[derive(Component)]
pub struct PathVisualization {
    pub unit_entity: Entity,
    pub animation_progress: f32, // 0.0 to total_path_length, tracks dark segment position
    pub loop_count: u32, // Number of times the animation has looped
}

#[derive(Component)]
pub struct DestinationRing {
    pub unit_entity: Entity,
    pub animation_timer: f32,
}

#[derive(Component)]
pub struct AnimationGraphs {
    idle_graph: Handle<AnimationGraph>,
    idle_index: AnimationNodeIndex,
    moving_graph: Handle<AnimationGraph>,
    moving_index: AnimationNodeIndex,
}

#[derive(Component)]
pub struct CurrentAnimationState {
    is_moving: bool,
}

#[derive(Component)]
pub struct TimerBar;

#[derive(Component)]
pub struct TimerText;

#[derive(Resource)]
pub struct HexMapConfig {
    pub map_radius: i32,
}

#[derive(Resource, Default)]
pub struct HoveredHex {
    pub entity: Option<Entity>,
}

#[derive(Resource, Default)]
pub struct Obstacles {
    pub positions: HashSet<(i32, i32)>,
}

#[derive(Resource, Default)]
pub struct Occupancy {
    pub positions: HashSet<(i32, i32)>,
    // Maps position to the entity occupying it
    pub position_to_entity: HashMap<(i32, i32), Entity>,
}

#[derive(Resource, Default)]
pub struct OccupancyIntent {
    // Maps entity to the cell it intends to occupy next
    pub intentions: HashMap<Entity, (i32, i32)>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LaunchPadOwner {
    Neutral,
    Red,
    Blue,
}

#[derive(Resource)]
pub struct LaunchPads {
    pub pads: Vec<Vec<(i32, i32)>>, // Each pad is a list of tile coordinates
}

#[derive(Resource)]
pub struct GameTimer {
    pub time_remaining: f32, // Seconds remaining
    pub is_active: bool,
    pub winning_army: Option<Army>,
}

impl Default for GameTimer {
    fn default() -> Self {
        Self {
            time_remaining: 20.0,
            is_active: false,
            winning_army: None,
        }
    }
}

#[derive(Resource, Default)]
pub struct GameState {
    pub winner: Option<Army>,
    pub game_over: bool,
}

#[derive(Component)]
pub struct GameOverScreen;

pub struct HexMapPlugin;

impl Plugin for HexMapPlugin {
    fn build(&self, app: &mut App) {
        let mut obstacles = Obstacles::default();
        obstacles.positions.insert((2, 0)); // Obstacle to the right
        obstacles.positions.insert((0, 2)); // Detail sprite location
        obstacles.positions.insert((-2, 0)); // Detail sprite location

        // Define launch pads
        let launch_pads = LaunchPads {
            pads: vec![
                vec![(-2, 1), (-1, 1), (-1, 0)], // Launch pad 1
                // Can add more launch pads here later
            ],
        };

        app.insert_resource(HexMapConfig { map_radius: 5 })
            .insert_resource(HoveredHex::default())
            .insert_resource(obstacles)
            .insert_resource(Occupancy::default())
            .insert_resource(OccupancyIntent::default())
            .insert_resource(launch_pads)
            .insert_resource(GameTimer::default())
            .insert_resource(GameState::default())
            .add_systems(Startup, (setup_hex_map, setup_ui, trigger_collision_test).chain())
            .add_systems(Update, (play_animation_when_loaded, update_unit_animations, hex_hover_system, update_outline_colors, update_occupancy_intent, move_units, handle_unit_selection, update_selected_visual, animate_selection_rings, update_path_visualizations, animate_destination_rings, update_occupancy, detect_collisions_and_repath, update_occupied_visuals, check_launch_pad_ownership, update_timer_ui, show_game_over_screen, handle_restart));
    }
}

fn create_hexagon_prism_mesh(height: f32) -> Mesh {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    let base_y = 0.0;
    let top_y = height;

    // TOP FACE
    // Center vertex (index 0)
    positions.push([0.0, top_y, 0.0]);
    normals.push([0.0, 1.0, 0.0]);
    uvs.push([0.5, 0.5]);

    // Top perimeter vertices (indices 1-6)
    // Flat-top orientation (will be rotated by Transform for screen alignment)
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
        indices.push((i + 1) as u32);
        indices.push(((i + 1) % 6 + 1) as u32);
    }

    // SIDE FACES
    let top_start_idx = 1u32; // First top perimeter vertex
    let bottom_start_idx = 7u32; // First bottom perimeter vertex

    // Bottom perimeter vertices (indices 7-12)
    // Flat-top orientation (will be rotated by Transform for screen alignment)
    for i in 0..6 {
        let angle = std::f32::consts::PI / 3.0 * i as f32;
        let x = HEX_RADIUS * angle.cos();
        let z = HEX_RADIUS * angle.sin();

        positions.push([x, base_y, z]);

        // Calculate outward normal for this face
        let normal_angle = angle + std::f32::consts::PI / 6.0; // Offset to get face normal
        let nx = normal_angle.cos();
        let nz = normal_angle.sin();
        normals.push([nx, 0.0, nz]);
        uvs.push([0.0, 0.0]);
    }

    // Create side faces (6 rectangular faces, each made of 2 triangles)
    for i in 0..6 {
        let next_i = (i + 1) % 6;

        let top_current = top_start_idx + i;
        let top_next = top_start_idx + next_i;
        let bottom_current = bottom_start_idx + i;
        let bottom_next = bottom_start_idx + next_i;

        // First triangle of quad
        indices.push(top_current);
        indices.push(bottom_current);
        indices.push(top_next);

        // Second triangle of quad
        indices.push(bottom_current);
        indices.push(bottom_next);
        indices.push(top_next);
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

fn create_selection_ring_mesh(inner_radius: f32, outer_radius: f32) -> Mesh {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    let segments = 32; // Smooth circle

    // Create vertices for inner and outer circles
    for i in 0..segments {
        let angle = (i as f32 / segments as f32) * std::f32::consts::PI * 2.0;
        let cos = angle.cos();
        let sin = angle.sin();

        // Outer vertex
        positions.push([outer_radius * cos, 0.1, outer_radius * sin]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.5 + cos * 0.5, 0.5 + sin * 0.5]);

        // Inner vertex
        positions.push([inner_radius * cos, 0.1, inner_radius * sin]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.5 + cos * 0.3, 0.5 + sin * 0.3]);
    }

    // Create triangles for the ring
    for i in 0..segments {
        let outer_current = (i * 2) as u32;
        let inner_current = (i * 2 + 1) as u32;
        let outer_next = ((i * 2 + 2) % (segments * 2)) as u32;
        let inner_next = ((i * 2 + 3) % (segments * 2)) as u32;

        // First triangle
        indices.push(outer_current);
        indices.push(inner_current);
        indices.push(outer_next);

        // Second triangle
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

fn create_hexagon_outline_mesh() -> Mesh {
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uvs = Vec::new();
    let mut indices = Vec::new();

    // Center the outline on the hex boundary so it overlaps with neighbors
    let outer_radius = HEX_RADIUS + (OUTLINE_WIDTH / 2.0);
    let inner_radius = HEX_RADIUS - (OUTLINE_WIDTH / 2.0);

    // Create 6 pairs of vertices (outer and inner) around the hexagon
    // Flat-top orientation (will be rotated by Transform for screen alignment)
    for i in 0..6 {
        let angle = std::f32::consts::PI / 3.0 * i as f32;
        let cos = angle.cos();
        let sin = angle.sin();

        // Outer vertex (extends into neighboring hexes)
        let outer_x = outer_radius * cos;
        let outer_z = outer_radius * sin;
        positions.push([outer_x, 0.0, outer_z]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.5, 0.5]);

        // Inner vertex
        let inner_x = inner_radius * cos;
        let inner_z = inner_radius * sin;
        positions.push([inner_x, 0.0, inner_z]);
        normals.push([0.0, 1.0, 0.0]);
        uvs.push([0.5, 0.5]);
    }

    // Create triangles for the outline ring
    for i in 0..6 {
        let outer_current = (i * 2) as u32;
        let inner_current = (i * 2 + 1) as u32;
        let outer_next = ((i * 2 + 2) % 12) as u32;
        let inner_next = ((i * 2 + 3) % 12) as u32;

        // First triangle
        indices.push(outer_current);
        indices.push(inner_current);
        indices.push(outer_next);

        // Second triangle
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

fn axial_to_world_pos(q: i32, r: i32) -> Vec3 {
    // Pointy-top hex coordinates
    // For pointy-top: horizontal spacing uses sqrt(3)*size, vertical uses 1.5*size
    let x = HEX_HEIGHT * (q as f32 + r as f32 * 0.5);
    let z = HEX_WIDTH * 0.75 * r as f32;
    // Y is always 0 since prisms are positioned at their base
    Vec3::new(x, 0.0, z)
}

// A* pathfinding for hex grid
#[derive(Copy, Clone, Eq, PartialEq)]
struct PathNode {
    pos: (i32, i32),
    f_score: i32, // f = g + h
}

impl Ord for PathNode {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse ordering for min-heap
        other.f_score.cmp(&self.f_score)
    }
}

impl PartialOrd for PathNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

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

fn find_path(start: (i32, i32), goal: (i32, i32), map_radius: i32, obstacles: &HashSet<(i32, i32)>) -> Option<Vec<(i32, i32)>> {
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
            // Reconstruct path
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
            // Check if neighbor is within map bounds
            if q.abs() > map_radius || r.abs() > map_radius || (q + r).abs() > map_radius {
                continue;
            }

            // Skip if neighbor is an obstacle (unless it's the goal)
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

    None // No path found
}

fn create_details_sprite_quad_from_pixels(width: f32, height: f32, x_px: f32, y_px: f32, sprite_w_px: f32, sprite_h_px: f32) -> Mesh {
    // Create a quad from pixel coordinates in the details.png spritesheet
    // Texture dimensions: 128x128 pixels
    let half_width = width / 2.0;

    let texture_size = 128.0;

    // Add a small inset to avoid texture bleeding
    let inset = 0.5;

    let u_start = (x_px + inset) / texture_size;
    let u_end = (x_px + sprite_w_px - inset) / texture_size;

    let v_start = (y_px + inset) / texture_size;
    let v_end = (y_px + sprite_h_px - inset) / texture_size;

    let positions = vec![
        [-half_width, 0.0, 0.0],
        [half_width, 0.0, 0.0],
        [half_width, height, 0.0],
        [-half_width, height, 0.0],
    ];

    let normals = vec![
        [0.0, 0.0, 1.0],
        [0.0, 0.0, 1.0],
        [0.0, 0.0, 1.0],
        [0.0, 0.0, 1.0],
    ];

    let uvs = vec![
        [u_start, v_end],
        [u_end, v_end],
        [u_end, v_start],
        [u_start, v_start],
    ];

    let indices = vec![0, 1, 2, 0, 2, 3];

    let mut mesh = Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::default(),
    );
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
    mesh.insert_indices(Indices::U32(indices.into_iter().map(|i| i as u32).collect()));
    mesh
}

fn create_sprite_quad(width: f32, height: f32, sprite_index: usize) -> Mesh {
    // Create a simple quad that will be used for sprite billboards
    // The quad is centered horizontally and sits on the ground
    let half_width = width / 2.0;

    // Calculate UV coordinates for the specific sprite
    // Sprite sheet: 2 columns x 3 rows (6 total sprites)
    // Texture dimensions: 128x256 pixels
    // Each sprite: 40x66 pixels (user specified)
    let texture_width_px = 128.0;
    let texture_height_px = 256.0;
    let sprite_width_px = 40.0;
    let sprite_height_px = 66.0;

    let col = sprite_index % 2;
    let row = sprite_index / 2;

    // Add a small inset (0.5 pixels) to avoid texture bleeding from adjacent sprites
    let inset = 0.5;

    let u_start = ((col as f32 * sprite_width_px) + inset) / texture_width_px;
    let u_end = (((col as f32 + 1.0) * sprite_width_px) - inset) / texture_width_px;

    let v_start = ((row as f32 * sprite_height_px) + inset) / texture_height_px;
    let v_end = (((row as f32 + 1.0) * sprite_height_px) - inset) / texture_height_px;

    let positions = vec![
        [-half_width, 0.0, 0.0],
        [half_width, 0.0, 0.0],
        [half_width, height, 0.0],
        [-half_width, height, 0.0],
    ];

    let normals = vec![
        [0.0, 0.0, 1.0],
        [0.0, 0.0, 1.0],
        [0.0, 0.0, 1.0],
        [0.0, 0.0, 1.0],
    ];

    let uvs = vec![
        [u_start, v_end],
        [u_end, v_end],
        [u_end, v_start],
        [u_start, v_start],
    ];

    let indices = vec![0, 1, 2, 0, 2, 3];

    let mut mesh = Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::default(),
    );
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
    mesh.insert_indices(Indices::U32(indices.into_iter().map(|i| i as u32).collect()));
    mesh
}

fn setup_hex_map(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut animation_graphs: ResMut<Assets<AnimationGraph>>,
    asset_server: Res<AssetServer>,
    config: Res<HexMapConfig>,
    obstacles: Res<Obstacles>,
) {
    // Spawn 3D camera with orthographic projection for true isometric view
    let mut orthographic = OrthographicProjection::default_3d();
    orthographic.scale = 0.8; // Lower value = more zoom

    commands.spawn((
        Camera3d::default(),
        Projection::Orthographic(orthographic),
        Transform::from_xyz(400.0, 400.0, 400.0).looking_at(Vec3::ZERO, Vec3::Y),
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

    let outline_mesh = meshes.add(create_hexagon_outline_mesh());

    // Generate hexagonal grid
    for q in -config.map_radius..=config.map_radius {
        let r1 = (-config.map_radius).max(-q - config.map_radius);
        let r2 = config.map_radius.min(-q + config.map_radius);

        for r in r1..=r2 {
            // All tiles at ground level
            let height = 0;

            // Create prism mesh with base height
            let prism_height = 5.0;
            let hex_mesh = meshes.add(create_hexagon_prism_mesh(prism_height));

            let world_pos = axial_to_world_pos(q, r);

            // Check if this tile is an obstacle
            let is_obstacle = obstacles.positions.contains(&(q, r));

            // Check if this tile is a launch pad
            let is_launch_pad = (q == -2 && r == 1) || (q == -1 && r == 1) || (q == -1 && r == 0);

            // Color tiles - black for normal tiles, red for obstacles
            let color = if is_obstacle {
                Color::srgb(1.0, 0.0, 0.0) // Bright red for obstacles
            } else {
                Color::srgb(0.0, 0.0, 0.0) // Black for all other tiles
            };

            // Spawn main hex tile
            // Rotate by 90 degrees to make flat-top hexes pointy-top
            let hex_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
            let hex_entity = commands.spawn((
                Mesh3d(hex_mesh),
                MeshMaterial3d(materials.add(StandardMaterial {
                    base_color: color,
                    unlit: is_obstacle, // Make obstacles unlit so red is bright
                    ..default()
                })),
                Transform::from_translation(world_pos).with_rotation(hex_rotation),
                HexTile { q, r, _height: height },
                Name::new(format!("Hex ({}, {})", q, r)),
            )).id();

            // Spawn hex outline ring for hover effect
            // Layer outlines: normal at +0.5, obstacles at +0.6, launch pads at +0.7 (highest)
            let base_outline_height = prism_height + 0.5;
            let outline_pos = if is_launch_pad {
                world_pos + Vec3::new(0.0, base_outline_height + 0.2, 0.0) // Highest for launch pads
            } else if is_obstacle {
                world_pos + Vec3::new(0.0, base_outline_height + 0.1, 0.0) // Slightly higher for obstacles
            } else {
                world_pos + Vec3::new(0.0, base_outline_height, 0.0)
            };

            let outline_color = if is_obstacle {
                Color::srgb(1.0, 0.0, 0.0) // Bright red for obstacles
            } else {
                Color::srgb(0.5, 0.5, 0.5) // Light gray normally
            };

            // Rotate outlines to match hex rotation
            let outline_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);

            if is_obstacle {
                // Obstacle outline - mark it so it doesn't get overwritten
                commands.spawn((
                    Mesh3d(outline_mesh.clone()),
                    MeshMaterial3d(materials.add(StandardMaterial {
                        base_color: outline_color,
                        unlit: true,
                        ..default()
                    })),
                    Transform::from_translation(outline_pos).with_rotation(outline_rotation),
                    HexOutline { hex_entity },
                    ObstacleOutline, // Mark as obstacle outline
                ));
            } else if is_launch_pad {
                // Launch pad outline - bright blue, renders on top
                commands.spawn((
                    Mesh3d(outline_mesh.clone()),
                    MeshMaterial3d(materials.add(StandardMaterial {
                        base_color: Color::srgb(0.2, 0.6, 1.0), // Bright blue
                        emissive: Color::srgb(0.2, 0.6, 1.0).into(), // Glowing blue
                        unlit: true,
                        ..default()
                    })),
                    Transform::from_translation(outline_pos).with_rotation(outline_rotation),
                    HexOutline { hex_entity },
                    LaunchPadOutline, // Mark as launch pad outline
                ));
            } else {
                // Normal outline
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

            // Print debug info
            if is_obstacle {
                println!("Creating RED OBSTACLE at ({}, {})", q, r);
            }
            if is_launch_pad {
                println!("Creating BLUE LAUNCH PAD at ({}, {})", q, r);
            }
        }
    }

    // Load alien sprites texture
    // Load 3D model
    let stickman_scene: Handle<Scene> = asset_server.load("Fox.glb#Scene0");

    // Spawn units positioned for collision testing
    // Two units on opposite sides that will move left-to-right toward each other
    // Format: (q, r, unit_index, army)
    let units = vec![
        (-3, 1, 0, Army::Red),   // Red unit on left
        (3, 1, 1, Army::Blue),    // Blue unit on right
    ];

    // Create selection ring mesh (reuse for all units)
    let ring_mesh = meshes.add(create_selection_ring_mesh(20.0, 30.0));
    let ring_material = materials.add(StandardMaterial {
        base_color: Color::srgb(1.0, 1.0, 1.0), // White
        emissive: Color::srgb(1.0, 1.0, 1.0).into(),
        unlit: true,
        ..default()
    });

    for (q, r, unit_index, army) in units {
        let world_pos = axial_to_world_pos(q, r);
        // Position unit above the hex tile (5.0 is the hex height)
        let unit_pos = world_pos + Vec3::new(0.0, 5.0, 0.0);

        // Create separate animation graphs for idle and moving
        let (idle_graph, idle_index) = AnimationGraph::from_clip(
            asset_server.load(GltfAssetLabel::Animation(0).from_asset("Fox.glb"))
        );
        let (moving_graph, moving_index) = AnimationGraph::from_clip(
            asset_server.load(GltfAssetLabel::Animation(2).from_asset("Fox.glb"))
        );
        let idle_graph_handle = animation_graphs.add(idle_graph);
        let moving_graph_handle = animation_graphs.add(moving_graph);

        // Spawn the 3D model as a scene (start with idle graph)
        let unit_entity = commands.spawn((
            SceneRoot(stickman_scene.clone()),
            Transform::from_translation(unit_pos)
                .with_scale(Vec3::splat(0.5)), // Scale for Fox model
            Unit { q, r, _sprite_index: unit_index, army },
            AnimationGraphHandle(idle_graph_handle.clone()),
            AnimationGraphs {
                idle_graph: idle_graph_handle,
                idle_index,
                moving_graph: moving_graph_handle,
                moving_index,
            },
            CurrentAnimationState { is_moving: false },
            Name::new(format!("Unit {} ({}, {})", unit_index, q, r)),
        )).id();

        // Spawn selection ring at unit's feet (initially hidden)
        // Position slightly above hex outlines (which are at 5.5)
        // Rotate to match hex orientation
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
            Visibility::Hidden, // Start hidden
        ));
    }

    // Load details.png texture and spawn a couple of sprites from first column
    let details_texture: Handle<Image> = asset_server.load("details.png");
    let details_material = materials.add(StandardMaterial {
        base_color_texture: Some(details_texture),
        alpha_mode: AlphaMode::Blend,
        unlit: true,
        cull_mode: None,
        ..default()
    });

    // Spawn detail sprites using the specific sprite at x=32px, 30px wide, 32px tall
    let detail_positions = vec![
        (0, 2),   // Center-ish, offset up
        (-2, 0),  // Left side
    ];

    for (q, r) in detail_positions {
        let world_pos = axial_to_world_pos(q, r);
        let sprite_pos = world_pos + Vec3::new(0.0, 0.0, 0.0);

        // Use the sprite at x=32px that is 30px wide and 32px tall
        let sprite_width_render = 30.0 * 2.0;  // Scale up 2x for visibility
        let sprite_height_render = 32.0 * 2.0;
        let detail_mesh = meshes.add(create_details_sprite_quad_from_pixels(
            sprite_width_render,
            sprite_height_render,
            32.0,  // x offset in pixels
            0.0,   // y offset (assuming first row)
            30.0,  // sprite width in pixels
            32.0   // sprite height in pixels
        ));

        let rotation = Quat::from_rotation_y(std::f32::consts::PI / 4.0);

        commands.spawn((
            Mesh3d(detail_mesh),
            MeshMaterial3d(details_material.clone()),
            Transform::from_translation(sprite_pos).with_rotation(rotation),
            Name::new(format!("Detail Sprite ({}, {})", q, r)),
        ));
    }
}

fn hex_hover_system(
    windows: Query<&Window>,
    camera_query: Query<(&Camera, &GlobalTransform)>,
    hex_query: Query<(Entity, &Transform, &HexTile)>,
    mut hovered_hex: ResMut<HoveredHex>,
    obstacles: Res<Obstacles>,
) {
    let Ok(window) = windows.single() else {
        return;
    };
    let Ok((camera, camera_transform)) = camera_query.single() else {
        return;
    };

    if let Some(cursor_position) = window.cursor_position() {
        // Cast a ray from the camera through the cursor position
        if let Ok(ray) = camera.viewport_to_world(camera_transform, cursor_position) {
            // Find intersection with the hex plane (y = 0 for hex bases)
            // Ray equation: point = origin + direction * t
            // For y = 0: origin.y + direction.y * t = 0
            // Solve for t: t = -origin.y / direction.y

            if ray.direction.y.abs() > 0.001 {
                let t = -ray.origin.y / ray.direction.y;
                let intersection = ray.origin + ray.direction * t;

                // Find which hex tile contains this world position
                let mut closest_hex: Option<(Entity, f32)> = None;

                for (entity, transform, _tile) in &hex_query {
                    let hex_pos = transform.translation;
                    let distance = Vec2::new(hex_pos.x - intersection.x, hex_pos.z - intersection.z).length();

                    // Check if point is within hex radius
                    if distance <= HEX_RADIUS {
                        match closest_hex {
                            None => closest_hex = Some((entity, distance)),
                            Some((_, closest_dist)) if distance < closest_dist => {
                                closest_hex = Some((entity, distance));
                            }
                            _ => {}
                        }
                    }
                }

                let new_hovered = closest_hex.map(|(entity, _)| entity);

                // Print hovered tile coordinates only when it changes
                if new_hovered != hovered_hex.entity {
                    if let Some(hovered_entity) = new_hovered {
                        for (entity, _, tile) in &hex_query {
                            if entity == hovered_entity {
                                let is_obstacle = obstacles.positions.contains(&(tile.q, tile.r));
                                let obstacle_text = if is_obstacle { " [OBSTACLE]" } else { "" };
                                println!("Hovering: ({}, {}){}", tile.q, tile.r, obstacle_text);
                                break;
                            }
                        }
                    }
                }

                hovered_hex.entity = new_hovered;
            }
        }
    } else {
        hovered_hex.entity = None;
    }
}

fn update_outline_colors(
    hovered_hex: Res<HoveredHex>,
    mut outline_query: Query<(&HexOutline, &MeshMaterial3d<StandardMaterial>, &mut Transform), (Without<ObstacleOutline>, Without<LaunchPadOutline>)>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    for (outline, material_handle, mut transform) in &mut outline_query {
        if let Some(material) = materials.get_mut(&material_handle.0) {
            // Check if this outline's hex is hovered
            let is_hovered = hovered_hex.entity == Some(outline.hex_entity);

            // Update color and height based on hover state
            if is_hovered {
                material.base_color = Color::srgb(1.0, 1.0, 0.0); // Yellow when hovered
                transform.translation.y = 5.5 + 0.15; // Raise above normal outlines
            } else {
                material.base_color = Color::srgb(0.5, 0.5, 0.5); // Light gray normally
                transform.translation.y = 5.5; // Normal height
            }
        }
    }
}

fn move_units(
    time: Res<Time>,
    mut commands: Commands,
    mut query: Query<(Entity, &mut Transform, &mut Unit, &mut UnitMovement)>,
) {
    for (entity, mut transform, mut unit, mut movement) in &mut query {
        // Check if we have more waypoints
        if movement.current_waypoint >= movement.path.len() {
            // Path complete
            commands.entity(entity).remove::<UnitMovement>();
            continue;
        }

        // Get current and next waypoint
        let current_hex = (unit.q, unit.r);
        let target_hex = movement.path[movement.current_waypoint];

        // Calculate positions
        let start_pos = axial_to_world_pos(current_hex.0, current_hex.1);
        let target_pos = axial_to_world_pos(target_hex.0, target_hex.1);
        let distance = start_pos.distance(target_pos);

        // Calculate target rotation for current movement
        let target_rotation = if distance > 0.0 {
            let direction = (target_pos - start_pos).normalize();
            let angle = direction.z.atan2(direction.x);
            Quat::from_rotation_y(-angle + std::f32::consts::PI / 2.0)
        } else {
            transform.rotation
        };

        // Smoothly interpolate rotation
        // Start rotating early (at 0% progress) and continue throughout the movement
        let rotation_speed = 8.0; // Higher = faster rotation
        transform.rotation = transform.rotation.slerp(target_rotation, time.delta_secs() * rotation_speed);

        // Update progress
        if distance > 0.0 {
            movement.progress += (time.delta_secs() * movement.speed) / distance;
        } else {
            movement.progress = 1.0;
        }

        if movement.progress >= 1.0 {
            // Reached current waypoint
            unit.q = target_hex.0;
            unit.r = target_hex.1;
            transform.translation.x = target_pos.x;
            transform.translation.z = target_pos.z;

            // Move to next waypoint
            movement.current_waypoint += 1;
            movement.progress = 0.0;

            // If no more waypoints, remove movement component
            if movement.current_waypoint >= movement.path.len() {
                commands.entity(entity).remove::<UnitMovement>();
            }
        } else {
            // Interpolate position
            let current_pos = start_pos.lerp(target_pos, movement.progress);
            transform.translation.x = current_pos.x;
            transform.translation.z = current_pos.z;
        }
    }
}

fn handle_unit_selection(
    mouse_button: Res<ButtonInput<MouseButton>>,
    hovered_hex: Res<HoveredHex>,
    hex_query: Query<&HexTile>,
    config: Res<HexMapConfig>,
    obstacles: Res<Obstacles>,
    occupancy: Res<Occupancy>,
    occupancy_intent: Res<OccupancyIntent>,
    unit_query: Query<(Entity, &Unit, Option<&UnitMovement>), Without<Selected>>,
    selected_query: Query<(Entity, &Unit, &UnitStats, Option<&UnitMovement>, &Transform), With<Selected>>,
    path_viz_query: Query<(Entity, &PathVisualization)>,
    dest_ring_query: Query<(Entity, &DestinationRing)>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut commands: Commands,
) {
    if mouse_button.just_pressed(MouseButton::Left) {
        if let Some(hovered_entity) = hovered_hex.entity {
            if let Ok(hovered_tile) = hex_query.get(hovered_entity) {
                // Check if there's a unit on this tile
                let mut clicked_unit = None;
                for (entity, unit, movement) in &unit_query {
                    // Only consider units that aren't currently moving
                    if movement.is_none() && unit.q == hovered_tile.q && unit.r == hovered_tile.r {
                        clicked_unit = Some(entity);
                        break;
                    }
                }

                if let Some(unit_entity) = clicked_unit {
                    // Deselect all units
                    for (entity, _, _, _, _) in &selected_query {
                        commands.entity(entity).remove::<Selected>();
                    }
                    // Select this unit
                    commands.entity(unit_entity).insert(Selected);
                } else {
                    // No unit on this tile - try to move selected unit here
                    if let Ok((selected_entity, selected_unit, stats, existing_movement, _unit_transform)) = selected_query.single() {
                        let goal = (hovered_tile.q, hovered_tile.r);

                        // Don't allow setting destination on obstacle tiles
                        if obstacles.positions.contains(&goal) {
                            return;
                        }

                        // Create combined blocking set: obstacles + occupied cells + intent cells (excluding the moving unit)
                        let mut blocking_cells = obstacles.positions.clone();
                        let unit_current_pos = (selected_unit.q, selected_unit.r);
                        for &occupied_pos in &occupancy.positions {
                            if occupied_pos != unit_current_pos && occupied_pos != goal {
                                blocking_cells.insert(occupied_pos);
                            }
                        }
                        // Include cells that other units intend to occupy
                        for (entity, &intent_pos) in &occupancy_intent.intentions {
                            if *entity != selected_entity && intent_pos != unit_current_pos && intent_pos != goal {
                                blocking_cells.insert(intent_pos);
                            }
                        }

                        // Despawn existing path visualization and destination ring to restart the animation
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
                            // Unit is already moving - determine if we should continue forward or reverse
                            let current_cell = (selected_unit.q, selected_unit.r);
                            let next_cell = movement.path[movement.current_waypoint];

                            // Find paths from both positions
                            let path_from_current = find_path(current_cell, goal, config.map_radius, &blocking_cells);
                            let path_from_next = find_path(next_cell, goal, config.map_radius, &blocking_cells);

                            // Determine which direction to go based on path length
                            let should_reverse = match (path_from_current, path_from_next) {
                                (Some(p1), Some(p2)) => p1.len() < p2.len(),
                                (Some(_), None) => true,
                                (None, Some(_)) => false,
                                (None, None) => false, // No path found, don't change direction
                            };

                            if should_reverse {
                                // Reverse back to current cell, then follow path from there
                                if let Some(path) = find_path(current_cell, goal, config.map_radius, &blocking_cells) {
                                    // Build new path: first go back to current cell, then follow new path
                                    let mut new_path = vec![current_cell];
                                    if path.len() > 1 {
                                        new_path.extend_from_slice(&path[1..]);
                                    }

                                    // Update unit's stored position to next_cell so the lerp works correctly
                                    // Visual position is currently: current_cell.lerp(next_cell, progress)
                                    // We want to maintain that position while reversing direction
                                    // By setting unit position to next_cell and inverting progress:
                                    // new visual = next_cell.lerp(current_cell, 1.0 - progress) = current_cell.lerp(next_cell, progress) ✓
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
                                            progress: 1.0 - movement.progress, // Reverse the progress
                                            speed: stats.speed,
                                        },
                                    ));

                                    // Spawn destination ring at goal
                                    spawn_destination_ring(&mut commands, &mut meshes, &mut materials, selected_entity, goal);
                                }
                            } else {
                                // Continue forward to next cell, then follow new path from there
                                if let Some(path) = find_path(next_cell, goal, config.map_radius, &blocking_cells) {
                                    // Keep the current segment (current_cell -> next_cell) and append the new path after
                                    let mut new_full_path = vec![next_cell];
                                    if path.len() > 1 {
                                        new_full_path.extend_from_slice(&path[1..]);
                                    }

                                    if new_full_path.len() > 1 {
                                        // Keep unit position and progress as-is, just update the path
                                        commands.entity(selected_entity).insert(UnitMovement {
                                            path: new_full_path,
                                            current_waypoint: 0,
                                            progress: movement.progress, // Continue with current progress toward next_cell
                                            speed: stats.speed,
                                        });

                                        // Spawn destination ring at goal
                                        spawn_destination_ring(&mut commands, &mut meshes, &mut materials, selected_entity, goal);
                                    }
                                }
                            }
                        } else {
                            // Unit is stationary - just find a path from current position
                            let start = (selected_unit.q, selected_unit.r);
                            if let Some(path) = find_path(start, goal, config.map_radius, &blocking_cells) {
                                let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                                    path[1..].to_vec()
                                } else {
                                    vec![]
                                };

                                if !path_to_follow.is_empty() {
                                    println!("User-commanded unit moving from ({}, {}) to destination ({}, {})", start.0, start.1, goal.0, goal.1);
                                    commands.entity(selected_entity).insert(UnitMovement {
                                        path: path_to_follow,
                                        current_waypoint: 0,
                                        progress: 0.0,
                                        speed: stats.speed,
                                    });

                                    // Spawn destination ring at goal
                                    spawn_destination_ring(&mut commands, &mut meshes, &mut materials, selected_entity, goal);
                                }
                            }
                        }
                        // Keep unit selected after moving
                    }
                }
            }
        }
    }
}

fn spawn_destination_ring(
    commands: &mut Commands,
    meshes: &mut ResMut<Assets<Mesh>>,
    materials: &mut ResMut<Assets<StandardMaterial>>,
    unit_entity: Entity,
    destination: (i32, i32),
) {
    let dest_pos = axial_to_world_pos(destination.0, destination.1);
    let ring_pos = dest_pos + Vec3::new(0.0, 6.5, 0.0); // Slightly above hex outlines

    let ring_mesh = meshes.add(create_selection_ring_mesh(25.0, 35.0)); // Larger than unit ring
    let ring_material = materials.add(StandardMaterial {
        base_color: Color::srgb(0.0, 0.5, 0.0), // Dark green
        emissive: Color::srgb(0.0, 0.5, 0.0).into(),
        unlit: true,
        ..default()
    });

    // Rotate destination ring to match hex orientation
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

fn update_selected_visual(
    selected_query: Query<&MeshMaterial3d<StandardMaterial>, (With<Unit>, With<Selected>)>,
    unselected_query: Query<&MeshMaterial3d<StandardMaterial>, (With<Unit>, Without<Selected>)>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // Make selected units brighter
    for material_handle in &selected_query {
        if let Some(material) = materials.get_mut(&material_handle.0) {
            material.emissive = Color::srgb(0.5, 0.5, 0.0).into(); // Yellow glow
        }
    }

    // Remove glow from unselected units
    for material_handle in &unselected_query {
        if let Some(material) = materials.get_mut(&material_handle.0) {
            material.emissive = Color::BLACK.into();
        }
    }
}

fn animate_selection_rings(
    time: Res<Time>,
    unit_query: Query<(Entity, &Transform, Has<Selected>), With<Unit>>,
    mut ring_query: Query<(&mut SelectionRing, &mut Transform, &mut Visibility), Without<Unit>>,
) {
    for (mut ring, mut ring_transform, mut visibility) in &mut ring_query {
        // Find the unit this ring belongs to
        if let Ok((_, unit_transform, is_selected)) = unit_query.get(ring.unit_entity) {
            // Update visibility based on selection
            let was_visible = *visibility == Visibility::Visible;
            *visibility = if is_selected {
                Visibility::Visible
            } else {
                Visibility::Hidden
            };

            // Reset animation when newly selected
            if is_selected && !was_visible {
                ring.animation_timer = 0.0;
            }

            // Position ring at unit's position (slightly above the ground)
            ring_transform.translation.x = unit_transform.translation.x;
            ring_transform.translation.y = unit_transform.translation.y + 1.0; // 1.0 above unit base
            ring_transform.translation.z = unit_transform.translation.z;

            // Animate: scale from large to normal size
            if is_selected {
                ring.animation_timer += time.delta_secs();

                // Animation duration: 0.5 seconds
                let animation_duration = 0.5;
                let progress = (ring.animation_timer / animation_duration).min(1.0);

                // Scale from 2.0 down to 1.0
                let start_scale = 2.5;
                let end_scale = 1.0;
                let current_scale = start_scale + (end_scale - start_scale) * progress;

                ring_transform.scale = Vec3::new(current_scale, 1.0, current_scale);

                // Also rotate slowly
                ring_transform.rotate_y(time.delta_secs() * 1.0);
            }
        }
    }
}

fn create_path_line_mesh(waypoints: &[(i32, i32)], current_pos: Vec3, animation_progress: f32) -> Mesh {
    let mut positions = Vec::new();
    let mut colors = Vec::new();
    let mut indices = Vec::new();

    if waypoints.is_empty() {
        // Return empty mesh
        let mut mesh = Mesh::new(
            PrimitiveTopology::TriangleList,
            RenderAssetUsages::default(),
        );
        mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
        mesh.insert_attribute(Mesh::ATTRIBUTE_COLOR, colors);
        mesh.insert_indices(Indices::U32(indices));
        return mesh;
    }

    // Calculate adjusted final position - stop at destination ring outer edge
    let destination_ring_outer_radius = 35.0;

    // Start from current position
    let line_width = 8.0;
    let line_height = 8.0; // Height above ground
    let white_color = [1.0, 1.0, 1.0, 1.0]; // Bright white, fully opaque
    let dark_color = [0.0, 0.5, 0.0, 1.0]; // Dark green (matches destination ring)
    let dark_segment_length = 25.0; // Length of dark segment in world units

    // Build line segments with subdivisions for smooth animation
    let mut prev_pos = Vec3::new(current_pos.x, line_height, current_pos.z);
    let mut accumulated_distance = 0.0;
    let subdivision_length = 0.5; // Subdivide segments every 0.5 world units for smooth animation

    // Add a white circle at the starting position
    let circle_segments = 8;
    let circle_radius = line_width * 0.5;
    let center_idx = positions.len() as u32;

    positions.push([prev_pos.x, prev_pos.y, prev_pos.z]);
    colors.push(white_color);

    for i in 0..circle_segments {
        let angle = (i as f32 / circle_segments as f32) * std::f32::consts::PI * 2.0;
        let x_offset = circle_radius * angle.cos();
        let z_offset = circle_radius * angle.sin();

        positions.push([prev_pos.x + x_offset, prev_pos.y, prev_pos.z + z_offset]);
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

        // If this is the last waypoint, shorten the segment to stop at ring outer edge
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

        // Determine number of subdivisions needed
        let num_subdivisions = (segment_length / subdivision_length).ceil() as i32;
        let num_subdivisions = num_subdivisions.max(1);

        // Create subdivided quads
        for i in 0..num_subdivisions {
            let t_start = i as f32 / num_subdivisions as f32;
            let t_end = (i + 1) as f32 / num_subdivisions as f32;

            let sub_start = prev_pos.lerp(curr_pos, t_start);
            let sub_end = prev_pos.lerp(curr_pos, t_end);

            let sub_start_dist = accumulated_distance + (segment_length * t_start);
            let sub_end_dist = accumulated_distance + (segment_length * t_end);

            let base_idx = positions.len() as u32;

            // Four vertices for the quad
            positions.push([sub_start.x - perpendicular.x, sub_start.y, sub_start.z - perpendicular.z]);
            positions.push([sub_start.x + perpendicular.x, sub_start.y, sub_start.z + perpendicular.z]);
            positions.push([sub_end.x + perpendicular.x, sub_end.y, sub_end.z + perpendicular.z]);
            positions.push([sub_end.x - perpendicular.x, sub_end.y, sub_end.z - perpendicular.z]);

            // Determine if this subsegment overlaps with the dark segment
            let dark_start = animation_progress;
            let dark_end = animation_progress + dark_segment_length;

            // Check if the dark segment overlaps this subsegment
            let segment_color = if dark_start <= sub_end_dist && dark_end >= sub_start_dist {
                // Dark segment overlaps this subsegment
                dark_color
            } else {
                white_color
            };

            colors.push(segment_color);
            colors.push(segment_color);
            colors.push(segment_color);
            colors.push(segment_color);

            // Two triangles for the quad
            indices.push(base_idx);
            indices.push(base_idx + 1);
            indices.push(base_idx + 2);

            indices.push(base_idx);
            indices.push(base_idx + 2);
            indices.push(base_idx + 3);
        }

        // Add a white circle at each waypoint to cover gaps at corners (skip last waypoint)
        if !is_last {
            let circle_segments = 8; // Number of segments in the circle
            let circle_radius = line_width * 0.5;
            let center_idx = positions.len() as u32;

            // Center vertex
            positions.push([curr_pos.x, curr_pos.y, curr_pos.z]);
            colors.push(white_color);

        // Circle perimeter vertices
        for i in 0..circle_segments {
            let angle = (i as f32 / circle_segments as f32) * std::f32::consts::PI * 2.0;
            let x_offset = circle_radius * angle.cos();
            let z_offset = circle_radius * angle.sin();

            positions.push([curr_pos.x + x_offset, curr_pos.y, curr_pos.z + z_offset]);
            colors.push(white_color);
        }

        // Create triangles for the circle
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

fn animate_destination_rings(
    time: Res<Time>,
    unit_query: Query<(Entity, &UnitMovement)>,
    mut ring_query: Query<(Entity, &mut DestinationRing, &mut Transform)>,
    mut commands: Commands,
) {
    // Remove destination rings for units that are no longer moving
    let moving_units: std::collections::HashSet<Entity> = unit_query.iter().map(|(e, _)| e).collect();

    let mut to_despawn = Vec::new();

    for (entity, mut ring, mut ring_transform) in &mut ring_query {
        // Remove ring if unit is no longer moving
        if !moving_units.contains(&ring.unit_entity) {
            to_despawn.push(entity);
            continue;
        }

        // Animate: pulse scale
        ring.animation_timer += time.delta_secs();

        // Pulsing animation with 1 second period
        let pulse_frequency = 2.0; // 2 cycles per second
        let pulse_progress = (ring.animation_timer * pulse_frequency).sin();

        // Scale between 0.9 and 1.1
        let min_scale = 0.9;
        let max_scale = 1.1;
        let current_scale = min_scale + (max_scale - min_scale) * ((pulse_progress + 1.0) / 2.0);

        ring_transform.scale = Vec3::new(current_scale, 1.0, current_scale);

        // Also rotate slowly
        ring_transform.rotate_y(time.delta_secs() * 0.5);
    }

    // Despawn destination rings for units that finished moving
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
    // First, remove path visualizations for units without movement
    let mut units_with_movement = std::collections::HashSet::new();
    for (unit_entity, _, _, movement) in &unit_query {
        if movement.is_some() {
            units_with_movement.insert(unit_entity);
        }
    }

    // Despawn path visualizations for units without movement
    for (viz_entity, path_viz, _) in &path_viz_query {
        if !units_with_movement.contains(&path_viz.unit_entity) {
            commands.entity(viz_entity).despawn();
        }
    }

    // Update or create path visualizations for moving units
    for (unit_entity, _unit, transform, movement) in &unit_query {
        if let Some(movement) = movement {
            let remaining_path = &movement.path[movement.current_waypoint..];

            // Calculate total path length for animation wrapping
            let mut total_length = 0.0;
            let mut prev_pos = transform.translation;
            for &(q, r) in remaining_path {
                let waypoint_pos = axial_to_world_pos(q, r);
                let curr_pos = Vec3::new(waypoint_pos.x, 0.0, waypoint_pos.z);
                total_length += prev_pos.distance(curr_pos);
                prev_pos = curr_pos;
            }

            // Check if visualization already exists
            let mut found = false;
            for (_viz_entity, mut path_viz, mut mesh_handle) in &mut path_viz_query {
                if path_viz.unit_entity == unit_entity {
                    found = true;

                    // Only animate if we haven't completed 2 loops
                    if path_viz.loop_count < 2 {
                        // Update animation progress at 2x unit speed
                        let animation_speed = movement.speed * 2.0;
                        path_viz.animation_progress += time.delta_secs() * animation_speed;

                        // Check if we completed a loop
                        if total_length > 0.0 && path_viz.animation_progress >= total_length {
                            path_viz.loop_count += 1;
                            path_viz.animation_progress %= total_length;
                        }
                    }

                    // After 2 loops, hide the dark segment by setting progress beyond the path
                    let animation_progress = if path_viz.loop_count >= 2 {
                        total_length + 100.0 // Far beyond the path so dark segment doesn't show
                    } else {
                        path_viz.animation_progress
                    };

                    // Update the mesh with current position, remaining path, and animation
                    let new_mesh = create_path_line_mesh(remaining_path, transform.translation, animation_progress);
                    mesh_handle.0 = meshes.add(new_mesh);
                    break;
                }
            }

            // Create new visualization if it doesn't exist
            if !found {
                let path_mesh = create_path_line_mesh(remaining_path, transform.translation, 0.0);

                commands.spawn((
                    Mesh3d(meshes.add(path_mesh)),
                    MeshMaterial3d(materials.add(StandardMaterial {
                        base_color: Color::srgba(1.0, 1.0, 1.0, 1.0), // Bright white
                        alpha_mode: AlphaMode::Blend,
                        unlit: true,
                        cull_mode: None, // Double-sided
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

fn update_occupancy_intent(
    unit_query: Query<(Entity, &Unit, &UnitMovement)>,
    mut occupancy_intent: ResMut<OccupancyIntent>,
) {
    occupancy_intent.intentions.clear();
    for (entity, unit, movement) in &unit_query {
        // Set intent based on progress toward next waypoint
        if movement.current_waypoint < movement.path.len() {
            // Once we're halfway to the next cell, claim it as our intent
            if movement.progress >= 0.5 {
                let next_cell = movement.path[movement.current_waypoint];
                occupancy_intent.intentions.insert(entity, next_cell);
            } else {
                // Before halfway, our intent is to stay in current position
                occupancy_intent.intentions.insert(entity, (unit.q, unit.r));
            }
        } else {
            // No more waypoints, intent is current position
            occupancy_intent.intentions.insert(entity, (unit.q, unit.r));
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

        // Determine which cell the unit visually occupies based on movement progress
        let occupied_cell = if let Some(movement) = movement_opt {
            if movement.current_waypoint < movement.path.len() && movement.progress >= 0.5 {
                // Past halfway - they visually occupy the next cell
                movement.path[movement.current_waypoint]
            } else {
                // Before halfway - they occupy current cell
                current_cell
            }
        } else {
            // Not moving - occupy current cell
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
    path_viz_query: Query<(Entity, &PathVisualization)>,
    dest_ring_query: Query<(Entity, &DestinationRing)>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut commands: Commands,
) {
    let mut units_to_repath: Vec<(Entity, Unit, (i32, i32), UnitMovement)> = Vec::new();

    // Check each moving unit to see if their next cell is blocked
    for (entity, unit, movement) in &unit_query {
        if movement.current_waypoint < movement.path.len() {
            let next_cell = movement.path[movement.current_waypoint];
            let current_cell = (unit.q, unit.r);

            // Check if the next cell is currently occupied by a higher-priority unit
            let should_yield_to_occupied_cell = if let Some(&occupying_entity) = occupancy.position_to_entity.get(&next_cell) {
                // Cell is occupied - only yield if the occupier has higher priority (lower entity ID)
                next_cell != current_cell && entity.to_bits() > occupying_entity.to_bits()
            } else {
                false
            };

            // For intent conflicts, only check when we're close enough to care (past 40% progress)
            // This prevents premature repathing when units are far from the conflict point
            let should_yield_to_intent = movement.progress >= 0.4 &&
                occupancy_intent.intentions.iter()
                    .any(|(other_entity, &intent_pos)| {
                        if *other_entity != entity && intent_pos == next_cell {
                            // This unit has lower priority (higher entity ID), so it must yield
                            entity.to_bits() > other_entity.to_bits()
                        } else {
                            false
                        }
                    });

            // Only repath if we need to yield to a higher-priority unit (either occupied or intent)
            if should_yield_to_occupied_cell || should_yield_to_intent {
                let final_goal = *movement.path.last().unwrap();
                units_to_repath.push((entity, unit.clone(), final_goal, movement.clone()));
            }
        }
    }

    // Repath units that detected collisions
    for (entity, unit, final_goal, old_movement) in units_to_repath {
        let current_cell = (unit.q, unit.r);
        let next_cell = old_movement.path[old_movement.current_waypoint];

        // Create blocking set (obstacles + occupied cells + intent cells, excluding our position and goal)
        let mut blocking = obstacles.positions.clone();
        for &occupied_pos in &occupancy.positions {
            if occupied_pos != current_cell && occupied_pos != final_goal {
                blocking.insert(occupied_pos);
            }
        }
        // Include cells that other units intend to occupy
        for (other_entity, &intent_pos) in &occupancy_intent.intentions {
            if *other_entity != entity && intent_pos != current_cell && intent_pos != final_goal {
                blocking.insert(intent_pos);
            }
        }

        // Try to find a new path from current cell
        if let Some(path) = find_path(current_cell, final_goal, config.map_radius, &blocking) {
            if path.len() > 1 {
                // Build new path: first reverse back to current cell, then follow new path
                let mut new_path = vec![current_cell];
                new_path.extend_from_slice(&path[1..]);

                if new_path != old_movement.path {
                    // Update unit to reverse smoothly
                    if let Ok((_, mut unit_component, mut movement)) = unit_query.get_mut(entity) {
                        // Update unit's stored position to next_cell so lerp works correctly
                        // Visual position is currently: current_cell.lerp(next_cell, progress)
                        // We want to maintain that position while reversing direction
                        // By setting unit position to next_cell and inverting progress:
                        // new visual = next_cell.lerp(current_cell, 1.0 - progress) = current_cell.lerp(next_cell, progress) ✓
                        unit_component.q = next_cell.0;
                        unit_component.r = next_cell.1;

                        movement.path = new_path;
                        movement.current_waypoint = 0;
                        movement.progress = 1.0 - old_movement.progress; // Reverse the progress
                    }

                    // Despawn and respawn path visualization
                    for (viz_entity, path_viz) in &path_viz_query {
                        if path_viz.unit_entity == entity {
                            commands.entity(viz_entity).despawn();
                            break;
                        }
                    }

                    // Despawn and respawn destination ring
                    for (ring_entity, dest_ring) in &dest_ring_query {
                        if dest_ring.unit_entity == entity {
                            commands.entity(ring_entity).despawn();
                            break;
                        }
                    }

                    spawn_destination_ring(&mut commands, &mut meshes, &mut materials, entity, final_goal);

                    println!("Unit repathing to avoid collision!");
                }
            }
        }
    }
}

fn trigger_collision_test(
    mut commands: Commands,
    unit_query: Query<(Entity, &Unit)>,
    config: Res<HexMapConfig>,
    obstacles: Res<Obstacles>,
    occupancy: Res<Occupancy>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    let units: Vec<(Entity, &Unit)> = unit_query.iter().collect();

    if units.len() >= 2 {
        // Move first unit from left to right
        let (entity1, unit1) = units[0];
        let start1 = (unit1.q, unit1.r);
        let goal1 = (3, 1);  // Move to right side, same visual row

        // Create blocking set for first unit
        let mut blocking1 = obstacles.positions.clone();
        for &occupied_pos in &occupancy.positions {
            if occupied_pos != start1 && occupied_pos != goal1 {
                blocking1.insert(occupied_pos);
            }
        }

        if let Some(path) = find_path(start1, goal1, config.map_radius, &blocking1) {
            let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                path[1..].to_vec()
            } else {
                vec![]
            };

            if !path_to_follow.is_empty() {
                println!("Unit 1 moving from ({}, {}) to destination ({}, {})", start1.0, start1.1, goal1.0, goal1.1);
                commands.entity(entity1).insert(UnitMovement {
                    path: path_to_follow,
                    current_waypoint: 0,
                    progress: 0.0,
                    speed: 100.0,
                });
                spawn_destination_ring(&mut commands, &mut meshes, &mut materials, entity1, goal1);
            }
        }

        // Move second unit from right to left
        let (entity2, unit2) = units[1];
        let start2 = (unit2.q, unit2.r);
        let goal2 = (-3, 1);  // Move to left side, same visual row

        // Create blocking set for second unit
        let mut blocking2 = obstacles.positions.clone();
        for &occupied_pos in &occupancy.positions {
            if occupied_pos != start2 && occupied_pos != goal2 {
                blocking2.insert(occupied_pos);
            }
        }

        if let Some(path) = find_path(start2, goal2, config.map_radius, &blocking2) {
            let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                path[1..].to_vec()
            } else {
                vec![]
            };

            if !path_to_follow.is_empty() {
                println!("Unit 2 moving from ({}, {}) to destination ({}, {})", start2.0, start2.1, goal2.0, goal2.1);
                commands.entity(entity2).insert(UnitMovement {
                    path: path_to_follow,
                    current_waypoint: 0,
                    progress: 0.0,
                    speed: 100.0,
                });
                spawn_destination_ring(&mut commands, &mut meshes, &mut materials, entity2, goal2);
            }
        }

        println!("Collision test triggered: Two units moving toward each other!");
    }
}

fn update_occupied_visuals(
    occupancy: Res<Occupancy>,
    hex_query: Query<(Entity, &HexTile)>,
    occupied_outline_query: Query<(Entity, &HexOutline), With<OccupiedOutline>>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // Remove all existing occupied outlines
    for (entity, _) in &occupied_outline_query {
        commands.entity(entity).despawn();
    }

    // Create new occupied outlines for currently occupied tiles
    for (hex_entity, hex_tile) in &hex_query {
        let tile_pos = (hex_tile.q, hex_tile.r);
        if occupancy.positions.contains(&tile_pos) {
            let world_pos = axial_to_world_pos(hex_tile.q, hex_tile.r);
            let outline_pos = world_pos + Vec3::new(0.0, 5.7, 0.0); // Higher than obstacle outlines

            let outline_mesh = meshes.add(create_hexagon_outline_mesh());
            let outline_material = materials.add(StandardMaterial {
                base_color: Color::srgb(1.0, 0.8, 0.0), // Yellow/orange
                emissive: Color::srgb(1.0, 0.8, 0.0).into(),
                unlit: true,
                ..default()
            });

            // Rotate occupied outline to match hex orientation
            let outline_rotation = Quat::from_rotation_y(std::f32::consts::PI / 2.0);
            commands.spawn((
                Mesh3d(outline_mesh),
                MeshMaterial3d(outline_material),
                Transform::from_translation(outline_pos).with_rotation(outline_rotation),
                HexOutline { hex_entity },
                OccupiedOutline,
            ));
        }
    }
}

// System that switches animations based on movement state
fn update_unit_animations(
    mut commands: Commands,
    mut units_query: Query<(Entity, &AnimationGraphs, &mut CurrentAnimationState, &mut AnimationGraphHandle, Option<&UnitMovement>), With<Unit>>,
    children_query: Query<&Children>,
    mut players_query: Query<&mut AnimationPlayer>,
) {
    for (unit_entity, anim_graphs, mut anim_state, mut graph_handle, movement) in units_query.iter_mut() {
        let is_moving = movement.is_some();

        // Debug logging
        if is_moving != anim_state.is_moving {
            println!("Unit {:?}: movement state changing from {} to {}",
                unit_entity,
                anim_state.is_moving,
                is_moving
            );
        }

        // Check if movement state changed
        if is_moving != anim_state.is_moving {
            anim_state.is_moving = is_moving;

            // Select the appropriate graph and animation index
            let (new_graph, new_index) = if is_moving {
                (anim_graphs.moving_graph.clone(), anim_graphs.moving_index)
            } else {
                (anim_graphs.idle_graph.clone(), anim_graphs.idle_index)
            };

            // Update the graph handle on the unit entity
            *graph_handle = AnimationGraphHandle(new_graph.clone());

            // Find the AnimationPlayer in descendants and update it
            let mut found_player = false;
            for descendant in children_query.iter_descendants(unit_entity) {
                if let Ok(mut player) = players_query.get_mut(descendant) {
                    // Update the graph handle on the player entity
                    commands.entity(descendant).insert(AnimationGraphHandle(new_graph.clone()));

                    // Stop all and play the new animation
                    player.stop_all();
                    player.play(new_index).repeat();
                    println!("Switched to {} animation (index {:?}) for unit {:?} on entity {:?}",
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
                println!("Warning: Could not find AnimationPlayer for unit {:?}", unit_entity);
            }
        }
    }
}

// System that plays animations when AnimationPlayers are added by scene loading
fn play_animation_when_loaded(
    mut commands: Commands,
    units_query: Query<(Entity, &AnimationGraphs, &AnimationGraphHandle), With<Unit>>,
    children_query: Query<&Children>,
    mut players_query: Query<(Entity, &mut AnimationPlayer), Added<AnimationPlayer>>,
) {
    // Find any newly added AnimationPlayers
    for (player_entity, mut player) in players_query.iter_mut() {
        println!("Found newly added AnimationPlayer on entity {:?}", player_entity);

        // Find which unit this player belongs to
        for (unit_entity, anim_graphs, graph_handle) in &units_query {
            // Check if player_entity is a descendant of unit_entity
            let mut is_descendant = false;
            for descendant in children_query.iter_descendants(unit_entity) {
                if descendant == player_entity {
                    is_descendant = true;
                    break;
                }
            }

            if is_descendant {
                println!("AnimationPlayer belongs to unit {:?}", unit_entity);

                // Add the graph handle to the entity with the player
                commands.entity(player_entity).insert(graph_handle.clone());

                // Start playing the idle animation by default
                player.play(anim_graphs.idle_index).repeat();
                println!("Started idle animation {:?} on entity {:?}", anim_graphs.idle_index, player_entity);

                break;
            }
        }
    }
}

fn setup_ui(mut commands: Commands) {
    // Create UI root node at the bottom of the screen
    commands.spawn((
        Node {
            width: Val::Percent(100.0),
            height: Val::Px(60.0),
            position_type: PositionType::Absolute,
            bottom: Val::Px(0.0),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            ..default()
        },
    )).with_children(|parent| {
        // Timer bar background
        parent.spawn((
            Node {
                width: Val::Px(400.0),
                height: Val::Px(30.0),
                border: UiRect::all(Val::Px(2.0)),
                ..default()
            },
            BorderColor::all(Color::srgb(0.3, 0.3, 0.3)),
            BackgroundColor(Color::srgb(0.1, 0.1, 0.1)),
        )).with_children(|parent| {
            // Timer bar fill (progress)
            parent.spawn((
                Node {
                    width: Val::Percent(0.0),  // Start at 0%
                    height: Val::Percent(100.0),
                    ..default()
                },
                BackgroundColor(Color::srgb(0.2, 0.8, 0.2)),
                TimerBar,
            ));
        });
        
        // Timer text
        parent.spawn((
            Text::new(""),
            TextFont {
                font_size: 20.0,
                ..default()
            },
            TextColor(Color::WHITE),
            Node {
                position_type: PositionType::Absolute,
                ..default()
            },
            TimerText,
        ));
    });
}

fn check_launch_pad_ownership(
    unit_query: Query<&Unit>,
    launch_pads: Res<LaunchPads>,
    mut game_timer: ResMut<GameTimer>,
    mut game_state: ResMut<GameState>,
    time: Res<Time>,
) {
    // Don't process if game is over
    if game_state.game_over {
        return;
    }
    // Determine ownership of each launch pad
    let mut pad_owners: Vec<LaunchPadOwner> = Vec::new();
    
    for pad in &launch_pads.pads {
        let mut has_red = false;
        let mut has_blue = false;
        
        // Check which armies have units on this pad
        for unit in unit_query.iter() {
            let unit_pos = (unit.q, unit.r);
            if pad.contains(&unit_pos) {
                match unit.army {
                    Army::Red => has_red = true,
                    Army::Blue => has_blue = true,
                }
            }
        }
        
        // Determine ownership
        let owner = if has_red && has_blue {
            LaunchPadOwner::Neutral  // Both armies present
        } else if has_red {
            LaunchPadOwner::Red
        } else if has_blue {
            LaunchPadOwner::Blue
        } else {
            LaunchPadOwner::Neutral  // No units
        };
        
        pad_owners.push(owner);
    }
    
    // Count how many pads each army owns
    let red_count = pad_owners.iter().filter(|&&o| o == LaunchPadOwner::Red).count();
    let blue_count = pad_owners.iter().filter(|&&o| o == LaunchPadOwner::Blue).count();
    let total_pads = pad_owners.len();
    let majority = (total_pads / 2) + 1;
    
    // Check if one army has the majority
    if red_count >= majority {
        if !game_timer.is_active {
            // Red just gained majority from no majority, start timer
            game_timer.is_active = true;
            game_timer.winning_army = Some(Army::Red);
            println!("Red army has majority! Timer started at {:.1}s.", game_timer.time_remaining);
        } else if game_timer.winning_army != Some(Army::Red) {
            // Ownership changed from Blue to Red, don't restart timer
            game_timer.winning_army = Some(Army::Red);
            println!("Ownership changed to Red army! Timer continues at {:.1}s.", game_timer.time_remaining);
        }

        // Tick down timer
        game_timer.time_remaining -= time.delta_secs();
        if game_timer.time_remaining <= 0.0 {
            println!("Red army wins!");
            game_state.game_over = true;
            game_state.winner = Some(Army::Red);
            game_timer.is_active = false;
        }
    } else if blue_count >= majority {
        if !game_timer.is_active {
            // Blue just gained majority from no majority, start timer
            game_timer.is_active = true;
            game_timer.winning_army = Some(Army::Blue);
            println!("Blue army has majority! Timer started at {:.1}s.", game_timer.time_remaining);
        } else if game_timer.winning_army != Some(Army::Blue) {
            // Ownership changed from Red to Blue, don't restart timer
            game_timer.winning_army = Some(Army::Blue);
            println!("Ownership changed to Blue army! Timer continues at {:.1}s.", game_timer.time_remaining);
        }

        // Tick down timer
        game_timer.time_remaining -= time.delta_secs();
        if game_timer.time_remaining <= 0.0 {
            println!("Blue army wins!");
            game_state.game_over = true;
            game_state.winner = Some(Army::Blue);
            game_timer.is_active = false;
        }
    } else {
        // No majority, pause timer but keep time remaining
        if game_timer.is_active {
            println!("No majority. Timer paused at {:.1}s.", game_timer.time_remaining);
        }
        game_timer.is_active = false;
        game_timer.winning_army = None;
        // Don't reset time_remaining - it will resume from current value
    }
}

fn update_timer_ui(
    game_timer: Res<GameTimer>,
    mut bar_query: Query<(&mut Node, &mut BackgroundColor), With<TimerBar>>,
    mut text_query: Query<&mut Text, With<TimerText>>,
) {
    if let Ok((mut node, mut bg_color)) = bar_query.single_mut() {
        if game_timer.is_active {
            // Update progress bar width
            let progress = game_timer.time_remaining / 20.0;
            node.width = Val::Percent(progress * 100.0);
            
            // Change color based on winning army
            *bg_color = match game_timer.winning_army {
                Some(Army::Red) => BackgroundColor(Color::srgb(0.9, 0.2, 0.2)),
                Some(Army::Blue) => BackgroundColor(Color::srgb(0.2, 0.4, 0.9)),
                None => BackgroundColor(Color::srgb(0.2, 0.8, 0.2)),
            };
        } else {
            // Timer inactive, show empty bar
            node.width = Val::Percent(0.0);
        }
    }
    
    if let Ok(mut text) = text_query.single_mut() {
        if game_timer.is_active {
            let winning_army_name = match game_timer.winning_army {
                Some(Army::Red) => "RED",
                Some(Army::Blue) => "BLUE",
                None => "",
            };
            **text = format!("{} {:.1}s", winning_army_name, game_timer.time_remaining);
        } else {
            **text = String::new();
        }
    }
}

fn show_game_over_screen(
    mut commands: Commands,
    game_state: Res<GameState>,
    game_over_query: Query<Entity, With<GameOverScreen>>,
) {
    // If game is over and we haven't shown the screen yet
    if game_state.game_over && game_over_query.is_empty() {
        let winner_name = match game_state.winner {
            Some(Army::Red) => "RED ARMY",
            Some(Army::Blue) => "BLUE ARMY",
            None => "NOBODY",
        };

        let winner_color = match game_state.winner {
            Some(Army::Red) => Color::srgb(0.9, 0.2, 0.2),
            Some(Army::Blue) => Color::srgb(0.2, 0.4, 0.9),
            None => Color::srgb(0.5, 0.5, 0.5),
        };

        // Create full-screen overlay
        commands.spawn((
            Node {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                position_type: PositionType::Absolute,
                justify_content: JustifyContent::Center,
                align_items: AlignItems::Center,
                ..default()
            },
            BackgroundColor(Color::srgba(0.0, 0.0, 0.0, 0.8)),
            GameOverScreen,
        )).with_children(|parent| {
            // Victory message container
            parent.spawn((
                Node {
                    flex_direction: FlexDirection::Column,
                    justify_content: JustifyContent::Center,
                    align_items: AlignItems::Center,
                    padding: UiRect::all(Val::Px(40.0)),
                    border: UiRect::all(Val::Px(4.0)),
                    ..default()
                },
                BackgroundColor(Color::srgb(0.15, 0.15, 0.15)),
                BorderColor::all(winner_color),
            )).with_children(|parent| {
                // "VICTORY!" text
                parent.spawn((
                    Text::new("VICTORY!"),
                    TextFont {
                        font_size: 80.0,
                        ..default()
                    },
                    TextColor(winner_color),
                    Node {
                        margin: UiRect::bottom(Val::Px(20.0)),
                        ..default()
                    },
                ));

                // Winner text
                parent.spawn((
                    Text::new(format!("{} WINS!", winner_name)),
                    TextFont {
                        font_size: 50.0,
                        ..default()
                    },
                    TextColor(Color::WHITE),
                    Node {
                        margin: UiRect::bottom(Val::Px(30.0)),
                        ..default()
                    },
                ));

                // Restart instruction
                parent.spawn((
                    Text::new("Press SPACE to restart"),
                    TextFont {
                        font_size: 24.0,
                        ..default()
                    },
                    TextColor(Color::srgb(0.7, 0.7, 0.7)),
                ));
            });
        });

        println!("Game over screen displayed!");
    }
}

fn handle_restart(
    mut commands: Commands,
    keyboard: Res<ButtonInput<KeyCode>>,
    mut game_state: ResMut<GameState>,
    mut game_timer: ResMut<GameTimer>,
    game_over_query: Query<Entity, With<GameOverScreen>>,
    children_query: Query<&Children>,
) {
    // Check if space bar is pressed and game is over
    if game_state.game_over && keyboard.just_pressed(KeyCode::Space) {
        println!("Restarting game...");

        // Reset game state
        game_state.game_over = false;
        game_state.winner = None;

        // Reset timer
        game_timer.time_remaining = 20.0;
        game_timer.is_active = false;
        game_timer.winning_army = None;

        // Remove game over screen - collect all descendants first
        for entity in game_over_query.iter() {
            let mut to_despawn = vec![entity];

            // Recursively collect all descendants
            let mut i = 0;
            while i < to_despawn.len() {
                if let Ok(children) = children_query.get(to_despawn[i]) {
                    for child in children.iter() {
                        to_despawn.push(child);
                    }
                }
                i += 1;
            }

            // Despawn in reverse order (children first)
            for e in to_despawn.into_iter().rev() {
                commands.entity(e).despawn();
            }
        }

        println!("Game restarted!");
    }
}
