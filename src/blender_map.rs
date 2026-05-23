use bevy::prelude::*;
use bevy::scene::SceneInstanceReady;
use bevy_skein::SkeinPlugin;

use crate::loading::{LoadingState, SelectedMap};
use crate::map::{world_pos_to_axial, HexMapConfig, HexTile as MapHexTile};
use crate::map_loader::MapDefinition;
use crate::ui::GameCamera;
use crate::units::Army;

// ---------------------------------------------------------------------------
// Marker components (applied in Blender via bevy_skein)
// ---------------------------------------------------------------------------

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
pub struct HexTile;

#[derive(Component, Reflect, Debug, Default)]
#[reflect(Component, Default)]
pub struct SpawnPoint {
    pub team: Army,
}

#[derive(Component, Reflect, Debug, Default)]
#[reflect(Component, Default)]
pub struct HqMarker {
    pub team: Army,
}

#[derive(Component, Reflect, Debug, Default)]
#[reflect(Component, Default)]
pub struct LaunchPadMarker {
    pub id: u32,
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
pub struct CrystalMarker;

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
pub struct Blocked;

#[derive(Component, Reflect, Debug, Default)]
#[reflect(Component, Default)]
pub struct CameraShift {
    pub x: f32,
    pub y: f32,
}

/// Blender hex radius=1.0, game HEX_RADIUS=64.0.
const BLENDER_SCALE: f32 = 64.0;

/// Inserted when a .glb map is loaded so other systems can skip procedural visuals.
#[derive(Resource)]
pub struct BlenderMapActive;

/// Set by the SceneInstanceReady observer so extraction runs one frame later
/// (after bevy_skein has flushed its deferred component insertions).
#[derive(Resource)]
struct BlenderSceneReady(Entity);

// ---------------------------------------------------------------------------
// Plugin
// ---------------------------------------------------------------------------

pub struct BlenderMapPlugin;

impl Plugin for BlenderMapPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(SkeinPlugin::default())
            .register_type::<HexTile>()
            .register_type::<SpawnPoint>()
            .register_type::<HqMarker>()
            .register_type::<LaunchPadMarker>()
            .register_type::<CrystalMarker>()
            .register_type::<Blocked>()
            .register_type::<CameraShift>()
            .register_type::<Army>()
            .add_systems(OnEnter(LoadingState::Loading), load_blender_map)
            .add_systems(
                Update,
                extract_map_definition.run_if(
                    in_state(LoadingState::Loading).and(resource_exists::<BlenderSceneReady>),
                ),
            )
            .add_systems(OnExit(LoadingState::Playing), cleanup_blender_map)
            .add_observer(on_scene_ready);
    }
}

// ---------------------------------------------------------------------------
// GLB scene loading
// ---------------------------------------------------------------------------

fn load_blender_map(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    selected_map: Res<SelectedMap>,
) {
    if !selected_map.0.ends_with(".glb") && !selected_map.0.ends_with(".gltf") {
        return;
    }

    commands.insert_resource(BlenderMapActive);

    let path = selected_map.0.strip_prefix("assets/")
        .unwrap_or(&selected_map.0)
        .to_string();
    commands.spawn((
        SceneRoot(asset_server.load(
            GltfAssetLabel::Scene(0).from_asset(path),
        )),
        Transform::from_scale(Vec3::splat(BLENDER_SCALE)),
        DespawnOnExit(LoadingState::Playing),
    ));

    info!("blender_map: spawning GLB scene from {}", selected_map.0);
}

fn cleanup_blender_map(mut commands: Commands) {
    commands.remove_resource::<BlenderMapActive>();
    commands.remove_resource::<BlenderSceneReady>();
}

fn on_scene_ready(
    event: On<SceneInstanceReady>,
    mut commands: Commands,
    blender_active: Option<Res<BlenderMapActive>>,
) {
    if blender_active.is_none() {
        return;
    }
    let scene_entity = event.event().entity;
    info!("blender_map: scene ready, deferring extraction to next frame");
    commands.insert_resource(BlenderSceneReady(scene_entity));
}

// ---------------------------------------------------------------------------
// Extract MapDefinition (runs one frame after SceneInstanceReady so skein
// components have been flushed)
// ---------------------------------------------------------------------------

fn extract_map_definition(
    mut commands: Commands,
    mut map_def: ResMut<MapDefinition>,
    mut map_config: ResMut<HexMapConfig>,
    scene_ready: Res<BlenderSceneReady>,
    children_query: Query<&Children>,
    global_transforms: Query<&GlobalTransform>,
    meshes: Query<(), With<Mesh3d>>,
    spawn_points: Query<&SpawnPoint>,
    hq_markers: Query<&HqMarker>,
    launch_pads: Query<&LaunchPadMarker>,
    crystal_markers: Query<(), With<CrystalMarker>>,
    blocked_markers: Query<(), With<Blocked>>,
    cameras: Query<((), Option<&CameraShift>), With<Camera3d>>,
    mut point_lights: Query<&mut PointLight>,
    mut spot_lights: Query<&mut SpotLight>,
    mut dir_lights: Query<&mut DirectionalLight>,
) {
    let scene_entity = scene_ready.0;
    commands.remove_resource::<BlenderSceneReady>();
    info!("blender_map: extracting MapDefinition from scene");

    let all_descendants: Vec<Entity> = collect_descendants(scene_entity, &children_query);

    for entity in &all_descendants {
        if meshes.get(*entity).is_err() {
            continue;
        }
        let Ok(gt) = global_transforms.get(*entity) else { continue };
        let pos = gt.translation();
        let (q, r) = world_pos_to_axial(pos.x, pos.z);

        map_def.tile_map.insert((q, r), 1);
        map_config.cell_world_pos.insert((q, r), Vec3::new(pos.x, 0.0, pos.z));
        commands.entity(*entity).insert(MapHexTile { q, r, _height: 0.0 });

        if let Ok(sp) = spawn_points.get(*entity) {
            match sp.team {
                Army::Red => map_def.spawn_red.push((q, r)),
                Army::Blue => map_def.spawn_blue.push((q, r)),
            }
        }

        if let Ok(hq) = hq_markers.get(*entity) {
            match hq.team {
                Army::Red => {
                    map_def.hq_red = Some((q, r));
                    map_def.obstacles.insert((q, r));
                }
                Army::Blue => {
                    map_def.hq_blue = Some((q, r));
                    map_def.obstacles.insert((q, r));
                }
            }
        }

        if blocked_markers.get(*entity).is_ok() {
            map_def.obstacles.insert((q, r));
        }

        if crystal_markers.get(*entity).is_ok() {
            map_def.crystal_fields.push((q, r));
        }
    }

    for entity in &all_descendants {
        let Ok(pad) = launch_pads.get(*entity) else { continue };
        let pad_descendants = collect_descendants(*entity, &children_query);
        let mut cells = Vec::new();
        for child in &pad_descendants {
            if meshes.get(*child).is_ok() {
                if let Ok(gt) = global_transforms.get(*child) {
                    let pos = gt.translation();
                    let (q, r) = world_pos_to_axial(pos.x, pos.z);
                    cells.push((q, r));
                    map_def.launch_pad_cells.insert((q, r));
                    commands.entity(*child).insert(MapHexTile { q, r, _height: 0.0 });
                }
            }
        }
        if !cells.is_empty() {
            let id = pad.id as usize;
            if id >= map_def.launch_pads.len() {
                map_def.launch_pads.resize(id + 1, Vec::new());
            }
            map_def.launch_pads[id] = cells;
        }
    }

    // Camera inherits 64x scale from scene root, so use Blender-space viewport
    // height (9.0) — the inherited scale handles the world-space conversion.
    // CameraShift component on the camera carries Blender's Shift X/Y values
    // (lost during glTF export) and maps them to viewport_origin.
    for entity in &all_descendants {
        if let Ok((_, shift)) = cameras.get(*entity) {
            let origin = Vec2::new(
                0.5 - shift.map_or(0.0, |s| s.x),
                0.5 - shift.map_or(0.0, |s| s.y),
            );
            commands.entity(*entity).insert((
                GameCamera,
                Projection::Orthographic(OrthographicProjection {
                    scaling_mode: bevy::camera::ScalingMode::FixedVertical {
                        viewport_height: 9.0,
                    },
                    viewport_origin: origin,
                    scale: 1.0,
                    far: 5000.0,
                    ..OrthographicProjection::default_3d()
                }),
            ));
            break;
        }
    }

    // Compensate for 64x scene scale: point/spot lights follow inverse-square
    // falloff, so intensity must increase by BLENDER_SCALE² to match Blender.
    let scale_sq = BLENDER_SCALE * BLENDER_SCALE;
    for entity in &all_descendants {
        if let Ok(mut light) = point_lights.get_mut(*entity) {
            light.intensity *= scale_sq;
            light.range *= BLENDER_SCALE;
        }
        if let Ok(mut light) = spot_lights.get_mut(*entity) {
            light.intensity *= scale_sq;
            light.range *= BLENDER_SCALE;
        }
        if let Ok(mut light) = dir_lights.get_mut(*entity) {
            light.illuminance *= scale_sq;
        }
    }

    map_def.loaded = true;

    info!(
        "blender_map: extracted {} tiles, {} obstacles, hq_red={:?}, hq_blue={:?}, \
         spawn_red={}, spawn_blue={}, {} launch pads, {} crystal fields",
        map_def.tile_map.len(),
        map_def.obstacles.len(),
        map_def.hq_red,
        map_def.hq_blue,
        map_def.spawn_red.len(),
        map_def.spawn_blue.len(),
        map_def.launch_pads.len(),
        map_def.crystal_fields.len(),
    );
}

fn collect_descendants(root: Entity, children_query: &Query<&Children>) -> Vec<Entity> {
    let mut result = Vec::new();
    let mut stack = vec![root];
    while let Some(entity) = stack.pop() {
        if let Ok(children) = children_query.get(entity) {
            for child in children.iter() {
                result.push(child);
                stack.push(child);
            }
        }
    }
    result
}
