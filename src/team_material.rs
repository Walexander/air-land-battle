use bevy::prelude::*;
use bevy::render::render_resource::AsBindGroup;

use crate::blender_map::LaunchPadMarker;
use crate::launch_pads::{LaunchPadOwner, LaunchPadOwnership};
use crate::loading::LoadingState;
use crate::units::Army;

#[derive(Asset, AsBindGroup, TypePath, Debug, Clone)]
pub struct TeamMaterial {
    #[uniform(0)]
    pub team_color: LinearRgba,

    #[texture(1)]
    #[sampler(2)]
    pub base_texture: Handle<Image>,

    #[texture(3)]
    #[sampler(4)]
    pub mask_texture: Handle<Image>,
}

impl Material for TeamMaterial {
    fn fragment_shader() -> bevy::shader::ShaderRef {
        "shaders/team_material.wgsl".into()
    }

    fn alpha_mode(&self) -> AlphaMode {
        AlphaMode::Opaque
    }
}

#[derive(Component, Reflect, Debug, Default)]
#[reflect(Component, Default)]
pub struct UseTeamMaterial {
    pub team: Army,
    pub mask: String,
}

#[derive(Component)]
struct PendingMaskLoad(Handle<Image>);

#[derive(Component)]
pub struct LaunchPadTeamColor {
    pub pad_id: usize,
    pub materials: Vec<Handle<TeamMaterial>>,
}

pub struct TeamMaterialPlugin;

impl Plugin for TeamMaterialPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(MaterialPlugin::<TeamMaterial>::default())
            .register_type::<UseTeamMaterial>()
            .add_systems(
                Update,
                (start_mask_loads, apply_team_materials).chain().run_if(
                    in_state(LoadingState::Loading).or(in_state(LoadingState::Playing))
                ),
            )
            .add_systems(
                Update,
                update_launch_pad_team_colors.run_if(in_state(LoadingState::Playing)),
            );
    }
}

const RED_TEAM: LinearRgba = LinearRgba::new(0.769, 0.145, 0.110, 1.0);
const BLUE_TEAM: LinearRgba = LinearRgba::new(0.129, 0.188, 0.729, 1.0);

fn team_color_for_army(army: Army) -> LinearRgba {
    match army {
        Army::Red => RED_TEAM,
        Army::Blue => BLUE_TEAM,
    }
}

fn team_color_for_owner(owner: LaunchPadOwner) -> LinearRgba {
    match owner {
        LaunchPadOwner::Red => RED_TEAM,
        LaunchPadOwner::Blue => BLUE_TEAM,
        LaunchPadOwner::Neutral => LinearRgba::new(0.5, 0.5, 0.5, 1.0),
        LaunchPadOwner::Contested => LinearRgba::new(0.8, 0.7, 0.0, 1.0),
    }
}

const NEUTRAL_COLOR: LinearRgba = LinearRgba::new(0.5, 0.5, 0.5, 1.0);

fn find_material_children(
    root: Entity,
    children_query: &Query<&Children>,
    child_materials: &Query<&MeshMaterial3d<StandardMaterial>>,
) -> Vec<Entity> {
    let mut result = Vec::new();
    let mut stack = vec![root];
    while let Some(entity) = stack.pop() {
        if let Ok(children) = children_query.get(entity) {
            for child in children.iter() {
                if child_materials.get(child).is_ok() {
                    result.push(child);
                } else {
                    stack.push(child);
                }
            }
        }
    }
    result
}

fn start_mask_loads(
    mut commands: Commands,
    query: Query<(Entity, &UseTeamMaterial), Without<PendingMaskLoad>>,
    asset_server: Res<AssetServer>,
) {
    for (entity, use_team) in &query {
        let mask_path = use_team.mask.strip_prefix("assets/").unwrap_or(&use_team.mask).to_string();
        if mask_path.is_empty() {
            warn!("team_material: {:?} has empty mask path, removing", entity);
            commands.entity(entity).remove::<UseTeamMaterial>();
            continue;
        }
        let handle: Handle<Image> = asset_server.load(&mask_path);
        info!("team_material: {:?} started loading mask={}", entity, mask_path);
        commands.entity(entity).insert(PendingMaskLoad(handle));
    }
}

fn apply_team_materials(
    mut commands: Commands,
    query: Query<(Entity, &UseTeamMaterial, &PendingMaskLoad, Option<&ChildOf>)>,
    children_query: Query<&Children>,
    child_materials: Query<&MeshMaterial3d<StandardMaterial>>,
    standard_materials: Res<Assets<StandardMaterial>>,
    mut team_materials: ResMut<Assets<TeamMaterial>>,
    asset_server: Res<AssetServer>,
    launch_pad_query: Query<&LaunchPadMarker>,
) {
    for (entity, use_team, pending, parent) in &query {
        if !matches!(asset_server.get_load_state(pending.0.id()), Some(bevy::asset::LoadState::Loaded)) {
            continue;
        }

        let mat_children = find_material_children(entity, &children_query, &child_materials);
        if mat_children.is_empty() {
            continue;
        }

        let is_launch_pad = launch_pad_query.get(entity).ok()
            .or_else(|| parent.and_then(|p| launch_pad_query.get(p.parent()).ok()));
        let team_color = if is_launch_pad.is_some() {
            NEUTRAL_COLOR
        } else {
            team_color_for_army(use_team.team)
        };

        let mut pad_materials = Vec::new();
        for mesh_entity in &mat_children {
            let base_handle = child_materials.get(*mesh_entity).unwrap();
            let Some(base_mat) = standard_materials.get(&base_handle.0) else { continue };
            let Some(base_tex) = base_mat.base_color_texture.clone() else { continue };

            let mat = team_materials.add(TeamMaterial {
                team_color,
                base_texture: base_tex,
                mask_texture: pending.0.clone(),
            });

            commands.entity(*mesh_entity).insert(MeshMaterial3d(mat.clone()));
            pad_materials.push(mat);
        }

        if let Some(pad) = is_launch_pad {
            if !pad_materials.is_empty() {
                commands.entity(entity).insert(LaunchPadTeamColor {
                    pad_id: pad.id as usize,
                    materials: pad_materials,
                });
            }
        }

        info!("team_material: applied to {:?} ({} meshes) launch_pad={:?}",
            entity, mat_children.len(), is_launch_pad.map(|p| p.id));
        commands.entity(entity).remove::<UseTeamMaterial>();
        commands.entity(entity).remove::<PendingMaskLoad>();
    }
}

fn update_launch_pad_team_colors(
    pad_ownership: Res<LaunchPadOwnership>,
    query: Query<&LaunchPadTeamColor>,
    mut team_materials: ResMut<Assets<TeamMaterial>>,
) {
    if !pad_ownership.is_changed() {
        return;
    }

    for pad_color in &query {
        let owner = pad_ownership.owners.get(pad_color.pad_id)
            .copied()
            .unwrap_or(LaunchPadOwner::Neutral);
        let color = team_color_for_owner(owner);

        for handle in &pad_color.materials {
            if let Some(mat) = team_materials.get_mut(handle) {
                mat.team_color = color;
            }
        }
    }
}
