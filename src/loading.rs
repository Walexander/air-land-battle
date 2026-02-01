use bevy::prelude::*;
use bevy::animation::AnimationClip;

#[derive(States, Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum LoadingState {
    #[default]
    Loading,
    Playing,
}

#[derive(Resource)]
pub struct AssetsLoading {
    pub handles: Vec<UntypedHandle>,
}

#[derive(Component)]
pub struct LoadingScreen;

#[derive(Component)]
pub struct LoadingProgressBar;

fn setup_loading_screen(mut commands: Commands) {
    // Spawn loading screen UI (Camera2d already exists from Startup)
    commands.spawn((
        Node {
            width: Val::Percent(100.0),
            height: Val::Percent(100.0),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            flex_direction: FlexDirection::Column,
            ..default()
        },
        BackgroundColor(Color::srgb(0.1, 0.1, 0.1)),
        LoadingScreen,
    ))
    .with_children(|parent| {
        parent.spawn((
            Text::new("LOADING"),
            TextFont {
                font_size: 64.0,
                ..default()
            },
            TextColor(Color::WHITE),
        ));

        // Progress bar outer border
        parent.spawn((
            Node {
                width: Val::Px(404.0),
                height: Val::Px(12.0),
                margin: UiRect::top(Val::Px(20.0)),
                justify_content: JustifyContent::Center,
                align_items: AlignItems::Center,
                ..default()
            },
            BackgroundColor(Color::WHITE),
        ))
        .with_children(|parent| {
            // Progress bar container
            parent.spawn((
                Node {
                    width: Val::Px(400.0),
                    height: Val::Px(8.0),
                    ..default()
                },
                BackgroundColor(Color::srgb(0.2, 0.2, 0.2)),
            ))
            .with_children(|parent| {
                // Progress bar fill
                parent.spawn((
                    Node {
                        width: Val::Percent(0.0),
                        height: Val::Percent(100.0),
                        ..default()
                    },
                    BackgroundColor(Color::WHITE),
                    LoadingProgressBar,
                ));
            });
        });
    });
}

fn preload_assets(mut commands: Commands, asset_server: Res<AssetServer>) {
    // Load all GLTF models used in the game
    let handles = vec![
        asset_server.load::<Scene>("Fox.glb#Scene0").untyped(),
        asset_server.load::<Scene>("walking-rifle.glb#Scene0").untyped(),
        asset_server.load::<Scene>("CesiumMan.glb#Scene0").untyped(),
        asset_server.load::<Scene>("Tractor.glb#Scene0").untyped(),
        asset_server.load::<Scene>("Lighthing Crystal.glb#Scene0").untyped(),
        // Load animations (only the ones that actually exist in each file)
        asset_server.load::<AnimationClip>("Fox.glb#Animation0").untyped(),
        asset_server.load::<AnimationClip>("Fox.glb#Animation2").untyped(),
        asset_server.load::<AnimationClip>("walking-rifle.glb#Animation0").untyped(),
        // Note: walking-rifle.glb only has Animation0, not Animation2
        asset_server.load::<AnimationClip>("CesiumMan.glb#Animation0").untyped(),
        // Note: Tractor.glb has no animations
    ];

    commands.insert_resource(AssetsLoading { handles });
}

fn check_assets_ready(
    mut next_state: ResMut<NextState<LoadingState>>,
    assets_loading: Res<AssetsLoading>,
    asset_server: Res<AssetServer>,
    mut progress_bar_query: Query<&mut Node, With<LoadingProgressBar>>,
) {
    // Count loaded assets
    let loaded_count = assets_loading.handles.iter()
        .filter(|handle| {
            matches!(
                asset_server.get_load_state(handle.id()),
                Some(bevy::asset::LoadState::Loaded)
            )
        })
        .count();

    let total_count = assets_loading.handles.len();
    let progress = (loaded_count as f32 / total_count as f32) * 100.0;

    // Update progress bar
    if let Ok(mut node) = progress_bar_query.single_mut() {
        node.width = Val::Percent(progress);
    }

    // Transition to playing when all loaded
    if loaded_count == total_count {
        println!("All assets loaded! Starting game...");
        next_state.set(LoadingState::Playing);
    }
}

fn cleanup_loading_screen(
    mut commands: Commands,
    loading_screen_query: Query<Entity, With<LoadingScreen>>,
) {
    for entity in &loading_screen_query {
        commands.entity(entity).despawn();
    }
}

fn setup_persistent_camera(mut commands: Commands) {
    // Spawn Camera2d that exists from the very start for egui
    // It will also render the loading screen UI
    // Order 1 so it renders on top of Camera3d (which will have order 0)
    commands.spawn((
        Camera2d,
        Camera {
            order: 1,
            ..default()
        },
    ));
}

pub struct LoadingPlugin;

impl Plugin for LoadingPlugin {
    fn build(&self, app: &mut App) {
        app.init_state::<LoadingState>()
            .add_systems(Startup, setup_persistent_camera)
            .add_systems(OnEnter(LoadingState::Loading), (setup_loading_screen, preload_assets))
            .add_systems(Update, check_assets_ready.run_if(in_state(LoadingState::Loading)))
            .add_systems(OnEnter(LoadingState::Playing), cleanup_loading_screen);
    }
}
