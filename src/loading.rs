use bevy::prelude::*;
use bevy::animation::AnimationClip;

#[derive(States, Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum LoadingState {
    #[default]
    TitleScreen,
    Loading,
    Playing,
}

/// The map file the player chose on the title screen.
#[derive(Resource)]
pub struct SelectedMap(pub String);

#[derive(Resource)]
pub struct AssetsLoading {
    pub handles: Vec<UntypedHandle>,
}

#[derive(Component)]
pub struct LoadingScreen;

#[derive(Component)]
pub struct LoadingProgressBar;

#[derive(Component)]
struct TitleScreen;

#[derive(Component)]
struct MapButton(String);

// ---------------------------------------------------------------------------
// Title screen
// ---------------------------------------------------------------------------

const MAPS: &[(&str, &str)] = &[
    ("Topsy Turvy",        "assets/maps/Topsy Turvy.tmx"),
    ("The Bullseye",       "assets/maps/The Bullseye.tmx"),
    ("Frozen Road",        "assets/maps/Frozen Road.tmx"),
    ("Side By Side By Side", "assets/maps/Side By Side By Side.tmx"),
];

fn setup_title_screen(mut commands: Commands) {
    commands.spawn((
        Node {
            width: Val::Percent(100.0),
            height: Val::Percent(100.0),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            flex_direction: FlexDirection::Column,
            row_gap: Val::Px(16.0),
            ..default()
        },
        BackgroundColor(Color::srgb(0.05, 0.05, 0.1)),
        TitleScreen,
    ))
    .with_children(|parent| {
        // Title
        parent.spawn((
            Text::new("AIR LAND BATTLE"),
            TextFont { font_size: 72.0, ..default() },
            TextColor(Color::srgb(0.9, 0.85, 0.5)),
            Node {
                margin: UiRect::bottom(Val::Px(8.0)),
                ..default()
            },
        ));

        // Subtitle
        parent.spawn((
            Text::new("Select a Map"),
            TextFont { font_size: 28.0, ..default() },
            TextColor(Color::srgb(0.7, 0.7, 0.7)),
            Node {
                margin: UiRect::bottom(Val::Px(32.0)),
                ..default()
            },
        ));

        // Map buttons
        for &(label, path) in MAPS {
            parent.spawn((
                Button,
                Node {
                    width: Val::Px(360.0),
                    height: Val::Px(60.0),
                    justify_content: JustifyContent::Center,
                    align_items: AlignItems::Center,
                    border: UiRect::all(Val::Px(2.0)),
                    ..default()
                },
                BackgroundColor(Color::srgb(0.15, 0.15, 0.25)),
                BorderColor::all(Color::srgb(0.4, 0.4, 0.6)),
                MapButton(path.to_string()),
            ))
            .with_children(|btn| {
                btn.spawn((
                    Text::new(label),
                    TextFont { font_size: 26.0, ..default() },
                    TextColor(Color::WHITE),
                ));
            });
        }
    });
}

fn handle_title_screen_buttons(
    mut commands: Commands,
    mut next_state: ResMut<NextState<LoadingState>>,
    mut interaction_query: Query<
        (&Interaction, &MapButton, &mut BackgroundColor),
        (Changed<Interaction>, With<Button>),
    >,
) {
    for (interaction, map_button, mut bg) in &mut interaction_query {
        match interaction {
            Interaction::Pressed => {
                commands.insert_resource(SelectedMap(map_button.0.clone()));
                next_state.set(LoadingState::Loading);
            }
            Interaction::Hovered => {
                *bg = BackgroundColor(Color::srgb(0.25, 0.25, 0.4));
            }
            Interaction::None => {
                *bg = BackgroundColor(Color::srgb(0.15, 0.15, 0.25));
            }
        }
    }
}

fn cleanup_title_screen(
    mut commands: Commands,
    query: Query<Entity, With<TitleScreen>>,
) {
    for entity in &query {
        commands.entity(entity).despawn();
    }
}

// ---------------------------------------------------------------------------
// Loading screen
// ---------------------------------------------------------------------------

fn setup_loading_screen(mut commands: Commands) {
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
    let handles = vec![
        asset_server.load::<Scene>("Fox.glb#Scene0").untyped(),
        asset_server.load::<Scene>("walking-rifle.glb#Scene0").untyped(),
        asset_server.load::<Scene>("CesiumMan.glb#Scene0").untyped(),
        asset_server.load::<Scene>("Tractor.glb#Scene0").untyped(),
        asset_server.load::<Scene>("Lighthing Crystal.glb#Scene0").untyped(),
        asset_server.load::<AnimationClip>("Fox.glb#Animation0").untyped(),
        asset_server.load::<AnimationClip>("Fox.glb#Animation2").untyped(),
        asset_server.load::<AnimationClip>("walking-rifle.glb#Animation0").untyped(),
        asset_server.load::<AnimationClip>("CesiumMan.glb#Animation0").untyped(),
    ];

    commands.insert_resource(AssetsLoading { handles });
}

fn check_assets_ready(
    mut next_state: ResMut<NextState<LoadingState>>,
    assets_loading: Res<AssetsLoading>,
    asset_server: Res<AssetServer>,
    mut progress_bar_query: Query<&mut Node, With<LoadingProgressBar>>,
    map_def: Option<Res<crate::map_loader::MapDefinition>>,
) {
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

    if let Ok(mut node) = progress_bar_query.single_mut() {
        node.width = Val::Percent(progress);
    }

    let map_loaded = map_def.map(|m| m.loaded).unwrap_or(false);
    if loaded_count == total_count && map_loaded {
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
            .add_systems(OnEnter(LoadingState::TitleScreen), (setup_title_screen, preload_assets))
            .add_systems(
                Update,
                handle_title_screen_buttons.run_if(in_state(LoadingState::TitleScreen)),
            )
            .add_systems(OnEnter(LoadingState::Loading), (cleanup_title_screen, setup_loading_screen))
            .add_systems(Update, check_assets_ready.run_if(in_state(LoadingState::Loading)))
            .add_systems(OnEnter(LoadingState::Playing), cleanup_loading_screen);
    }
}
