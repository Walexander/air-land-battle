use bevy::prelude::*;
use bevy::animation::AnimationClip;

#[derive(States, Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum LoadingState {
    #[default]
    TitleScreen,
    Lobby,
    Loading,
    Playing,
}

/// The map file the player chose on the title screen.
#[derive(Resource)]
pub struct SelectedMap(pub String);

/// Marker for the "Host Game" button on the title screen.
#[derive(Component)]
pub struct HostButton;

/// Marker for the "Join Game" button on the title screen.
#[derive(Component)]
pub struct JoinButton;

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
pub struct MapButton(pub String);

// ---------------------------------------------------------------------------
// Title screen
// ---------------------------------------------------------------------------

pub const MAPS: &[(&str, &str)] = &[
    ("Hex Map (Blender)",  "assets/HexMap.glb"),
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
            row_gap: Val::Px(32.0),
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
        ));

        // Row: buttons on left, minimap on right
        parent.spawn(Node {
            flex_direction: FlexDirection::Row,
            align_items: AlignItems::Center,
            column_gap: Val::Px(48.0),
            ..default()
        })
        .with_children(|row| {
            // Left column: subtitle + map buttons
            row.spawn(Node {
                flex_direction: FlexDirection::Column,
                align_items: AlignItems::Center,
                row_gap: Val::Px(14.0),
                ..default()
            })
            .with_children(|col| {
                col.spawn((
                    Text::new("Select a Map"),
                    TextFont { font_size: 26.0, ..default() },
                    TextColor(Color::srgb(0.7, 0.7, 0.7)),
                    Node { margin: UiRect::bottom(Val::Px(8.0)), ..default() },
                ));

                for &(label, path) in MAPS {
                    col.spawn((
                        Button,
                        Node {
                            width: Val::Px(340.0),
                            height: Val::Px(56.0),
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
                            TextFont { font_size: 24.0, ..default() },
                            TextColor(Color::WHITE),
                        ));
                    });
                }

                // Multiplayer divider
                col.spawn((
                    Node {
                        width: Val::Px(340.0),
                        height: Val::Px(1.0),
                        margin: UiRect::vertical(Val::Px(8.0)),
                        ..default()
                    },
                    BackgroundColor(Color::srgb(0.3, 0.3, 0.5)),
                ));

                col.spawn((
                    Text::new("Multiplayer"),
                    TextFont { font_size: 18.0, ..default() },
                    TextColor(Color::srgb(0.6, 0.6, 0.8)),
                    Node { margin: UiRect::bottom(Val::Px(4.0)), ..default() },
                ));

                // Host Game button
                col.spawn((
                    Button,
                    Node {
                        width: Val::Px(340.0),
                        height: Val::Px(50.0),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        border: UiRect::all(Val::Px(2.0)),
                        ..default()
                    },
                    BackgroundColor(Color::srgb(0.1, 0.2, 0.15)),
                    BorderColor::all(Color::srgb(0.3, 0.6, 0.4)),
                    HostButton,
                ))
                .with_children(|btn| {
                    btn.spawn((
                        Text::new("Host Game"),
                        TextFont { font_size: 22.0, ..default() },
                        TextColor(Color::srgb(0.5, 1.0, 0.6)),
                    ));
                });

                // Join Game button
                col.spawn((
                    Button,
                    Node {
                        width: Val::Px(340.0),
                        height: Val::Px(50.0),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        border: UiRect::all(Val::Px(2.0)),
                        margin: UiRect::top(Val::Px(4.0)),
                        ..default()
                    },
                    BackgroundColor(Color::srgb(0.1, 0.15, 0.25)),
                    BorderColor::all(Color::srgb(0.3, 0.4, 0.7)),
                    JoinButton,
                ))
                .with_children(|btn| {
                    btn.spawn((
                        Text::new("Join Game"),
                        TextFont { font_size: 22.0, ..default() },
                        TextColor(Color::srgb(0.5, 0.7, 1.0)),
                    ));
                });
            });

            // Right column: minimap preview panel
            row.spawn((
                Node {
                    width: Val::Px(508.0),
                    height: Val::Px(259.0),
                    border: UiRect::all(Val::Px(2.0)),
                    justify_content: JustifyContent::Center,
                    align_items: AlignItems::Center,
                    ..default()
                },
                BackgroundColor(Color::srgb(0.08, 0.08, 0.15)),
                BorderColor::all(Color::srgb(0.35, 0.35, 0.55)),
                crate::minimap::MinimapPanel,
            ))
            .with_children(|panel| {
                // The image node sits inside, sized to fill the panel
                panel.spawn((
                    Node {
                        width: Val::Percent(100.0),
                        height: Val::Percent(100.0),
                        ..default()
                    },
                    ImageNode::default(),
                    crate::minimap::MinimapDisplay,
                ));
            });
        });
    });
}

fn handle_title_screen_buttons(
    mut commands: Commands,
    mut next_state: ResMut<NextState<LoadingState>>,
    mut map_query: Query<
        (&Interaction, &MapButton, &mut BackgroundColor),
        (Changed<Interaction>, With<Button>),
    >,
    mut host_query: Query<
        (&Interaction, &mut BackgroundColor),
        (Changed<Interaction>, With<HostButton>, Without<MapButton>),
    >,
    mut join_query: Query<
        (&Interaction, &mut BackgroundColor),
        (Changed<Interaction>, With<JoinButton>, Without<MapButton>, Without<HostButton>),
    >,
    mut mode: ResMut<crate::networking::MultiplayerMode>,
) {
    for (interaction, map_button, mut bg) in &mut map_query {
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

    for (interaction, mut bg) in &mut host_query {
        match interaction {
            Interaction::Pressed => {
                *mode = crate::networking::MultiplayerMode::Host;
                next_state.set(LoadingState::Lobby);
            }
            Interaction::Hovered => {
                *bg = BackgroundColor(Color::srgb(0.15, 0.3, 0.2));
            }
            Interaction::None => {
                *bg = BackgroundColor(Color::srgb(0.1, 0.2, 0.15));
            }
        }
    }

    for (interaction, mut bg) in &mut join_query {
        match interaction {
            Interaction::Pressed => {
                *mode = crate::networking::MultiplayerMode::Client;
                next_state.set(LoadingState::Lobby);
            }
            Interaction::Hovered => {
                *bg = BackgroundColor(Color::srgb(0.15, 0.2, 0.35));
            }
            Interaction::None => {
                *bg = BackgroundColor(Color::srgb(0.1, 0.15, 0.25));
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
