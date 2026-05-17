use bevy::prelude::*;
use bevy::diagnostic::{FrameTimeDiagnosticsPlugin, DiagnosticsStore};
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_inspector_egui::bevy_egui::EguiPlugin;
use bevy_sprinkles::SprinklesPlugin;

mod map;
mod map_loader;
mod minimap;
mod units;
mod selection;
mod launch_pads;
mod ui;
mod loading;
mod ai;
mod economy;
mod music;
mod hex_pathfinding;
mod networking;
mod lobby;
mod net_sync;

use map::MapPlugin;
use map_loader::MapLoaderPlugin;
use minimap::MinimapPlugin;
use units::UnitsPlugin;
use selection::SelectionPlugin;
use launch_pads::LaunchPadsPlugin;
use ui::UIPlugin;
use loading::LoadingPlugin;
use ai::AIPlugin;
use economy::EconomyPlugin;
use music::MusicPlugin;
use networking::NetworkingPlugin;
use lobby::LobbyPlugin;
use net_sync::NetSyncPlugin;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                resolution: bevy::window::WindowResolution::new(1500, 720),
                title: "Air Land Battle".to_string(),
                present_mode: bevy::window::PresentMode::AutoVsync,
                ..default()
            }),
            ..default()
        }))
        .add_plugins(FrameTimeDiagnosticsPlugin::default())
        .add_plugins(EguiPlugin::default())
        .add_plugins(WorldInspectorPlugin::default().run_if(inspector_enabled))
        .add_plugins(bevy_mod_outline::OutlinePlugin)
        .add_plugins(NetworkingPlugin)
        .add_plugins(LobbyPlugin)
        .add_plugins(NetSyncPlugin)
        .add_plugins(LoadingPlugin)
        .add_plugins(MinimapPlugin)
        .add_plugins(MapLoaderPlugin)
        .add_plugins(LaunchPadsPlugin)
        .add_plugins(MapPlugin)
        .add_plugins(EconomyPlugin)
        .add_plugins(UnitsPlugin)
        .add_plugins(AIPlugin)
        .add_plugins(SelectionPlugin)
        .add_plugins(UIPlugin)
        .add_plugins(MusicPlugin)
        .add_plugins(SprinklesPlugin)
        .insert_resource(bevy::light::PointLightShadowMap { size: 2048 })
        .insert_resource(InspectorEnabled(false))
        .insert_resource(Paused(false))
        .insert_resource(Countdown::default())
        .add_systems(Startup, (setup_fps_counter, setup_game_speed))
        .add_systems(OnEnter(loading::LoadingState::Playing), start_countdown)
        .add_systems(OnExit(loading::LoadingState::Playing), cleanup_countdown)
        .add_systems(Update, (update_fps_text, toggle_inspector, toggle_pause, handle_pause_time, show_pause_overlay))
        .add_systems(Update, tick_countdown.run_if(in_state(loading::LoadingState::Playing)))
        .add_systems(Update, handle_game_net_events.run_if(in_state(loading::LoadingState::Playing)))
        .add_systems(Update, quit_to_menu.run_if(in_state(loading::LoadingState::Playing)))
        .run();
}

fn handle_game_net_events(
    mut events: MessageReader<networking::GameNetEvent>,
    mut countdown: ResMut<Countdown>,
    mut paused: ResMut<Paused>,
    mut game_state: ResMut<launch_pads::GameState>,
) {
    for event in events.read() {
        match event {
            networking::GameNetEvent::StartGame => {
                info!("GameStart received — beginning countdown");
                countdown.active = true;
                paused.0 = true; // tick_countdown un-pauses on completion
            }
            networking::GameNetEvent::GameOver { winner } => {
                info!("GameOver received from host — winner: {:?}", winner);
                game_state.game_over = true;
                game_state.winner = Some(*winner);
            }
            networking::GameNetEvent::Snapshot(_) => {} // handled by net_sync::apply_snapshot
        }
    }
}

fn quit_to_menu(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut next_state: ResMut<NextState<loading::LoadingState>>,
    mut paused: ResMut<Paused>,
    mut countdown: ResMut<Countdown>,
) {
    if keyboard.just_pressed(KeyCode::KeyQ) {
        paused.0 = false;
        countdown.active = false;
        next_state.set(loading::LoadingState::TitleScreen);
    }
}

fn setup_game_speed(mut time: ResMut<Time<Virtual>>) {
    // Slow down game by 20% (0.8 = 80% speed)
    time.set_relative_speed(0.8);
}

#[derive(Component)]
struct FpsText;

#[derive(Resource)]
struct InspectorEnabled(bool);

#[derive(Resource)]
pub struct Paused(pub bool);

fn inspector_enabled(inspector: Res<InspectorEnabled>) -> bool {
    inspector.0
}

fn toggle_inspector(
    mut inspector: ResMut<InspectorEnabled>,
    keyboard: Res<ButtonInput<KeyCode>>,
) {
    if keyboard.just_pressed(KeyCode::KeyH) {
        inspector.0 = !inspector.0;
        println!("Inspector {}", if inspector.0 { "enabled" } else { "disabled" });
    }
}

fn setup_fps_counter(mut commands: Commands) {
    // Spawn FPS text in top-left corner
    commands.spawn((
        Text::new("FPS: --"),
        TextFont {
            font_size: 24.0,
            ..default()
        },
        TextColor(Color::srgb(0.0, 1.0, 0.0)), // Green text
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(10.0),
            left: Val::Px(10.0),
            ..default()
        },
        FpsText,
    ));
}

fn update_fps_text(
    diagnostics: Res<DiagnosticsStore>,
    mut query: Query<&mut Text, With<FpsText>>,
) {
    for mut text in &mut query {
        if let Some(fps) = diagnostics.get(&FrameTimeDiagnosticsPlugin::FPS)
            && let Some(value) = fps.smoothed() {
                text.0 = format!("FPS: {:.0}", value);
            }
    }
}

fn toggle_pause(
    mut paused: ResMut<Paused>,
    keyboard: Res<ButtonInput<KeyCode>>,
) {
    if keyboard.just_pressed(KeyCode::Space) {
        paused.0 = !paused.0;
        println!("⏸️  Game {}", if paused.0 { "PAUSED" } else { "RESUMED" });
    }
}

fn handle_pause_time(
    paused: Res<Paused>,
    mut time: ResMut<Time<Virtual>>,
) {
    if paused.is_changed() {
        if paused.0 {
            time.set_relative_speed(0.0);
        } else {
            time.set_relative_speed(0.8); // Restore the game speed from setup
        }
    }
}

#[derive(Resource, Default)]
struct Countdown {
    remaining: f32,
    active: bool,
    // Camera intro animation
    cam_start_x: f32,
    cam_start_z: f32,
    cam_start_scale: f32,
    cam_z_offset: f32,   // fixed distance: camera.z - look_at.z
    cam_end_at: f32,     // remaining seconds when camera should reach home
}

#[derive(Component)]
struct CountdownOverlay;

#[derive(Component)]
struct CountdownShadow;

#[derive(Component)]
struct CountdownText;

fn start_countdown(
    mut commands: Commands,
    mut paused: ResMut<Paused>,
    mut countdown: ResMut<Countdown>,
    mut cam: ResMut<crate::ui::CameraSettings>,
    map_def: Res<crate::map_loader::MapDefinition>,
    net_mode: Res<networking::MultiplayerMode>,
    mut net_broadcast: MessageWriter<networking::BroadcastNetMsg>,
) {
    // In multiplayer, pause and wait for the GameStart handshake.
    // We still set up the camera intro so it's ready to go.
    // The actual countdown activation happens in handle_game_net_events.
    if *net_mode != networking::MultiplayerMode::Singleplayer {
        paused.0 = true;
        countdown.active = false;
        // Tell the peer (or host) that we have finished loading.
        net_broadcast.write(networking::BroadcastNetMsg(networking::NetworkMessage::ReadyToPlay));
        // Fall through to set up camera and overlay, but active stays false.
    } else {
        countdown.active = true;
        paused.0 = true;
    }

    countdown.remaining = 6.5;

    // Compute centroid of Red spawn cells as the intro focus point.
    let spawn_world: Vec<Vec3> = map_def.spawn_red.iter()
        .map(|&(q, r)| crate::map::axial_to_world_pos(q, r))
        .collect();
    let (focus_x, focus_z) = if spawn_world.is_empty() {
        (cam.home_x, cam.home_z)
    } else {
        let fx = spawn_world.iter().map(|v| v.x).sum::<f32>() / spawn_world.len() as f32;
        let fz = spawn_world.iter().map(|v| v.z).sum::<f32>() / spawn_world.len() as f32;
        (fx, fz)
    };

    let z_offset = cam.z - cam.look_at_z;
    countdown.cam_start_x = focus_x;
    countdown.cam_start_z = focus_z;
    countdown.cam_start_scale = 0.5;
    countdown.cam_z_offset = z_offset;
    countdown.cam_end_at = 0.7; // 200 ms before "Go!" at 0.5 s

    // Snap camera to the zoomed-in start position.
    cam.look_at_x = focus_x;
    cam.look_at_z = focus_z;
    cam.x = focus_x;
    cam.z = focus_z + z_offset;
    cam.scale = countdown.cam_start_scale;

    // Overlay is present for the full sequence (hold + countdown).
    commands.spawn((
        Node {
            position_type: PositionType::Absolute,
            width: Val::Percent(100.0),
            height: Val::Percent(100.0),
            flex_direction: FlexDirection::Column,
            justify_content: JustifyContent::Start,
            align_items: AlignItems::Center,
            padding: UiRect::top(Val::Vh(33.0)),
            ..default()
        },
        BackgroundColor(Color::srgba(0.0, 0.0, 0.0, 0.4)),
        ZIndex(100),
        CountdownOverlay,
    )).with_children(|parent| {
        // Container for stacking shadow + main text
        parent.spawn(Node {
            position_type: PositionType::Relative,
            ..default()
        }).with_children(|stack| {
            // Shadow: absolutely positioned, offset down-right
            stack.spawn((
                Text::new(""),
                TextFont { font_size: 120.0, ..default() },
                TextColor(Color::srgba(0.0, 0.0, 0.0, 0.6)),
                Node {
                    position_type: PositionType::Absolute,
                    left: Val::Px(5.0),
                    top: Val::Px(5.0),
                    ..default()
                },
                CountdownShadow,
            ));
            // Main text on top
            stack.spawn((
                Text::new(""),   // empty during the 3-second hold
                TextFont { font_size: 120.0, ..default() },
                TextColor(Color::srgb(1.0, 1.0, 1.0)),
                CountdownText,
            ));
        });
    });
}

fn cleanup_countdown(
    mut commands: Commands,
    mut countdown: ResMut<Countdown>,
    overlay_query: Query<Entity, With<CountdownOverlay>>,
) {
    countdown.active = false;
    for entity in &overlay_query {
        commands.entity(entity).despawn();
    }
}

fn tick_countdown(
    mut commands: Commands,
    time: Res<Time<Real>>,
    mut countdown: ResMut<Countdown>,
    mut paused: ResMut<Paused>,
    mut cam: ResMut<crate::ui::CameraSettings>,
    overlay_query: Query<Entity, With<CountdownOverlay>>,
    mut text_query: Query<&mut Text, With<CountdownText>>,
    mut shadow_query: Query<&mut Text, (With<CountdownShadow>, Without<CountdownText>)>,
) {
    if !countdown.active {
        return;
    }

    countdown.remaining -= time.delta_secs();

    // Animate camera from zoomed-in base view to home position.
    // t goes 0→1 over the intro window; ease-out cubic for a smooth deceleration.
    let intro_total = 3.5 - countdown.cam_end_at;
    let elapsed = (intro_total - (countdown.remaining - countdown.cam_end_at)).max(0.0);
    let t = (elapsed / intro_total).clamp(0.0, 1.0);
    let t_ease = t.powi(3); // ease-in cubic: starts slowly, accelerates to end

    cam.look_at_x = countdown.cam_start_x + (cam.home_x - countdown.cam_start_x) * t_ease;
    cam.look_at_z = countdown.cam_start_z + (cam.home_z - countdown.cam_start_z) * t_ease;
    cam.x = cam.look_at_x;
    cam.z = cam.look_at_z + countdown.cam_z_offset;
    cam.scale = countdown.cam_start_scale + (0.85 - countdown.cam_start_scale) * t_ease;

    if countdown.remaining <= 0.0 {
        countdown.active = false;
        for entity in &overlay_query {
            commands.entity(entity).despawn();
        }
        paused.0 = false;
        return;
    }

    let label = if countdown.remaining > 3.5 {
        ""       // 3-second hold — overlay visible, no number yet
    } else if countdown.remaining > 2.5 {
        "3"
    } else if countdown.remaining > 1.5 {
        "2"
    } else if countdown.remaining > 0.5 {
        "1"
    } else {
        "Go!"
    };

    for mut text in &mut text_query {
        text.0 = label.to_string();
    }
    for mut text in &mut shadow_query {
        text.0 = label.to_string();
    }
}

#[derive(Component)]
struct PauseOverlay;

fn show_pause_overlay(
    mut commands: Commands,
    paused: Res<Paused>,
    countdown: Res<Countdown>,
    overlay_query: Query<Entity, With<PauseOverlay>>,
    children_query: Query<&Children>,
    countdown_overlay_query: Query<(), With<CountdownOverlay>>,
) {
    // Don't show "PAUSED" while the countdown sequence is active (including
    // the pre-start wait in multiplayer where paused=true but no countdown yet).
    if paused.is_changed() && !countdown.active && countdown_overlay_query.is_empty() {
        if paused.0 {
            // Show pause overlay
            if overlay_query.is_empty() {
                commands.spawn((
                    Node {
                        position_type: PositionType::Absolute,
                        width: Val::Percent(100.0),
                        height: Val::Percent(100.0),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        ..default()
                    },
                    BackgroundColor(Color::srgba(0.0, 0.0, 0.0, 0.5)),
                    ZIndex(-1),
                    PauseOverlay,
                )).with_children(|parent| {
                    parent.spawn((
                        Text::new("PAUSED"),
                        TextFont {
                            font_size: 120.0,
                            ..default()
                        },
                        TextColor(Color::srgb(1.0, 1.0, 1.0)),
                    ));
                });
            }
        } else {
            // Hide pause overlay and all children
            for entity in overlay_query.iter() {
                let mut to_despawn = vec![entity];

                let mut i = 0;
                while i < to_despawn.len() {
                    if let Ok(children) = children_query.get(to_despawn[i]) {
                        for child in children.iter() {
                            to_despawn.push(child);
                        }
                    }
                    i += 1;
                }

                for entity_to_remove in to_despawn {
                    commands.entity(entity_to_remove).despawn();
                }
            }
        }
    }
}
