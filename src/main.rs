use bevy::prelude::*;
use bevy::diagnostic::{FrameTimeDiagnosticsPlugin, DiagnosticsStore};
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_inspector_egui::bevy_egui::EguiPlugin;

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

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                resolution: bevy::window::WindowResolution::new(1500, 720),
                title: "Air Land Battle".to_string(),
                ..default()
            }),
            ..default()
        }))
        .add_plugins(FrameTimeDiagnosticsPlugin::default())
        .add_plugins(EguiPlugin::default())
        .add_plugins(WorldInspectorPlugin::default().run_if(inspector_enabled))
        .add_plugins(bevy_mod_outline::OutlinePlugin)
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
        .insert_resource(InspectorEnabled(false))
        .insert_resource(Paused(false))
        .add_systems(Startup, (setup_fps_counter, setup_game_speed))
        .add_systems(Update, (update_fps_text, toggle_inspector, toggle_pause, handle_pause_time, show_pause_overlay))
        .add_systems(Update, quit_to_menu.run_if(in_state(loading::LoadingState::Playing)))
        .run();
}

fn quit_to_menu(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut next_state: ResMut<NextState<loading::LoadingState>>,
    mut paused: ResMut<Paused>,
) {
    if keyboard.just_pressed(KeyCode::KeyQ) {
        if paused.0 {
            paused.0 = false;
        }
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

#[derive(Component)]
struct PauseOverlay;

fn show_pause_overlay(
    mut commands: Commands,
    paused: Res<Paused>,
    overlay_query: Query<Entity, With<PauseOverlay>>,
    children_query: Query<&Children>,
) {
    if paused.is_changed() {
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
