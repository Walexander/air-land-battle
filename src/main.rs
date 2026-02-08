use bevy::prelude::*;
use bevy::diagnostic::{FrameTimeDiagnosticsPlugin, DiagnosticsStore};
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_inspector_egui::bevy_egui::EguiPlugin;

mod map;
mod units;
mod selection;
mod launch_pads;
mod ui;
mod loading;
mod ai;
mod economy;

use map::MapPlugin;
use units::UnitsPlugin;
use selection::SelectionPlugin;
use launch_pads::LaunchPadsPlugin;
use ui::UIPlugin;
use loading::LoadingPlugin;
use ai::AIPlugin;
use economy::EconomyPlugin;

// #[derive(Component)]
// struct Position {
//     x: f32,
//     y: f32,
// }

// #[derive(Component)]
// struct Person;

// #[derive(Component)]
// struct Name(String);

// #[derive(Resource)]
// struct GreetTimer(Timer);
// #[derive(Resource)]
// struct PositionTimer(Timer);

// fn print_position_system(time: Res<Time>, mut timer: ResMut<PositionTimer>, query: Query<&Position>) {
//     if timer.0.tick(time.delta()).just_finished() {
//         for position in &query {
//             println!("Position: {} {}", position.x, position.y);
//         }
//     }
// }

// fn greet_people(time: Res<Time>, mut timer: ResMut<GreetTimer>, query: Query<&Name, With<Person>>) {
//     if timer.0.tick(time.delta()).just_finished() {
//         for name in &query {
//             println!("Person (named): {}", name.0);
//         }
//     }
// }
// fn update_people(mut query: Query<&mut Name, With<Person>>) {
//     for mut name in &mut query {
//         if name.0 == "Shirley" {
//             name.0 = "Shirley Alexander".to_string();
//             break;
//         }
//     }
// }

// fn add_people(mut commands: Commands) {
//     commands.spawn(
//         (Position {x: 24.0, y: 32.0 }, Person, Name("Will".to_string()))
//     );
//     commands.spawn((Position {x: 19.0, y: 12.0 }, Person, Name("Shirley".to_string())));
// }

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(FrameTimeDiagnosticsPlugin::default())
        .add_plugins(EguiPlugin::default())
        .add_plugins(WorldInspectorPlugin::new())
        .add_plugins(bevy_mod_outline::OutlinePlugin)
        .add_plugins(LoadingPlugin)
        .add_plugins(LaunchPadsPlugin)
        .add_plugins(MapPlugin)
        .add_plugins(EconomyPlugin)
        .add_plugins(UnitsPlugin)
        .add_plugins(AIPlugin)
        .add_plugins(SelectionPlugin)
        .add_plugins(UIPlugin)
        .add_systems(Startup, (setup_fps_counter, setup_game_speed))
        .add_systems(Update, update_fps_text)
        .run();
}

fn setup_game_speed(mut time: ResMut<Time<Virtual>>) {
    // Slow down game by 20% (0.8 = 80% speed)
    time.set_relative_speed(0.8);
}

#[derive(Component)]
struct FpsText;

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

// pub struct HelloPlugin;
// impl Plugin for HelloPlugin {
//     fn build(&self, app: &mut App) {
//         app.add_systems(Startup, add_people)
//             .add_systems(Update, (update_people, print_position_system));
//         app.add_systems(Update, greet_people);
//         app.insert_resource(GreetTimer(Timer::from_seconds(2.0, TimerMode::Repeating)));
//         app.insert_resource(PositionTimer(Timer::from_seconds(2.0, TimerMode::Repeating)));
//     }
// }
