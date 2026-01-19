use bevy::prelude::*;
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_inspector_egui::bevy_egui::EguiPlugin;

mod map;
mod units;
mod selection;
mod launch_pads;
mod ui;

use map::MapPlugin;
use units::UnitsPlugin;
use selection::SelectionPlugin;
use launch_pads::LaunchPadsPlugin;
use ui::UIPlugin;

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
        .add_plugins(EguiPlugin::default())
        .add_plugins(WorldInspectorPlugin::new())
        .add_plugins(LaunchPadsPlugin)
        .add_plugins(MapPlugin)
        .add_plugins(UnitsPlugin)
        .add_plugins(SelectionPlugin)
        .add_plugins(UIPlugin)
        .run();
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
