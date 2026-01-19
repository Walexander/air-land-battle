use bevy::prelude::*;

use crate::launch_pads::{GameState, GameTimer};
use crate::units::Army;

// Components
#[derive(Component)]
pub struct TimerBar;

#[derive(Component)]
pub struct TimerText;

#[derive(Component)]
pub struct GameOverScreen;

// Systems
fn setup_ui(mut commands: Commands) {
    commands
        .spawn(Node {
            width: Val::Percent(100.0),
            height: Val::Px(60.0),
            position_type: PositionType::Absolute,
            bottom: Val::Px(0.0),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            ..default()
        })
        .with_children(|parent| {
            parent
                .spawn((
                    Node {
                        width: Val::Px(400.0),
                        height: Val::Px(30.0),
                        border: UiRect::all(Val::Px(2.0)),
                        ..default()
                    },
                    BorderColor::all(Color::srgb(0.3, 0.3, 0.3)),
                    BackgroundColor(Color::srgb(0.1, 0.1, 0.1)),
                ))
                .with_children(|parent| {
                    parent.spawn((
                        Node {
                            width: Val::Percent(0.0),
                            height: Val::Percent(100.0),
                            ..default()
                        },
                        BackgroundColor(Color::srgb(0.2, 0.8, 0.2)),
                        TimerBar,
                    ));
                });

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

fn update_timer_ui(
    game_timer: Res<GameTimer>,
    mut bar_query: Query<(&mut Node, &mut BackgroundColor), With<TimerBar>>,
    mut text_query: Query<&mut Text, With<TimerText>>,
) {
    if let Ok((mut node, mut bg_color)) = bar_query.single_mut() {
        if game_timer.is_active {
            let progress = game_timer.time_remaining / 20.0;
            node.width = Val::Percent(progress * 100.0);

            *bg_color = match game_timer.winning_army {
                Some(Army::Red) => BackgroundColor(Color::srgb(0.9, 0.2, 0.2)),
                Some(Army::Blue) => BackgroundColor(Color::srgb(0.2, 0.4, 0.9)),
                None => BackgroundColor(Color::srgb(0.2, 0.8, 0.2)),
            };
        } else {
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
            **text = format!(
                "{} {:.1}s",
                winning_army_name, game_timer.time_remaining
            );
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

        commands
            .spawn((
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
            ))
            .with_children(|parent| {
                parent
                    .spawn((
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
                    ))
                    .with_children(|parent| {
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
    if game_state.game_over && keyboard.just_pressed(KeyCode::Space) {
        println!("Restarting game...");

        game_state.game_over = false;
        game_state.winner = None;

        game_timer.time_remaining = 20.0;
        game_timer.is_active = false;
        game_timer.winning_army = None;

        for entity in game_over_query.iter() {
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

            for e in to_despawn.into_iter().rev() {
                commands.entity(e).despawn();
            }
        }

        println!("Game restarted!");
    }
}

pub struct UIPlugin;

impl Plugin for UIPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup_ui).add_systems(
            Update,
            (update_timer_ui, show_game_over_screen, handle_restart),
        );
    }
}
