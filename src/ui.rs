use bevy::prelude::*;
use bevy::input::mouse::MouseWheel;

use crate::launch_pads::{GameState, GameTimer};
use crate::economy::Economy;
use crate::units::{Army, Unit, UnitClass, UnitSpawnRequest, UnitSpawnQueue, SpawnCooldowns};
use crate::loading::LoadingState;

// Components
#[derive(Component)]
pub struct TimerBar;

#[derive(Component)]
pub struct TimerText;

#[derive(Component)]
pub struct GameOverScreen;

#[derive(Component)]
pub struct CameraControlsPanel;

#[derive(Component)]
pub struct GameCamera;

#[derive(Component)]
pub struct RedMoneyText;

#[derive(Component)]
pub struct BlueMoneyText;

#[derive(Component)]
pub struct HexCoordinateDisplay;

#[derive(Component)]
pub enum UnitSpawnButton {
    Infantry,
    Cavalry,
    Artillery,
    Harvester,
}

#[derive(Component)]
pub struct SpawnButtonFill {
    pub button_type: UnitSpawnButton,
}

// Resources for camera controls
#[derive(Resource)]
pub struct CameraSettings {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub look_at_x: f32,
    pub look_at_y: f32,
    pub look_at_z: f32,
    pub scale: f32,
}

#[derive(Resource)]
pub struct CameraControlsVisible(pub bool);

#[derive(Resource, Default)]
pub struct UIClicked(pub bool);

// Systems
fn reset_ui_clicked(mut ui_clicked: ResMut<UIClicked>) {
    ui_clicked.0 = false;
}

fn setup_ui(mut commands: Commands) {
    // Bottom bar with buttons and money counter
    commands
        .spawn(Node {
            width: Val::Percent(100.0),
            height: Val::Px(90.0),
            position_type: PositionType::Absolute,
            bottom: Val::Px(0.0),
            flex_direction: FlexDirection::Row,
            align_items: AlignItems::Center,
            column_gap: Val::Px(12.0),
            justify_content: JustifyContent::Center,
            padding: UiRect::all(Val::Px(7.5)),
            ..default()
        })
        .insert(BackgroundColor(Color::srgb(1.0, 1.0, 1.0)))
        .with_children(|parent| {
            // Infantry button
            parent
                .spawn((
                    Button,
                    Node {
                        width: Val::Px(75.0),
                        height: Val::Px(75.0),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        border: UiRect::all(Val::Px(3.0)),
                        ..default()
                    },
                    BackgroundColor(Color::srgb(0.0, 0.0, 0.0)),
                    BorderColor::all(Color::srgb(0.2, 0.2, 0.2)),
                    UnitSpawnButton::Infantry,
                ))
                .with_children(|button_parent| {
                    // Progress fill bar (starts at bottom)
                    button_parent.spawn((
                        Node {
                            width: Val::Percent(100.0),
                            height: Val::Percent(0.0),
                            position_type: PositionType::Absolute,
                            bottom: Val::Px(0.0),
                            ..default()
                        },
                        BackgroundColor(Color::srgb(0.0, 0.6, 0.0)),
                        SpawnButtonFill {
                            button_type: UnitSpawnButton::Infantry,
                        },
                    ));

                    // Text centered
                    button_parent.spawn((
                        Text::new("I"),
                        TextFont {
                            font_size: 36.0,
                            ..default()
                        },
                        TextColor(Color::WHITE),
                    ));
                });

            // Cavalry button
            parent
                .spawn((
                    Button,
                    Node {
                        width: Val::Px(75.0),
                        height: Val::Px(75.0),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        border: UiRect::all(Val::Px(3.0)),
                        ..default()
                    },
                    BackgroundColor(Color::srgb(0.0, 0.0, 0.0)),
                    BorderColor::all(Color::srgb(0.2, 0.2, 0.2)),
                    UnitSpawnButton::Cavalry,
                ))
                .with_children(|button_parent| {
                    // Progress fill bar (starts at bottom)
                    button_parent.spawn((
                        Node {
                            width: Val::Percent(100.0),
                            height: Val::Percent(0.0),
                            position_type: PositionType::Absolute,
                            bottom: Val::Px(0.0),
                            ..default()
                        },
                        BackgroundColor(Color::srgb(0.0, 0.6, 0.0)),
                        SpawnButtonFill {
                            button_type: UnitSpawnButton::Cavalry,
                        },
                    ));

                    // Text centered
                    button_parent.spawn((
                        Text::new("C"),
                        TextFont {
                            font_size: 36.0,
                            ..default()
                        },
                        TextColor(Color::WHITE),
                    ));
                });

            // Artillery button
            parent
                .spawn((
                    Button,
                    Node {
                        width: Val::Px(75.0),
                        height: Val::Px(75.0),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        border: UiRect::all(Val::Px(3.0)),
                        ..default()
                    },
                    BackgroundColor(Color::srgb(0.0, 0.0, 0.0)),
                    BorderColor::all(Color::srgb(0.2, 0.2, 0.2)),
                    UnitSpawnButton::Artillery,
                ))
                .with_children(|button_parent| {
                    // Progress fill bar (starts at bottom)
                    button_parent.spawn((
                        Node {
                            width: Val::Percent(100.0),
                            height: Val::Percent(0.0),
                            position_type: PositionType::Absolute,
                            bottom: Val::Px(0.0),
                            ..default()
                        },
                        BackgroundColor(Color::srgb(0.0, 0.6, 0.0)),
                        SpawnButtonFill {
                            button_type: UnitSpawnButton::Artillery,
                        },
                    ));

                    // Text centered
                    button_parent.spawn((
                        Text::new("A"),
                        TextFont {
                            font_size: 36.0,
                            ..default()
                        },
                        TextColor(Color::WHITE),
                    ));
                });

            // Harvester button
            parent
                .spawn((
                    Button,
                    Node {
                        width: Val::Px(75.0),
                        height: Val::Px(75.0),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        border: UiRect::all(Val::Px(3.0)),
                        ..default()
                    },
                    BackgroundColor(Color::srgb(0.0, 0.0, 0.0)),
                    BorderColor::all(Color::srgb(0.2, 0.2, 0.2)),
                    UnitSpawnButton::Harvester,
                ))
                .with_children(|button_parent| {
                    // Progress fill bar (starts at bottom)
                    button_parent.spawn((
                        Node {
                            width: Val::Percent(100.0),
                            height: Val::Percent(0.0),
                            position_type: PositionType::Absolute,
                            bottom: Val::Px(0.0),
                            ..default()
                        },
                        BackgroundColor(Color::srgb(0.0, 0.6, 0.0)),
                        SpawnButtonFill {
                            button_type: UnitSpawnButton::Harvester,
                        },
                    ));

                    // Text centered
                    button_parent.spawn((
                        Text::new("H"),
                        TextFont {
                            font_size: 36.0,
                            ..default()
                        },
                        TextColor(Color::WHITE),
                    ));
                });

            // Money display (to the right of buttons)
            parent.spawn((
                Text::new("Red: $100"),
                TextFont {
                    font_size: 36.0,
                    ..default()
                },
                TextColor(Color::srgb(0.9, 0.2, 0.2)),
                Node {
                    width: Val::Px(225.0),
                    ..default()
                },
                RedMoneyText,
            ));
        });

    // Blue army money display (top-right)
    commands.spawn((
        Text::new("Blue: $100"),
        TextFont {
            font_size: 36.0,
            ..default()
        },
        TextColor(Color::srgb(0.2, 0.4, 0.9)),
        Node {
            position_type: PositionType::Absolute,
            top: Val::Px(15.0),
            right: Val::Px(15.0),
            ..default()
        },
        BlueMoneyText,
    ));

    commands
        .spawn(Node {
            width: Val::Percent(100.0),
            height: Val::Px(60.0),
            position_type: PositionType::Absolute,
            top: Val::Px(0.0),
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

    // Hex coordinate display (bottom left of screen, above white bar)
    commands.spawn((
        Text::new(""),
        TextFont {
            font_size: 18.0,
            ..default()
        },
        TextColor(Color::srgb(0.0, 0.0, 0.0)),
        Node {
            position_type: PositionType::Absolute,
            bottom: Val::Px(70.0),
            left: Val::Px(10.0),
            ..default()
        },
        BackgroundColor(Color::srgba(1.0, 1.0, 1.0, 0.7)),
        HexCoordinateDisplay,
    ));
}

fn update_timer_ui(
    game_timer: Res<GameTimer>,
    mut bar_query: Query<(&mut Node, &mut BackgroundColor), With<TimerBar>>,
    mut text_query: Query<&mut Text, With<TimerText>>,
) {
    if let Ok((mut node, mut bg_color)) = bar_query.single_mut() {
        // Always show progress bar based on time remaining
        let progress = game_timer.time_remaining / 20.0;
        node.width = Val::Percent(progress * 100.0);

        // Change color based on winning army (yellow when no owner)
        *bg_color = match game_timer.winning_army {
            Some(Army::Red) => BackgroundColor(Color::srgb(0.9, 0.2, 0.2)),
            Some(Army::Blue) => BackgroundColor(Color::srgb(0.2, 0.4, 0.9)),
            None => BackgroundColor(Color::srgb(0.8, 0.7, 0.0)), // Darker yellow for no owner
        };
    }

    if let Ok(mut text) = text_query.single_mut() {
        // Always show the time remaining
        **text = format!("{:.1}s", game_timer.time_remaining);
    }
}

fn update_money_ui(
    economy: Res<Economy>,
    mut red_query: Query<&mut Text, (With<RedMoneyText>, Without<BlueMoneyText>)>,
    mut blue_query: Query<&mut Text, (With<BlueMoneyText>, Without<RedMoneyText>)>,
) {
    if let Ok(mut text) = red_query.single_mut() {
        **text = format!("Red: ${}", economy.red_money);
    }

    if let Ok(mut text) = blue_query.single_mut() {
        **text = format!("Blue: ${}", economy.blue_money);
    }
}

fn update_hex_coordinate_display(
    hovered_hex: Res<crate::map::HoveredHex>,
    mut coord_query: Query<&mut Text, With<HexCoordinateDisplay>>,
) {
    if let Ok(mut text) = coord_query.single_mut() {
        if hovered_hex.entity.is_some() {
            **text = format!("({}, {})", hovered_hex.q, hovered_hex.r);
        } else {
            **text = String::new();
        }
    }
}

fn handle_unit_spawn_buttons(
    mut interaction_query: Query<
        (&Interaction, &UnitSpawnButton, &mut BorderColor, &mut Node),
        Changed<Interaction>,
    >,
    economy: Res<Economy>,
    spawn_cooldowns: Res<SpawnCooldowns>,
    unit_query: Query<&Unit>,
    mut spawn_queue: ResMut<UnitSpawnQueue>,
    mut ui_clicked: ResMut<UIClicked>,
) {
    for (interaction, button, mut border_color, mut node) in &mut interaction_query {
        // Mark that UI was clicked
        if *interaction == Interaction::Pressed {
            ui_clicked.0 = true;
        }

        let unit_class = match button {
            UnitSpawnButton::Infantry => UnitClass::Infantry,
            UnitSpawnButton::Cavalry => UnitClass::Cavalry,
            UnitSpawnButton::Artillery => UnitClass::Artillery,
            UnitSpawnButton::Harvester => UnitClass::Harvester,
        };

        // Count current Red army units (including harvesters)
        let red_unit_count = unit_query.iter()
            .filter(|u| u.army == Army::Red)
            .count();

        match *interaction {
            Interaction::Pressed => {
                let cost = unit_class.cost();
                let can_afford = economy.red_money >= cost;
                let red_cooldowns = spawn_cooldowns.get_army_cooldowns(Army::Red);
                let cooldown_ready = red_cooldowns.is_ready(unit_class, red_unit_count);

                // Visual: make button look pressed
                node.border = UiRect {
                    left: Val::Px(3.0),
                    right: Val::Px(3.0),
                    top: Val::Px(4.0),
                    bottom: Val::Px(2.0),
                };

                if can_afford && cooldown_ready {
                    spawn_queue.requests.push(UnitSpawnRequest {
                        unit_class,
                        army: Army::Red,
                    });
                    *border_color = BorderColor::all(Color::srgb(0.1, 0.1, 0.1));
                } else if !can_afford {
                    // Flash red border if not enough money
                    *border_color = BorderColor::all(Color::srgb(0.8, 0.2, 0.2));
                } else {
                    // Pressed but on cooldown - darker border
                    *border_color = BorderColor::all(Color::srgb(0.1, 0.1, 0.1));
                }
            }
            Interaction::Hovered => {
                // Reset border to uniform
                node.border = UiRect::all(Val::Px(3.0));

                let red_cooldowns = spawn_cooldowns.get_army_cooldowns(Army::Red);
                // Highlight border when hovered
                if red_cooldowns.is_ready(unit_class, red_unit_count) {
                    *border_color = BorderColor::all(Color::srgb(0.6, 0.6, 0.6));
                } else {
                    *border_color = BorderColor::all(Color::srgb(0.2, 0.2, 0.2));
                }
            }
            Interaction::None => {
                // Reset border to uniform
                node.border = UiRect::all(Val::Px(3.0));

                // Reset border color
                *border_color = BorderColor::all(Color::srgb(0.2, 0.2, 0.2));
            }
        }
    }
}

fn update_spawn_button_visuals(
    spawn_cooldowns: Res<SpawnCooldowns>,
    economy: Res<Economy>,
    unit_query: Query<&Unit>,
    mut button_query: Query<(&UnitSpawnButton, &mut BackgroundColor), Without<SpawnButtonFill>>,
    mut fill_query: Query<(&SpawnButtonFill, &mut Node, &mut BackgroundColor), Without<UnitSpawnButton>>,
) {
    let red_cooldowns = spawn_cooldowns.get_army_cooldowns(Army::Red);

    // Count current Red army units (including harvesters)
    let red_unit_count = unit_query.iter()
        .filter(|u| u.army == Army::Red)
        .count();

    // Update button backgrounds
    for (button, mut bg_color) in &mut button_query {
        let unit_class = match button {
            UnitSpawnButton::Infantry => UnitClass::Infantry,
            UnitSpawnButton::Cavalry => UnitClass::Cavalry,
            UnitSpawnButton::Artillery => UnitClass::Artillery,
            UnitSpawnButton::Harvester => UnitClass::Harvester,
        };

        let can_afford = economy.red_money >= unit_class.cost();
        let is_ready = red_cooldowns.is_ready(unit_class, red_unit_count);

        if is_ready && !can_afford {
            // Not affordable but ready - red background
            *bg_color = BackgroundColor(Color::srgb(0.6, 0.0, 0.0));
        } else {
            // Always black background (fill bar shows progress)
            *bg_color = BackgroundColor(Color::srgb(0.0, 0.0, 0.0));
        }
    }

    // Update fill bar heights and colors
    for (fill, mut node, mut fill_color) in &mut fill_query {
        let unit_class = match fill.button_type {
            UnitSpawnButton::Infantry => UnitClass::Infantry,
            UnitSpawnButton::Cavalry => UnitClass::Cavalry,
            UnitSpawnButton::Artillery => UnitClass::Artillery,
            UnitSpawnButton::Harvester => UnitClass::Harvester,
        };

        let can_afford = economy.red_money >= unit_class.cost();
        let _is_ready = red_cooldowns.is_ready(unit_class, red_unit_count);

        // Set fill bar color based on affordability (regardless of cooldown status)
        if !can_afford {
            *fill_color = BackgroundColor(Color::srgb(0.8, 0.0, 0.0)); // Bright red when can't afford
        } else {
            *fill_color = BackgroundColor(Color::srgb(0.0, 0.6, 0.0)); // Green when affordable
        }

        let progress = red_cooldowns.get_progress(unit_class, red_unit_count);
        let height_percent = progress * 100.0;
        node.height = Val::Percent(height_percent);
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

fn setup_camera_controls(mut commands: Commands, settings: Res<CameraSettings>) {
    commands
        .spawn((
            Node {
                width: Val::Px(300.0),
                height: Val::Auto,
                position_type: PositionType::Absolute,
                top: Val::Px(10.0),
                right: Val::Px(10.0),
                flex_direction: FlexDirection::Column,
                padding: UiRect::all(Val::Px(10.0)),
                row_gap: Val::Px(8.0),
                ..default()
            },
            BackgroundColor(Color::srgba(0.1, 0.1, 0.1, 0.8)),
            ZIndex(1000),
            CameraControlsPanel,
        ))
        .with_children(|parent| {
            parent.spawn((
                Text::new("Camera Controls"),
                TextFont {
                    font_size: 18.0,
                    ..default()
                },
                TextColor(Color::WHITE),
            ));

            // Helper closure to spawn sliders
            let mut spawn_slider = |label: &str, min: f32, max: f32, initial_value: f32, name: &str| {
                parent.spawn((
                    Text::new(format!("{}{:.1}", label, initial_value)),
                    TextFont {
                        font_size: 14.0,
                        ..Default::default()
                    },
                    TextColor(Color::srgb(0.9, 0.9, 0.9)),
                    Name::new(format!("{}_label", name)),
                ));

                parent.spawn((
                    Button,
                    Node {
                        width: Val::Px(280.0),
                        height: Val::Px(20.0),
                        ..Default::default()
                    },
                    BackgroundColor(Color::srgb(0.3, 0.3, 0.3)),
                    Name::new(format!("{}_bg", name)),
                    CameraSliderBg {
                        min,
                        max,
                        param_name: name.to_string(),
                    },
                )).with_children(|slider_bg| {
                    slider_bg.spawn((
                        Node {
                            width: Val::Percent(((initial_value - min) / (max - min)) * 100.0),
                            height: Val::Percent(100.0),
                            ..Default::default()
                        },
                        BackgroundColor(Color::srgb(0.2, 0.6, 1.0)),
                        Name::new(format!("{}_slider", name)),
                        CameraSlider {
                            value: initial_value,
                        },
                    ));
                });
            };

            spawn_slider("Cam X: ", -1000.0, 1000.0, settings.x, "cam_x");
            spawn_slider("Cam Y: ", 0.0, 1000.0, settings.y, "cam_y");
            spawn_slider("Cam Z: ", -1000.0, 1000.0, settings.z, "cam_z");
            spawn_slider("Look X: ", -500.0, 500.0, settings.look_at_x, "look_x");
            spawn_slider("Look Y: ", -500.0, 500.0, settings.look_at_y, "look_y");
            spawn_slider("Look Z: ", -500.0, 500.0, settings.look_at_z, "look_z");
            spawn_slider("Scale: ", 0.1, 2.0, settings.scale, "scale");
        });
}

#[derive(Component)]
struct CameraSliderBg {
    min: f32,
    max: f32,
    param_name: String,
}

#[derive(Component)]
struct CameraSlider {
    value: f32,
}

fn handle_keyboard_and_scroll_camera(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut scroll_events: MessageReader<MouseWheel>,
    mut settings: ResMut<CameraSettings>,
    time: Res<Time>,
) {
    // Reset camera to original position when 'R' is pressed
    if keyboard.just_pressed(KeyCode::KeyR) {
        settings.x = 0.0;
        settings.y = 300.0;
        settings.z = 500.0;
        settings.look_at_x = 0.0;
        settings.look_at_y = 0.0;
        settings.look_at_z = 0.0;
        settings.scale = 0.8;
        return;
    }

    // Pan speed based on current scale (zoom level)
    let pan_speed = 400.0 * settings.scale * time.delta_secs();

    // Handle arrow key panning - move both camera and look-at target together
    if keyboard.pressed(KeyCode::ArrowUp) {
        settings.z -= pan_speed;
        settings.look_at_z -= pan_speed;
    }
    if keyboard.pressed(KeyCode::ArrowDown) {
        settings.z += pan_speed;
        settings.look_at_z += pan_speed;
    }
    if keyboard.pressed(KeyCode::ArrowLeft) {
        settings.x -= pan_speed;
        settings.look_at_x -= pan_speed;
    }
    if keyboard.pressed(KeyCode::ArrowRight) {
        settings.x += pan_speed;
        settings.look_at_x += pan_speed;
    }

    // Handle mouse scroll wheel zooming
    for event in scroll_events.read() {
        let zoom_speed = 0.1;
        let zoom_delta = -event.y * zoom_speed; // Negative because scroll up should zoom in

        settings.scale += zoom_delta * settings.scale; // Scale zoom by current zoom level
        settings.scale = settings.scale.clamp(0.3, 0.8); // Only allow zooming out from initial scale (smaller = more zoomed out for ortho)
    }
}

fn update_camera_from_settings(
    mut camera_query: Query<(&mut Transform, &mut Projection), With<GameCamera>>,
    settings: Res<CameraSettings>,
) {
    if !settings.is_changed() {
        return;
    }

    let Ok((mut transform, mut projection)) = camera_query.single_mut() else {
        return;
    };

    transform.translation = Vec3::new(settings.x, settings.y, settings.z);
    transform.look_at(
        Vec3::new(settings.look_at_x, settings.look_at_y, settings.look_at_z),
        Vec3::Y,
    );

    if let Projection::Orthographic(ortho) = projection.as_mut() {
        ortho.scale = settings.scale;
    }
}

fn handle_camera_slider_input(
    mouse_button: Res<ButtonInput<MouseButton>>,
    windows: Query<&Window>,
    interaction_query: Query<
        (&Interaction, &CameraSliderBg, &Node, &Children),
        (With<Button>, Without<CameraSlider>)
    >,
    mut slider_query: Query<(&mut Node, &mut CameraSlider), Without<CameraSliderBg>>,
    mut settings: ResMut<CameraSettings>,
    mut label_query: Query<(&mut Text, &Name)>,
) {
    let Ok(window) = windows.single() else {
        return;
    };

    let Some(cursor_pos): Option<Vec2> = window.cursor_position() else {
        return;
    };

    // Only process when mouse is held down
    if !mouse_button.pressed(MouseButton::Left) {
        return;
    }

    for (interaction, slider_bg, bg_node, children) in &interaction_query {
        // Only process if we're hovering or pressing on this slider
        if *interaction != Interaction::Hovered && *interaction != Interaction::Pressed {
            continue;
        }

        let width = match bg_node.width {
            Val::Px(w) => w,
            _ => 280.0,
        };

        // Since we're already hovering/pressing on this specific slider,
        // Bevy's interaction system has determined the cursor is within bounds.
        // We can use a simpler approach: map the slider's expected position range.
        // The sliders are on the right side, about 300px from the right edge.
        // When hovering, we know cursor is over the slider, so use a percentage
        // based on the panel's right-aligned position.

        let window_width = window.width();
        // Panel is 300px wide, 10px from right = starts at (width - 310)
        // Sliders are 280px wide within the panel
        let panel_right = window_width - 10.0;
        let panel_left = panel_right - 300.0;
        let slider_left = panel_left + 10.0; // 10px padding
        let _slider_right = slider_left + 280.0;

        // Calculate cursor position relative to this slider
        let relative_x = (cursor_pos.x - slider_left).max(0.0);
        let percent = (relative_x / width).clamp(0.0, 1.0);

        // Update the actual slider child
        for child in children.iter() {
            if let Ok((mut slider_node, mut slider)) = slider_query.get_mut(child) {
                slider_node.width = Val::Percent(percent * 100.0);

                let new_value = slider_bg.min + (slider_bg.max - slider_bg.min) * percent;
                slider.value = new_value;

                // Update settings
                match slider_bg.param_name.as_str() {
                    "cam_x" => settings.x = new_value,
                    "cam_y" => settings.y = new_value,
                    "cam_z" => settings.z = new_value,
                    "look_x" => settings.look_at_x = new_value,
                    "look_y" => settings.look_at_y = new_value,
                    "look_z" => settings.look_at_z = new_value,
                    "scale" => settings.scale = new_value,
                    _ => {}
                }

                // Update label
                for (mut text, name) in &mut label_query {
                    if name.as_str() == format!("{}_label", slider_bg.param_name) {
                        let label = match slider_bg.param_name.as_str() {
                            "cam_x" => "Cam X: ",
                            "cam_y" => "Cam Y: ",
                            "cam_z" => "Cam Z: ",
                            "look_x" => "Look X: ",
                            "look_y" => "Look Y: ",
                            "look_z" => "Look Z: ",
                            "scale" => "Scale: ",
                            _ => "",
                        };
                        **text = format!("{}{:.1}", label, new_value);
                    }
                }
                break; // Only update one slider per background
            }
        }
    }
}

fn toggle_camera_controls_visibility(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut visible: ResMut<CameraControlsVisible>,
) {
    if keyboard.just_pressed(KeyCode::KeyH) {
        visible.0 = !visible.0;
        println!("Camera controls visibility toggled: {}", visible.0);
    }
}

fn update_camera_controls_panel_visibility(
    visible: Res<CameraControlsVisible>,
    mut query: Query<&mut Visibility, With<CameraControlsPanel>>,
) {
    for mut visibility in &mut query {
        *visibility = if visible.0 {
            Visibility::Visible
        } else {
            Visibility::Hidden
        };
    }
}

pub struct UIPlugin;

impl Plugin for UIPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(CameraSettings {
            x: 0.0,
            y: 300.0,
            z: 500.0,
            look_at_x: 0.0,
            look_at_y: 0.0,
            look_at_z: 100.0,
            scale: 0.8,
        })
        .insert_resource(CameraControlsVisible(false))
        .insert_resource(UIClicked(false))
        .add_systems(OnEnter(LoadingState::Playing), (setup_ui, setup_camera_controls))
        .add_systems(
            Update,
            (
                reset_ui_clicked,
                update_timer_ui,
                update_money_ui,
                update_hex_coordinate_display,
                handle_unit_spawn_buttons,
                update_spawn_button_visuals,
                show_game_over_screen,
                handle_restart,
            ).run_if(in_state(LoadingState::Playing)),
        )
        .add_systems(
            Update,
            (
                handle_keyboard_and_scroll_camera,
                handle_camera_slider_input,
                update_camera_from_settings,
                toggle_camera_controls_visibility,
                update_camera_controls_panel_visibility,
            ).run_if(in_state(LoadingState::Playing)),
        );
    }
}
