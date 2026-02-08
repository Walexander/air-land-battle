use bevy::prelude::*;

use crate::units::{Army, Unit};
use crate::loading::LoadingState;

// Constants
pub const GAME_DURATION: f32 = 50.0;

// Enums
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum LaunchPadOwner {
    Neutral,
    Contested,
    Red,
    Blue,
}

// Resources
#[derive(Resource)]
pub struct LaunchPads {
    pub pads: Vec<Vec<(i32, i32)>>,
}

#[derive(Resource)]
pub struct GameTimer {
    pub time_remaining: f32,
    pub is_active: bool,
    pub winning_army: Option<Army>,
}

impl Default for GameTimer {
    fn default() -> Self {
        Self {
            time_remaining: GAME_DURATION,
            is_active: false,
            winning_army: None,
        }
    }
}

#[derive(Resource, Default)]
pub struct GameState {
    pub winner: Option<Army>,
    pub game_over: bool,
}

#[derive(Resource, Default)]
pub struct LaunchPadOwnership {
    pub owners: Vec<LaunchPadOwner>,
}

// Systems
fn check_launch_pad_ownership(
    unit_query: Query<&Unit>,
    launch_pads: Res<LaunchPads>,
    mut game_timer: ResMut<GameTimer>,
    mut game_state: ResMut<GameState>,
    mut pad_ownership: ResMut<LaunchPadOwnership>,
    time: Res<Time>,
) {
    if game_state.game_over {
        return;
    }

    let mut pad_owners: Vec<LaunchPadOwner> = Vec::new();

    for pad in &launch_pads.pads {
        let mut has_red = false;
        let mut has_blue = false;

        for unit in unit_query.iter() {
            let unit_pos = (unit.q, unit.r);
            if pad.contains(&unit_pos) {
                match unit.army {
                    Army::Red => has_red = true,
                    Army::Blue => has_blue = true,
                }
            }
        }

        let owner = if has_red && has_blue {
            LaunchPadOwner::Contested
        } else if has_red {
            LaunchPadOwner::Red
        } else if has_blue {
            LaunchPadOwner::Blue
        } else {
            LaunchPadOwner::Neutral
        };

        pad_owners.push(owner);
    }

    // Store ownership state for visualization
    pad_ownership.owners = pad_owners.clone();

    let red_count = pad_owners
        .iter()
        .filter(|&&o| o == LaunchPadOwner::Red)
        .count();
    let blue_count = pad_owners
        .iter()
        .filter(|&&o| o == LaunchPadOwner::Blue)
        .count();

    if red_count > blue_count {
        if !game_timer.is_active {
            game_timer.is_active = true;
            game_timer.winning_army = Some(Army::Red);
            println!(
                "Red army controls majority of launch pads! Timer started at {:.1}s.",
                game_timer.time_remaining
            );
        } else if game_timer.winning_army != Some(Army::Red) {
            game_timer.winning_army = Some(Army::Red);
            println!(
                "Ownership changed to Red army! Timer continues at {:.1}s.",
                game_timer.time_remaining
            );
        }

        game_timer.time_remaining -= time.delta_secs();
        if game_timer.time_remaining <= 0.0 {
            println!("Red army wins!");
            game_state.game_over = true;
            game_state.winner = Some(Army::Red);
            game_timer.is_active = false;
        }
    } else if blue_count > red_count {
        if !game_timer.is_active {
            game_timer.is_active = true;
            game_timer.winning_army = Some(Army::Blue);
            println!(
                "Blue army controls majority of launch pads! Timer started at {:.1}s.",
                game_timer.time_remaining
            );
        } else if game_timer.winning_army != Some(Army::Blue) {
            game_timer.winning_army = Some(Army::Blue);
            println!(
                "Ownership changed to Blue army! Timer continues at {:.1}s.",
                game_timer.time_remaining
            );
        }

        game_timer.time_remaining -= time.delta_secs();
        if game_timer.time_remaining <= 0.0 {
            println!("Blue army wins!");
            game_state.game_over = true;
            game_state.winner = Some(Army::Blue);
            game_timer.is_active = false;
        }
    } else {
        // Launch pads are tied/neutral
        if game_timer.time_remaining < 10.0 {
            // When under 10 seconds, timer counts back up (at same rate it counts down)
            game_timer.time_remaining += time.delta_secs();
            // Clamp to not exceed 10 seconds
            if game_timer.time_remaining > 10.0 {
                game_timer.time_remaining = 10.0;
            }
        }

        if game_timer.is_active {
            println!(
                "Launch pads tied. Timer at {:.1}s.",
                game_timer.time_remaining
            );
        }
        game_timer.is_active = false;
        game_timer.winning_army = None;
    }
}

pub struct LaunchPadsPlugin;

impl Plugin for LaunchPadsPlugin {
    fn build(&self, app: &mut App) {
        let mut launch_pads = LaunchPads { pads: Vec::new() };
        // Launch platform 1 (left side)
        launch_pads
            .pads
            .push(vec![(-3, 3), (-3, 2), (-2, 2)]);
        // Launch platform 2 (center)
        launch_pads
            .pads
            .push(vec![(0, 2), (1, 2), (0, 3)]);
        // Launch platform 3 (bottom right)
        launch_pads
            .pads
            .push(vec![(1, -1), (1, -2), (0, -1), (0, 0)]);

        app.insert_resource(launch_pads)
            .insert_resource(GameTimer::default())
            .insert_resource(GameState::default())
            .insert_resource(LaunchPadOwnership::default())
            .add_systems(Update, check_launch_pad_ownership.run_if(in_state(LoadingState::Playing)));
    }
}
