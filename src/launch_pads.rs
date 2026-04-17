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
    pub missile_animation_complete: bool,
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
        if game_timer.time_remaining < 5.0 {
            // When under 5 seconds, timer counts back up (at same rate it counts down)
            game_timer.time_remaining += time.delta_secs();
            // Clamp to not exceed 5 seconds
            if game_timer.time_remaining > 5.0 {
                game_timer.time_remaining = 5.0;
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
        app.insert_resource(LaunchPads { pads: Vec::new() })
            .insert_resource(GameTimer::default())
            .insert_resource(GameState::default())
            .insert_resource(LaunchPadOwnership::default())
            .add_systems(OnEnter(LoadingState::Playing), populate_launch_pads)
            .add_systems(Update, check_launch_pad_ownership.run_if(in_state(LoadingState::Playing).and(not_paused)));
    }
}

fn populate_launch_pads(
    mut launch_pads: ResMut<LaunchPads>,
    map_def: Res<crate::map_loader::MapDefinition>,
    mut game_timer: ResMut<GameTimer>,
    mut game_state: ResMut<GameState>,
    mut pad_ownership: ResMut<LaunchPadOwnership>,
) {
    launch_pads.pads = map_def.launch_pads.clone();
    println!("LaunchPads: loaded {} pads from map definition", launch_pads.pads.len());
    *game_timer = GameTimer::default();
    *game_state = GameState::default();
    *pad_ownership = LaunchPadOwnership::default();
}

fn not_paused(paused: Res<crate::Paused>) -> bool {
    !paused.0
}
