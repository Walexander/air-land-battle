use bevy::prelude::*;

use crate::loading::LoadingState;
use crate::map::{CrystalField, HexMapConfig, Obstacles};
use crate::units::{find_path, Army, Occupancy, Unit, UnitMovement, UnitStats};

// Economy Resources
#[derive(Resource)]
pub struct Economy {
    pub red_money: i32,
    pub blue_money: i32,
}

impl Default for Economy {
    fn default() -> Self {
        Self {
            red_money: 100,
            blue_money: 100,
        }
    }
}

#[derive(Resource)]
pub struct PassiveIncomeTimer {
    pub timer: f32,
}

impl Default for PassiveIncomeTimer {
    fn default() -> Self {
        Self { timer: 0.0 }
    }
}

// Harvester Components
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HarvesterState {
    Idle,
    MovingToField,
    Harvesting,
    MovingToBase,
}

#[derive(Component)]
pub struct Harvester {
    pub state: HarvesterState,
    pub harvest_timer: f32,
    pub harvest_duration: f32, // Time to fill up (10 seconds)
    pub crystals_carried: i32,
    pub crystal_accumulator: f32, // Fractional crystals accumulated
    pub spawn_point: (i32, i32), // Base location
    pub target_field: Option<(i32, i32)>,
}

#[derive(Component)]
struct HarvesterDepositing;

// Systems
fn harvester_ai_find_target(
    mut harvester_query: Query<(Entity, &Unit, &mut Harvester), Without<UnitMovement>>,
    crystal_query: Query<&CrystalField>,
) {
    for (_entity, unit, mut harvester) in &mut harvester_query {
        // Only process idle harvesters
        if harvester.state != HarvesterState::Idle {
            continue;
        }

        // Find closest crystal field with crystals remaining
        let mut closest_field: Option<(i32, i32, f32)> = None;

        for crystal_field in &crystal_query {
            if crystal_field.crystals_remaining <= 0 {
                continue;
            }

            let dx = (crystal_field.q - unit.q) as f32;
            let dy = (crystal_field.r - unit.r) as f32;
            let distance = (dx * dx + dy * dy).sqrt();

            if let Some((_, _, min_dist)) = closest_field {
                if distance < min_dist {
                    closest_field = Some((crystal_field.q, crystal_field.r, distance));
                }
            } else {
                closest_field = Some((crystal_field.q, crystal_field.r, distance));
            }
        }

        if let Some((target_q, target_r, _)) = closest_field {
            harvester.target_field = Some((target_q, target_r));
            harvester.state = HarvesterState::MovingToField;
            println!("Harvester at ({}, {}) targeting crystal field at ({}, {})",
                unit.q, unit.r, target_q, target_r);
        }
    }
}

// Command harvesters to move to their target fields
fn harvester_move_to_field(
    mut commands: Commands,
    mut harvester_query: Query<(Entity, &Unit, &UnitStats, &mut Harvester), Without<UnitMovement>>,
    occupancy: Res<Occupancy>,
    obstacles: Res<Obstacles>,
    config: Res<HexMapConfig>,
) {
    for (entity, unit, stats, mut harvester) in &mut harvester_query {
        if harvester.state != HarvesterState::MovingToField {
            continue;
        }

        if let Some((target_q, target_r)) = harvester.target_field {
            // Check if we've arrived
            if unit.q == target_q && unit.r == target_r {
                harvester.state = HarvesterState::Harvesting;
                harvester.harvest_timer = 0.0;
                harvester.crystal_accumulator = 0.0;
                println!("Harvester arrived at crystal field ({}, {}), starting to harvest", target_q, target_r);
                continue;
            }

            // Build blocking cells set (obstacles + occupied cells, excluding current position and goal)
            let mut blocking_cells = obstacles.positions.clone();
            for &occupied_pos in &occupancy.positions {
                if occupied_pos != (unit.q, unit.r) && occupied_pos != (target_q, target_r) {
                    blocking_cells.insert(occupied_pos);
                }
            }

            // Use proper A* pathfinding
            if let Some(path) = find_path((unit.q, unit.r), (target_q, target_r), config.map_radius, &blocking_cells) {
                let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                    path[1..].to_vec()
                } else {
                    vec![]
                };

                if !path_to_follow.is_empty() {
                    commands.entity(entity).insert(UnitMovement {
                        path: path_to_follow,
                        current_waypoint: 0,
                        progress: 0.0,
                        speed: stats.speed,
                        segment_start: (unit.q, unit.r),
                    });
                }
            }
        }
    }
}

// Passive income system - players earn money even without harvesters
// at 25% of a harvester's rate (1.25 crystals/sec vs 5 crystals/sec)
fn passive_income_system(
    time: Res<Time>,
    mut timer: ResMut<PassiveIncomeTimer>,
    mut economy: ResMut<Economy>,
) {
    timer.timer += time.delta_secs();

    // Award 1.25 crystals per second = 1 crystal every 0.8 seconds
    const INCOME_INTERVAL: f32 = 0.8;

    while timer.timer >= INCOME_INTERVAL {
        timer.timer -= INCOME_INTERVAL;
        economy.red_money += 1;
        economy.blue_money += 1;
    }
}

// Harvest crystals over time
fn harvester_collect_crystals(
    time: Res<Time>,
    mut economy: ResMut<Economy>,
    mut harvester_query: Query<(&Unit, &Army, &mut Harvester)>,
    mut crystal_query: Query<&mut CrystalField>,
) {
    for (unit, army, mut harvester) in &mut harvester_query {
        if harvester.state != HarvesterState::Harvesting {
            continue;
        }

        harvester.harvest_timer += time.delta_secs();

        // Collect crystals over time (5 crystals per second = 50 total in 10 seconds)
        let crystals_per_second = 5.0;
        harvester.crystal_accumulator += crystals_per_second * time.delta_secs();

        // Extract integer crystals from accumulator
        let delta_crystals = harvester.crystal_accumulator.floor() as i32;
        if delta_crystals > 0 {
            harvester.crystal_accumulator -= delta_crystals as f32;

            // Find the crystal field at this position
            for mut crystal_field in &mut crystal_query {
                if crystal_field.q == unit.q && crystal_field.r == unit.r {
                    if crystal_field.crystals_remaining > 0 {
                        let amount = delta_crystals.min(crystal_field.crystals_remaining);
                        crystal_field.crystals_remaining -= amount;
                        harvester.crystals_carried += amount;

                        // Pay money immediately as crystals are collected (1 crystal = 1 money)
                        match army {
                            Army::Red => economy.red_money += amount,
                            Army::Blue => economy.blue_money += amount,
                        }
                    }
                    break;
                }
            }
        }

        // Check if full (10 seconds passed or carried 50 crystals)
        if harvester.harvest_timer >= harvester.harvest_duration || harvester.crystals_carried >= 50 {
            harvester.state = HarvesterState::MovingToBase;
            println!("Harvester full with {} crystals, returning to base at ({}, {})",
                harvester.crystals_carried, harvester.spawn_point.0, harvester.spawn_point.1);
        }
    }
}

// Command harvesters to return to base
fn harvester_return_to_base(
    mut commands: Commands,
    mut harvester_query: Query<(Entity, &Unit, &UnitStats, &mut Harvester), Without<UnitMovement>>,
    occupancy: Res<Occupancy>,
    obstacles: Res<Obstacles>,
    config: Res<HexMapConfig>,
) {
    for (entity, unit, stats, harvester) in &mut harvester_query {
        if harvester.state != HarvesterState::MovingToBase {
            continue;
        }

        let (base_q, base_r) = harvester.spawn_point;

        // Check if we've arrived at base
        if unit.q == base_q && unit.r == base_r {
            // Deposit crystals and reset
            commands.entity(entity).insert(HarvesterDepositing);
            println!("Harvester arrived at base, depositing {} crystals", harvester.crystals_carried);
            continue;
        }

        // Build blocking cells set (obstacles + occupied cells, excluding current position and goal)
        let mut blocking_cells = obstacles.positions.clone();
        for &occupied_pos in &occupancy.positions {
            if occupied_pos != (unit.q, unit.r) && occupied_pos != (base_q, base_r) {
                blocking_cells.insert(occupied_pos);
            }
        }

        // Use proper A* pathfinding
        if let Some(path) = find_path((unit.q, unit.r), (base_q, base_r), config.map_radius, &blocking_cells) {
            let path_to_follow: Vec<(i32, i32)> = if path.len() > 1 {
                path[1..].to_vec()
            } else {
                vec![]
            };

            if !path_to_follow.is_empty() {
                commands.entity(entity).insert(UnitMovement {
                    path: path_to_follow,
                    current_waypoint: 0,
                    progress: 0.0,
                    speed: stats.speed,
                    segment_start: (unit.q, unit.r),
                });
            }
        }
    }
}

// Deposit crystals and reset harvester state
fn harvester_deposit_crystals(
    mut commands: Commands,
    mut harvester_query: Query<(Entity, &Army, &mut Harvester), With<HarvesterDepositing>>,
) {
    for (entity, army, mut harvester) in &mut harvester_query {
        // Money was already paid during collection, just reset the harvester
        println!("{:?} harvester returned to base with {} crystals collected",
            army, harvester.crystals_carried);

        // Reset harvester state
        harvester.state = HarvesterState::Idle;
        harvester.crystals_carried = 0;
        harvester.target_field = None;

        // Remove depositing marker
        commands.entity(entity).remove::<HarvesterDepositing>();
    }
}

// Economy Plugin
pub struct EconomyPlugin;

impl Plugin for EconomyPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<Economy>()
            .init_resource::<PassiveIncomeTimer>()
            .add_systems(
                Update,
                (
                    passive_income_system,
                    harvester_ai_find_target,
                    harvester_move_to_field,
                    harvester_collect_crystals,
                    harvester_return_to_base,
                    harvester_deposit_crystals,
                )
                    .run_if(in_state(LoadingState::Playing)),
            );
    }
}
