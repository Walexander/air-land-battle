use bevy::prelude::*;

use crate::loading::LoadingState;
use crate::map::{CrystalField, HexMapConfig, Obstacles};
use crate::units::{find_path_waypoints, Army, Occupancy, Unit, UnitClass, UnitMovement, UnitStats, UnitDefinitions};

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
}

#[derive(Component)]
pub struct Harvester {
    pub state: HarvesterState,
    pub harvest_timer: f32,
    pub crystals_carried: i32,
    pub crystal_accumulator: f32, // Fractional crystals accumulated
    pub target_field: Option<(i32, i32)>,
}

// Systems
fn harvester_ai_find_target(
    mut harvester_query: Query<(Entity, &Unit, &mut Harvester), Without<UnitMovement>>,
    crystal_query: Query<&CrystalField>,
    occupancy: Res<Occupancy>,
) {
    // First pass: collect all currently targeted fields (immutable access)
    let mut targeted_fields = std::collections::HashSet::new();
    for (_, _, harvester) in harvester_query.iter() {
        if let Some(target) = harvester.target_field {
            targeted_fields.insert(target);
        }
    }

    // Second pass: find targets for idle harvesters (mutable access)
    for (_entity, unit, mut harvester) in &mut harvester_query {
        // Only process idle harvesters
        if harvester.state != HarvesterState::Idle {
            continue;
        }

        // Find closest crystal field with crystals remaining that is unoccupied and not already targeted
        let mut closest_field: Option<(i32, i32, f32)> = None;

        for crystal_field in &crystal_query {
            let field_pos = (crystal_field.q, crystal_field.r);

            // Skip if field is depleted
            if crystal_field.crystals_remaining <= 0 {
                continue;
            }

            // Skip if field is occupied (unless it's our current position)
            if occupancy.positions.contains(&field_pos) && field_pos != (unit.q, unit.r) {
                continue;
            }

            // Skip if another harvester is already targeting this field
            if targeted_fields.contains(&field_pos) {
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
            // Add this new target to the set so subsequent harvesters don't also target it
            targeted_fields.insert((target_q, target_r));
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
    hex_grid: Res<crate::hex_pathfinding::HexPathfindingGrid>,
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

            // Use world-space waypoint pathfinding
            if let Some(waypoints) = find_path_waypoints((unit.q, unit.r), (target_q, target_r), &config.valid_cells, &blocking_cells, &hex_grid) {
                if waypoints.len() > 1 {
                    commands.entity(entity).insert(UnitMovement {
                        waypoints,
                        current_waypoint: 1,
                        progress: 0.0,
                        speed: stats.speed,
                        segment_distance: 0.0,
                        segment_start: Vec3::ZERO,
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
    mut harvester_query: Query<(&Unit, &Army, &UnitClass, &mut Harvester)>,
    mut crystal_query: Query<&mut CrystalField>,
    unit_definitions: Res<UnitDefinitions>,
) {
    for (unit, army, unit_class, mut harvester) in &mut harvester_query {
        if harvester.state != HarvesterState::Harvesting {
            continue;
        }

        harvester.harvest_timer += time.delta_secs();

        // Get harvester behavior from unit definition
        let unit_def = unit_class.definition(&unit_definitions);
        let harvester_behavior = unit_def.harvester_behavior.as_ref()
            .expect("Harvester unit should have harvester_behavior");

        let crystals_per_second = harvester_behavior.crystals_per_second;
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

        // Check if current field is depleted
        let field_depleted = crystal_query.iter().any(|field| {
            field.q == unit.q && field.r == unit.r && field.crystals_remaining <= 0
        });

        if field_depleted {
            harvester.state = HarvesterState::Idle;
            harvester.target_field = None;
            harvester.crystals_carried = 0;
            println!("Crystal field at ({}, {}) depleted, harvester searching for new field", unit.q, unit.r);
        }
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
                )
                    .run_if(in_state(LoadingState::Playing).and(not_paused)),
            );
    }
}

fn not_paused(paused: Res<crate::Paused>) -> bool {
    !paused.0
}
