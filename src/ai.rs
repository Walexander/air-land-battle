use bevy::prelude::*;
use std::collections::HashSet;

use crate::loading::LoadingState;
use crate::launch_pads::{GameTimer, LaunchPadOwner, LaunchPadOwnership, LaunchPads};
use crate::map::{HexMapConfig, Obstacles};
use crate::units::{
    find_path, hex_distance, Army, ClaimedCellsThisFrame, Economy, Occupancy,
    OccupancyIntent, Unit, UnitClass, UnitMovement, UnitSpawnQueue, UnitSpawnRequest, UnitStats, SpawnCooldowns,
};

// AI Strategy types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AIStrategy {
    Economic,      // Focus on building harvesters and income
    Aggressive,    // Attack enemy units and disrupt economy
    Expansionist,  // Capture neutral pads
    Defensive,     // Protect our pads
}

#[derive(Resource)]
pub struct AIController {
    pub spawn_timer: f32,
    pub spawn_interval: f32,
    pub command_timer: f32,
    pub command_interval: f32,
    pub strategy: AIStrategy,
    pub strategy_timer: f32, // Time to reconsider strategy
    pub strategy_commitment_timer: f32, // Time in current strategy
    pub min_commitment_time: f32, // Minimum time before switching strategies
}

impl Default for AIController {
    fn default() -> Self {
        Self {
            spawn_timer: 0.0,
            spawn_interval: 2.0, // Check every 2 seconds if should spawn
            command_timer: 0.0,
            command_interval: 1.5, // Give commands every 1.5 seconds
            strategy: AIStrategy::Economic, // Start with economy
            strategy_timer: 0.0,
            strategy_commitment_timer: 0.0,
            min_commitment_time: 20.0, // Stay in strategy for at least 20 seconds
        }
    }
}

// AI Systems
fn ai_spawn_units(
    time: Res<Time>,
    mut ai_controller: ResMut<AIController>,
    economy: Res<Economy>,
    spawn_cooldowns: Res<SpawnCooldowns>,
    mut spawn_queue: ResMut<UnitSpawnQueue>,
    unit_query: Query<(&Unit, &UnitClass)>,
    pad_ownership: Res<LaunchPadOwnership>,
    launch_pads: Res<LaunchPads>,
    game_timer: Res<GameTimer>,
) {
    ai_controller.spawn_timer += time.delta_secs();
    ai_controller.strategy_timer += time.delta_secs();
    ai_controller.strategy_commitment_timer += time.delta_secs();

    if ai_controller.spawn_timer < ai_controller.spawn_interval {
        return;
    }

    ai_controller.spawn_timer = 0.0;

    // Reconsider strategy every 15 seconds
    if ai_controller.strategy_timer >= 15.0 {
        ai_controller.strategy_timer = 0.0;

        // Evaluate what the best strategy would be
        let new_strategy = evaluate_strategy(
            &unit_query,
            &economy,
            &pad_ownership,
            &launch_pads,
            &game_timer,
        );

        // Only switch strategies if:
        // 1. We've been in current strategy for at least min_commitment_time
        // 2. The new strategy is different from current
        if new_strategy != ai_controller.strategy {
            if ai_controller.strategy_commitment_timer >= ai_controller.min_commitment_time {
                println!("🔄 AI Strategy change: {:?} -> {:?} (after {:.1}s)",
                    ai_controller.strategy, new_strategy, ai_controller.strategy_commitment_timer);
                ai_controller.strategy = new_strategy;
                ai_controller.strategy_commitment_timer = 0.0;
            } else {
                println!("⏳ AI wants to switch to {:?} but committed to {:?} for {:.1}s more",
                    new_strategy, ai_controller.strategy,
                    ai_controller.min_commitment_time - ai_controller.strategy_commitment_timer);
            }
        }
    }

    // Count blue units by type and army
    let mut blue_infantry = 0;
    let mut blue_cavalry = 0;
    let mut blue_artillery = 0;
    let mut blue_harvesters = 0;

    for (unit, unit_class) in unit_query.iter() {
        if unit.army == Army::Blue {
            match unit_class {
                UnitClass::Infantry => blue_infantry += 1,
                UnitClass::Cavalry => blue_cavalry += 1,
                UnitClass::Artillery => blue_artillery += 1,
                UnitClass::Harvester => blue_harvesters += 1,
            }
        }
    }

    let blue_combat_units = blue_infantry + blue_cavalry + blue_artillery;
    let blue_money = economy.blue_money;

    // Decide what to spawn based on strategy
    let unit_to_spawn = match ai_controller.strategy {
        AIStrategy::Economic => {
            // Build only 1 harvester initially for economy
            if blue_harvesters < 1 && blue_money >= UnitClass::Harvester.cost() {
                Some(UnitClass::Harvester)
            }
            // Then build cheap infantry for defense
            else if blue_money >= UnitClass::Infantry.cost() {
                Some(UnitClass::Infantry)
            } else {
                None
            }
        }
        AIStrategy::Aggressive => {
            // Build a mix of combat units, prioritizing artillery if we can afford it
            if blue_money >= UnitClass::Artillery.cost() && blue_artillery < blue_combat_units / 3 {
                Some(UnitClass::Artillery)
            } else if blue_money >= UnitClass::Cavalry.cost() && blue_cavalry < blue_combat_units / 3 {
                Some(UnitClass::Cavalry)
            } else if blue_money >= UnitClass::Infantry.cost() {
                Some(UnitClass::Infantry)
            } else {
                None
            }
        }
        AIStrategy::Expansionist => {
            // Build fast cavalry for capturing pads
            if blue_money >= UnitClass::Cavalry.cost() && blue_cavalry < blue_combat_units / 2 {
                Some(UnitClass::Cavalry)
            } else if blue_money >= UnitClass::Infantry.cost() {
                Some(UnitClass::Infantry)
            } else {
                None
            }
        }
        AIStrategy::Defensive => {
            // Build infantry quickly for defense, then artillery
            if blue_money >= UnitClass::Infantry.cost() && blue_combat_units < 3 {
                Some(UnitClass::Infantry) // Get 3 infantry out quickly
            } else if blue_money >= UnitClass::Artillery.cost() && blue_artillery < 1 {
                Some(UnitClass::Artillery) // Then add artillery
            } else if blue_money >= UnitClass::Infantry.cost() {
                Some(UnitClass::Infantry) // Back to infantry
            } else {
                None
            }
        }
    };

    // Spawn the chosen unit if cooldown is ready
    if let Some(unit_class) = unit_to_spawn {
        // Count current Blue army units (including harvesters)
        let blue_unit_count = unit_query.iter()
            .filter(|(u, _uc)| u.army == Army::Blue)
            .count();

        let blue_cooldowns = spawn_cooldowns.get_army_cooldowns(Army::Blue);
        if blue_cooldowns.is_ready(unit_class, blue_unit_count) {
            spawn_queue.requests.push(UnitSpawnRequest {
                unit_class,
                army: Army::Blue,
            });
            println!("AI ({:?} strategy): Spawning {:?}", ai_controller.strategy, unit_class);
        }
    }
}

// Evaluate which strategy the AI should use based on game state
fn evaluate_strategy(
    unit_query: &Query<(&Unit, &UnitClass)>,
    economy: &Economy,
    pad_ownership: &LaunchPadOwnership,
    _launch_pads: &LaunchPads,
    game_timer: &GameTimer,
) -> AIStrategy {
    let mut blue_units = 0;
    let mut red_units = 0;
    let mut blue_harvesters = 0;
    let mut blue_combat_units = 0;
    let mut red_combat_units = 0;

    for (unit, unit_class) in unit_query.iter() {
        match unit.army {
            Army::Blue => {
                blue_units += 1;
                if *unit_class == UnitClass::Harvester {
                    blue_harvesters += 1;
                } else {
                    blue_combat_units += 1;
                }
            }
            Army::Red => {
                red_units += 1;
                if *unit_class != UnitClass::Harvester {
                    red_combat_units += 1;
                }
            }
        }
    }

    // Count controlled pads
    let mut blue_pads = 0;
    let mut neutral_pads = 0;
    let mut red_pads = 0;
    let mut contested_pads = 0;

    for owner in &pad_ownership.owners {
        match owner {
            LaunchPadOwner::Blue => blue_pads += 1,
            LaunchPadOwner::Neutral => neutral_pads += 1,
            LaunchPadOwner::Red => red_pads += 1,
            LaunchPadOwner::Contested => contested_pads += 1,
        }
    }

    // CRITICAL 1: Early game rush detection
    // If player has combat units and we don't, respond immediately!
    if red_combat_units > 0 && blue_combat_units == 0 {
        println!("⚠️ EARLY RUSH DETECTED: Player has {} combat units, we have 0 - BUILDING ARMY!", red_combat_units);
        return AIStrategy::Defensive; // Build units to defend, not harvesters
    }

    // If player is way ahead in combat units early, stop building economy
    if red_combat_units >= 2 && blue_combat_units < red_combat_units {
        println!("⚠️ PLAYER ARMY ADVANTAGE: Red {} vs Blue {} - BUILDING COMBAT UNITS!", red_combat_units, blue_combat_units);
        return AIStrategy::Aggressive; // Focus on combat
    }

    // CRITICAL 2: Check win condition urgency
    // If timer is active and counting down, this overrides all other strategies
    if game_timer.is_active
        && let Some(winning_army) = game_timer.winning_army {
            match winning_army {
                Army::Red => {
                    // Player is winning! We MUST attack and contest their pads immediately
                    println!("🚨 EMERGENCY: Player winning with {:.1}s left - ATTACKING PADS!", game_timer.time_remaining);
                    return AIStrategy::Aggressive; // Attack enemy pads aggressively
                }
                Army::Blue => {
                    // We're winning! Different behavior based on time remaining
                    if game_timer.time_remaining < 10.0 {
                        println!("🎯 AI WINNING: {:.1}s left - HOLD POSITIONS!", game_timer.time_remaining);
                        return AIStrategy::Defensive; // Just hold what we have
                    }
                }
            }
        }

    // Score all strategies (timer urgency is handled above, this is for normal play)
    let mut scores = [(AIStrategy::Economic, score_economic_strategy(blue_harvesters, economy.blue_money)),
        (AIStrategy::Aggressive, score_aggressive_strategy(blue_units, red_units, economy.blue_money)),
        (AIStrategy::Expansionist, score_expansionist_strategy(blue_pads, red_pads, neutral_pads)),
        (AIStrategy::Defensive, score_defensive_strategy(blue_units, red_units, blue_pads, red_pads, contested_pads))];

    // Sort by score descending
    scores.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

    scores[0].0
}

// Helper functions to score each strategy
fn score_economic_strategy(blue_harvesters: usize, blue_money: i32) -> f32 {
    let mut score = 0.0;

    // Strong need for economy if no harvesters
    if blue_harvesters == 0 {
        score += 50.0;
    }

    // Moderate need for second harvester if low money
    if blue_harvesters == 1 && blue_money < 80 {
        score += 20.0; // Reduced from 30
    }

    // Economy is less valuable if we already have good income
    if blue_harvesters >= 2 {
        score -= 40.0; // Increased penalty
    }

    // After first harvester, economy is less urgent
    if blue_harvesters >= 1 {
        score -= 15.0;
    }

    score
}

fn score_aggressive_strategy(blue_units: usize, red_units: usize, blue_money: i32) -> f32 {
    let mut score = 0.0;

    // Good if we have military advantage
    if blue_units > red_units {
        score += 40.0;
    }

    // Good if we have money to sustain aggression
    if blue_money > 100 {
        score += 30.0;
    }

    // Bad if we're outnumbered
    if red_units > blue_units {
        score -= 20.0;
    }

    score
}

fn score_expansionist_strategy(blue_pads: usize, red_pads: usize, neutral_pads: usize) -> f32 {
    let mut score = 0.0;

    // Strong priority if neutral pads exist
    if neutral_pads > 0 {
        score += 50.0;
    }

    // Higher priority if enemy has more pads
    if red_pads > blue_pads {
        score += 30.0;
    }

    // No point in expansion if no pads to capture
    if neutral_pads == 0 && red_pads <= blue_pads {
        score -= 40.0;
    }

    score
}

fn score_defensive_strategy(blue_units: usize, red_units: usize, blue_pads: usize, red_pads: usize, contested_pads: usize) -> f32 {
    let mut score = 0.0;

    // High priority if outnumbered
    if red_units > blue_units * 2 {
        score += 60.0;
    } else if red_units > blue_units {
        score += 30.0;
    }

    // Very high priority if pads are contested
    if contested_pads > 0 {
        score += 50.0;
    }

    // Defensive if we're winning on pads (protect what we have)
    if blue_pads > red_pads {
        score += 20.0;
    }

    // Lower priority if we have advantage
    if blue_units > red_units {
        score -= 20.0;
    }

    score
}

fn ai_command_units(
    time: Res<Time>,
    mut ai_controller: ResMut<AIController>,
    mut commands: Commands,
    launch_pads: Res<LaunchPads>,
    pad_ownership: Res<LaunchPadOwnership>,
    obstacles: Res<Obstacles>,
    map_config: Res<HexMapConfig>,
    occupancy: Res<Occupancy>,
    occupancy_intent: Res<OccupancyIntent>,
    mut claimed_cells: ResMut<ClaimedCellsThisFrame>,
    unit_query: Query<(Entity, &Unit, &UnitStats, &UnitClass, Option<&UnitMovement>)>,
) {
    ai_controller.command_timer += time.delta_secs();

    if ai_controller.command_timer < ai_controller.command_interval {
        return;
    }

    ai_controller.command_timer = 0.0;

    // Find all idle blue units (units without movement or who have reached destination)
    let mut idle_blue_units = Vec::new();
    let mut all_red_units = Vec::new();

    for (entity, unit, stats, unit_class, movement) in unit_query.iter() {
        if unit.army == Army::Blue {
            // Unit is idle if it has no movement component or has reached end of path
            let is_idle = movement.is_none()
                || movement.is_some_and(|m| m.current_waypoint >= m.path.len());
            if is_idle {
                idle_blue_units.push((entity, unit, stats, unit_class));
            }
        } else if unit.army == Army::Red {
            all_red_units.push((unit.q, unit.r, unit_class));
        }
    }

    if idle_blue_units.is_empty() {
        return;
    }

    // Find targets based on current strategy
    let strategy = ai_controller.strategy;

    println!("🤖 AI commanding {} idle units (Strategy: {:?})", idle_blue_units.len(), strategy);

    for (entity, unit, stats, unit_class) in idle_blue_units {
        let unit_pos = (unit.q, unit.r);

        // Build blocking cells set
        let mut blocking_cells = obstacles.positions.clone();
        for &occupied_pos in &occupancy.positions {
            if occupied_pos != unit_pos {
                blocking_cells.insert(occupied_pos);
            }
        }
        for (other_entity, &intent_pos) in &occupancy_intent.intentions {
            if *other_entity != entity && intent_pos != unit_pos {
                blocking_cells.insert(intent_pos);
            }
        }
        for &claimed_cell in &claimed_cells.cells {
            if claimed_cell != unit_pos {
                blocking_cells.insert(claimed_cell);
            }
        }

        // Choose target based on strategy
        let target = match strategy {
            AIStrategy::Economic => {
                // Harvesters do their own thing, combat units defend blue pads
                if *unit_class == UnitClass::Harvester {
                    None // Harvesters have their own AI
                } else {
                    // Defend our pads, attack enemy pads, or capture neutral pads (game start)
                    find_defensive_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells)
                        .or_else(|| find_enemy_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
                        .or_else(|| find_neutral_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
                }
            }
            AIStrategy::Aggressive => {
                // Priority: Enemy harvesters > Enemy units on pads > Enemy pads
                find_enemy_harvester_target(unit_pos, &all_red_units, &blocking_cells)
                    .or_else(|| find_enemy_unit_target(unit_pos, &all_red_units, &launch_pads, &pad_ownership, &blocking_cells))
                    .or_else(|| find_enemy_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
                    .or_else(|| find_neutral_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
            }
            AIStrategy::Expansionist => {
                // Priority: Neutral pads > Enemy pads
                find_neutral_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells)
                    .or_else(|| find_enemy_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
            }
            AIStrategy::Defensive => {
                // Protect our pads, stay near them
                // If no blue/contested pads to defend, attack enemy pads to contest them
                find_defensive_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells)
                    .or_else(|| find_enemy_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
                    .or_else(|| find_neutral_pad_target(unit_pos, &launch_pads, &pad_ownership, &blocking_cells))
            }
        };

        // Command unit to move to target
        if let Some(target_pos) = target {
            if let Some(path) = find_path(unit_pos, target_pos, map_config.map_radius, &blocking_cells)
                && path.len() > 1 {
                    let path_to_follow: Vec<(i32, i32)> = path[1..].to_vec();

                    commands.entity(entity).insert(UnitMovement {
                        path: path_to_follow.clone(),
                        current_waypoint: 0,
                        progress: 0.0,
                        speed: stats.speed,
                        segment_start: unit_pos,
                    });

                    for &cell in &path_to_follow {
                        claimed_cells.cells.insert(cell);
                    }
                }
        } else {
            println!("⚠️ AI unit at ({}, {}) has NO TARGET (Strategy: {:?}, Class: {:?})",
                unit_pos.0, unit_pos.1, strategy, unit_class);
        }
    }
}

// Helper functions for target selection
fn find_enemy_harvester_target(
    unit_pos: (i32, i32),
    red_units: &[(i32, i32, &UnitClass)],
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    let harvesters: Vec<(i32, i32)> = red_units
        .iter()
        .filter(|(_, _, class)| **class == UnitClass::Harvester)
        .map(|(q, r, _)| (*q, *r))
        .filter(|pos| !blocking_cells.contains(pos))
        .collect();

    find_nearest(unit_pos, &harvesters)
}

fn find_enemy_unit_target(
    unit_pos: (i32, i32),
    red_units: &[(i32, i32, &UnitClass)],
    launch_pads: &LaunchPads,
    pad_ownership: &LaunchPadOwnership,
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    // Find enemy units that are on pads (threatening capture)
    let mut enemy_positions_on_pads = Vec::new();

    for (pad_index, pad_tiles) in launch_pads.pads.iter().enumerate() {
        let owner = pad_ownership.owners.get(pad_index).copied().unwrap_or(LaunchPadOwner::Neutral);

        // Look for enemy units on neutral or our pads
        if owner != LaunchPadOwner::Red {
            for &(eq, er, _) in red_units {
                if pad_tiles.contains(&(eq, er)) && !blocking_cells.contains(&(eq, er)) {
                    enemy_positions_on_pads.push((eq, er));
                }
            }
        }
    }

    find_nearest(unit_pos, &enemy_positions_on_pads)
}

fn find_enemy_pad_target(
    unit_pos: (i32, i32),
    launch_pads: &LaunchPads,
    pad_ownership: &LaunchPadOwnership,
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    let mut targets = Vec::new();

    for (pad_index, pad_tiles) in launch_pads.pads.iter().enumerate() {
        let owner = pad_ownership.owners.get(pad_index).copied().unwrap_or(LaunchPadOwner::Neutral);

        if owner == LaunchPadOwner::Red || owner == LaunchPadOwner::Contested {
            for &(q, r) in pad_tiles {
                if !blocking_cells.contains(&(q, r)) {
                    targets.push((q, r));
                }
            }
        }
    }

    find_nearest(unit_pos, &targets)
}

fn find_neutral_pad_target(
    unit_pos: (i32, i32),
    launch_pads: &LaunchPads,
    pad_ownership: &LaunchPadOwnership,
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    let mut targets = Vec::new();

    for (pad_index, pad_tiles) in launch_pads.pads.iter().enumerate() {
        let owner = pad_ownership.owners.get(pad_index).copied().unwrap_or(LaunchPadOwner::Neutral);

        if owner == LaunchPadOwner::Neutral {
            for &(q, r) in pad_tiles {
                if !blocking_cells.contains(&(q, r)) {
                    targets.push((q, r));
                }
            }
        }
    }

    find_nearest(unit_pos, &targets)
}

fn find_defensive_target(
    unit_pos: (i32, i32),
    launch_pads: &LaunchPads,
    pad_ownership: &LaunchPadOwnership,
    blocking_cells: &HashSet<(i32, i32)>,
) -> Option<(i32, i32)> {
    let mut targets = Vec::new();

    for (pad_index, pad_tiles) in launch_pads.pads.iter().enumerate() {
        let owner = pad_ownership.owners.get(pad_index).copied().unwrap_or(LaunchPadOwner::Neutral);

        // Stay near blue or contested pads
        if owner == LaunchPadOwner::Blue || owner == LaunchPadOwner::Contested {
            for &(q, r) in pad_tiles {
                if !blocking_cells.contains(&(q, r)) {
                    targets.push((q, r));
                }
            }
        }
    }

    find_nearest(unit_pos, &targets)
}

fn find_nearest(from: (i32, i32), targets: &[(i32, i32)]) -> Option<(i32, i32)> {
    if targets.is_empty() {
        return None;
    }

    let mut nearest = targets[0];
    let mut min_dist = hex_distance(from, nearest);

    for &target in &targets[1..] {
        let dist = hex_distance(from, target);
        if dist < min_dist {
            min_dist = dist;
            nearest = target;
        }
    }

    Some(nearest)
}

// AI Plugin
pub struct AIPlugin;

impl Plugin for AIPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<AIController>()
            .add_systems(
                Update,
                (ai_spawn_units, ai_command_units).run_if(in_state(LoadingState::Playing)),
            );
    }
}
