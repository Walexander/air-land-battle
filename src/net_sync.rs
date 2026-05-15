/// Host-authoritative state sync.
///
/// The host broadcasts a full `GameStateSnapshot` every `SNAPSHOT_INTERVAL`
/// seconds. The client receives it and reconciles its local world:
///   - unit positions + health overwritten
///   - new units queued for spawning
///   - dead units (absent from snapshot) marked with health=0 so the existing
///     `remove_dead_units` system plays the death animation
///   - economy, game timer, and launch-pad ownership overwritten
///
/// Movement smoothness:
///   - Host: `broadcast_new_movements` detects any newly inserted `UnitMovement`
///     and broadcasts `MoveUnit` to the client. This covers harvester auto-moves,
///     host player commands (which no longer send from selection.rs), and
///     collision repaths.
///   - Client: applies `MoveUnit` immediately for smooth animation; snapshot
///     corrects any positional drift.
use bevy::prelude::*;

use crate::economy::Economy;
use crate::launch_pads::{GameState, GameTimer, LaunchPadOwner, LaunchPadOwnership};
use crate::loading::LoadingState;
use crate::networking::{
    BroadcastNetMsg, GameNetEvent, GameStateSnapshot, MultiplayerMode, NetworkMessage,
    PendingMoveQueue, StableId, UnitNetState,
};
use crate::units::{Health, Unit, UnitClass, UnitMovement,
                   UnitSpawnQueue, UnitSpawnRequest, UnitStats};

const SNAPSHOT_INTERVAL: f32 = 0.2;

// ---------------------------------------------------------------------------
// broadcast_snapshot — host only
// ---------------------------------------------------------------------------

pub fn broadcast_snapshot(
    mut timer: Local<f32>,
    time: Res<Time>,
    net_mode: Res<MultiplayerMode>,
    unit_query: Query<(&Unit, &UnitClass, &StableId, &Health, &UnitStats, Option<&UnitMovement>)>,
    economy: Res<Economy>,
    game_timer: Res<GameTimer>,
    pad_ownership: Res<LaunchPadOwnership>,
    game_state: Res<GameState>,
    mut net_broadcast: MessageWriter<BroadcastNetMsg>,
) {
    if *net_mode != MultiplayerMode::Host {
        return;
    }

    *timer += time.delta_secs();
    if *timer < SNAPSHOT_INTERVAL {
        return;
    }
    *timer = 0.0;

    let units: Vec<UnitNetState> = unit_query
        .iter()
        .map(|(unit, unit_class, sid, health, _stats, maybe_movement)| UnitNetState {
            stable_id: sid.0,
            army: unit.army,
            unit_class: *unit_class,
            q: unit.q,
            r: unit.r,
            health: health.current,
            max_health: health.max,
            move_target: maybe_movement.and_then(|m| {
                m.waypoints.last().map(|&wp| world_pos_to_axial(wp))
            }),
        })
        .collect();

    let pad_owners = pad_ownership
        .owners
        .iter()
        .map(|&o| match o {
            LaunchPadOwner::Neutral => 0u8,
            LaunchPadOwner::Red => 1,
            LaunchPadOwner::Blue => 2,
            LaunchPadOwner::Contested => 3,
        })
        .collect();

    let snapshot = GameStateSnapshot {
        units,
        red_money: economy.red_money as f32,
        blue_money: economy.blue_money as f32,
        timer_remaining: game_timer.time_remaining,
        timer_active: game_timer.is_active,
        timer_winning_army: game_timer.winning_army,
        pad_owners,
        game_over: game_state.game_over,
        winner: game_state.winner,
    };

    net_broadcast.write(BroadcastNetMsg(NetworkMessage::StateSnapshot(Box::new(snapshot))));
}

// ---------------------------------------------------------------------------
// broadcast_new_movements — host only
//
// Whenever the host inserts a UnitMovement on any of its own (host-army) units,
// it broadcasts MoveUnit so the client can animate smoothly. The client's own
// units are skipped here because the client applied the move optimistically and
// already has UnitMovement in place.
// ---------------------------------------------------------------------------

pub fn broadcast_new_movements(
    net_mode: Res<MultiplayerMode>,
    new_movements: Query<(&StableId, &UnitMovement), Added<UnitMovement>>,
    mut net_broadcast: MessageWriter<BroadcastNetMsg>,
) {
    if *net_mode != MultiplayerMode::Host {
        return;
    }

    for (sid, movement) in &new_movements {
        if let Some(&last_wp) = movement.waypoints.last() {
            let (target_q, target_r) = world_pos_to_axial(last_wp);
            net_broadcast.write(BroadcastNetMsg(NetworkMessage::MoveUnit {
                stable_id: sid.0,
                target_q,
                target_r,
            }));
        }
    }
}

fn world_pos_to_axial(pos: Vec3) -> (i32, i32) {
    const HEX_WIDTH: f32 = 128.0;
    const HEX_HEIGHT: f32 = HEX_WIDTH * 0.866_025_4;
    let r = (pos.z / (HEX_WIDTH * 0.75)).round() as i32;
    let q = (pos.x / HEX_HEIGHT - r as f32 * 0.5).round() as i32;
    (q, r)
}

// ---------------------------------------------------------------------------
// apply_snapshot — client only
// ---------------------------------------------------------------------------

pub fn apply_snapshot(
    mut events: MessageReader<GameNetEvent>,
    mut unit_query: Query<(Entity, &mut Unit, &StableId, &mut Health, Option<&UnitMovement>)>,
    mut spawn_queue: ResMut<UnitSpawnQueue>,
    mut economy: ResMut<Economy>,
    mut game_timer: ResMut<GameTimer>,
    mut pad_ownership: ResMut<LaunchPadOwnership>,
    mut game_state: ResMut<GameState>,
    net_mode: Res<MultiplayerMode>,
    mut pending_moves: ResMut<PendingMoveQueue>,
) {
    if *net_mode != MultiplayerMode::Client {
        // Drain events so they don't accumulate on host/singleplayer
        for _ in events.read() {}
        return;
    }

    for event in events.read() {
        let GameNetEvent::Snapshot(snapshot) = event else {
            continue;
        };

        let snapshot_ids: std::collections::HashSet<u32> =
            snapshot.units.iter().map(|u| u.stable_id).collect();

        // Collect existing entities first (avoids borrow conflicts below).
        // Also record each unit's current movement destination so we can skip
        // redundant movement updates and avoid stuttering every snapshot.
        let existing: Vec<(Entity, u32, Option<(i32, i32)>)> = unit_query
            .iter()
            .map(|(e, _, sid, _, maybe_mv)| {
                let dest = maybe_mv.and_then(|m| {
                    m.waypoints.last().map(|&wp| world_pos_to_axial(wp))
                });
                (e, sid.0, dest)
            })
            .collect();
        let existing_ids: std::collections::HashSet<u32> =
            existing.iter().map(|(_, id, _)| *id).collect();

        // Update existing units; mark units absent from snapshot as dead.
        // Also reconcile movement destination if it differs from the snapshot.
        for (entity, stable_id, local_dest) in &existing {
            let Ok((_, mut unit, _, mut health, _)) = unit_query.get_mut(*entity) else {
                continue;
            };
            if let Some(us) = snapshot.units.iter().find(|u| u.stable_id == *stable_id) {
                unit.q = us.q;
                unit.r = us.r;
                health.current = us.health;
                health.max = us.max_health;

                // Reconcile movement: if the host says the unit is heading somewhere
                // different from the local animation, update it.
                if let Some((tq, tr)) = us.move_target {
                    let already_correct = local_dest.map_or(false, |d| d == (tq, tr));
                    let already_pending = pending_moves.items.iter()
                        .any(|&(id, q, r)| id == *stable_id && q == tq && r == tr);
                    if !already_correct && !already_pending {
                        pending_moves.items.push((*stable_id, tq, tr));
                    }
                }
            } else {
                // Unit no longer exists on host — trigger local death animation
                health.current = 0.0;
            }
        }

        // Spawn units the client doesn't know about yet.
        // Also check the pending queue so that back-to-back snapshots don't
        // double-queue the same unit before spawn_unit_from_request flushes.
        let pending_spawn_ids: std::collections::HashSet<u32> =
            spawn_queue.requests.iter().filter_map(|r| r.stable_id).collect();
        for us in &snapshot.units {
            if !existing_ids.contains(&us.stable_id) && !pending_spawn_ids.contains(&us.stable_id) {
                spawn_queue.requests.push(UnitSpawnRequest {
                    unit_class: us.unit_class,
                    army: us.army,
                    spawn_pos: Some((us.q, us.r)),
                    skip_validation: true,
                    stable_id: Some(us.stable_id),
                });
                // Queue the initial movement if the unit is already moving on the host.
                if let Some((tq, tr)) = us.move_target {
                    let already_pending = pending_moves.items.iter()
                        .any(|&(id, q, r)| id == us.stable_id && q == tq && r == tr);
                    if !already_pending {
                        pending_moves.items.push((us.stable_id, tq, tr));
                    }
                }
            }
        }

        // Economy
        economy.red_money = snapshot.red_money as i32;
        economy.blue_money = snapshot.blue_money as i32;

        // Timer
        game_timer.time_remaining = snapshot.timer_remaining;
        game_timer.is_active = snapshot.timer_active;
        game_timer.winning_army = snapshot.timer_winning_army;

        // Launch-pad ownership (for pad colour rendering)
        pad_ownership.owners = snapshot
            .pad_owners
            .iter()
            .map(|&v| match v {
                1 => LaunchPadOwner::Red,
                2 => LaunchPadOwner::Blue,
                3 => LaunchPadOwner::Contested,
                _ => LaunchPadOwner::Neutral,
            })
            .collect();

        // Game state
        if snapshot.game_over && !game_state.game_over {
            game_state.game_over = true;
            game_state.winner = snapshot.winner;
        }
    }
}

// ---------------------------------------------------------------------------
// tick_game_timer_client
//
// Runs every frame on the client to keep the on-screen timer smooth between
// snapshots (otherwise it would update in 200 ms jumps).
// ---------------------------------------------------------------------------

pub fn tick_game_timer_client(
    mut game_timer: ResMut<GameTimer>,
    time: Res<Time>,
    net_mode: Res<MultiplayerMode>,
) {
    if *net_mode != MultiplayerMode::Client {
        return;
    }
    if game_timer.is_active {
        game_timer.time_remaining = (game_timer.time_remaining - time.delta_secs()).max(0.0);
    }
}

// ---------------------------------------------------------------------------
// Plugin
// ---------------------------------------------------------------------------

pub struct NetSyncPlugin;

impl Plugin for NetSyncPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                broadcast_snapshot,
                broadcast_new_movements,
                apply_snapshot,
                tick_game_timer_client,
            )
                .run_if(in_state(LoadingState::Playing)),
        );
    }
}
