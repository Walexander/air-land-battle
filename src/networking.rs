use bevy::prelude::*;
use bevy_matchbox::prelude::*;
use serde::{Deserialize, Serialize};

use crate::hex_pathfinding::HexPathfindingGrid;
use crate::loading::LoadingState;
use crate::map::{HexMapConfig, Obstacles};
use crate::units::{
    find_path_waypoints, Army, LocalPlayerArmy, Occupancy, Unit, UnitClass, UnitMovement,
    UnitSpawnQueue, UnitSpawnRequest, UnitStats,
};

// ---------------------------------------------------------------------------
// Resources
// ---------------------------------------------------------------------------

/// Move commands that arrived before the target unit was spawned.
/// Retried every frame until the unit exists.
#[derive(Resource, Default)]
pub struct PendingMoveQueue {
    pub items: Vec<(u32, i32, i32)>, // (stable_id, target_q, target_r)
}

/// Whether we're in singleplayer or multiplayer (host or client).
#[derive(Resource, Default, Clone, Copy, PartialEq, Eq, Debug)]
pub enum MultiplayerMode {
    #[default]
    Singleplayer,
    Host,
    Client,
}

/// Run condition: system should only run when NOT the multiplayer client.
pub fn is_not_client(mode: Res<MultiplayerMode>) -> bool {
    *mode != MultiplayerMode::Client
}

// ---------------------------------------------------------------------------
// Components for cross-machine unit identity
// ---------------------------------------------------------------------------

/// Stable monotonic ID assigned to every unit at spawn time.
/// Used to identify units across processes where Bevy Entity IDs differ.
#[derive(Component, Clone, Copy)]
pub struct StableId(pub u32);

static NEXT_STABLE_ID: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

pub fn next_stable_id() -> u32 {
    NEXT_STABLE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

/// Called once when this process is confirmed to be the multiplayer client.
/// Seeds the stable-ID counter into the upper half of u32 so client-allocated
/// tentative IDs can never collide with host-allocated IDs (which start at 0).
pub fn init_client_id_space() {
    NEXT_STABLE_ID.store(0x8000_0000, std::sync::atomic::Ordering::Relaxed);
}

// ---------------------------------------------------------------------------
// State snapshot data (host → client every 200 ms)
// ---------------------------------------------------------------------------

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct UnitNetState {
    pub stable_id: u32,
    pub army: Army,
    pub unit_class: UnitClass,
    pub q: i32,
    pub r: i32,
    pub health: f32,
    pub max_health: f32,
    /// Destination hex if the unit is currently moving, otherwise None.
    pub move_target: Option<(i32, i32)>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct GameStateSnapshot {
    pub units: Vec<UnitNetState>,
    pub red_money: f32,
    pub blue_money: f32,
    pub timer_remaining: f32,
    pub timer_active: bool,
    pub timer_winning_army: Option<Army>,
    /// LaunchPadOwner serialised as u8: 0=Neutral 1=Red 2=Blue 3=Contested
    pub pad_owners: Vec<u8>,
    pub game_over: bool,
    pub winner: Option<Army>,
}

// ---------------------------------------------------------------------------
// Network message types
// ---------------------------------------------------------------------------

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum NetworkMessage {
    // Lobby handshake
    Hello,
    LobbyAssignment { peer_army: Army, map_path: String },
    LobbyReady,

    // Game-start handshake
    ReadyToPlay,
    GameStart,

    // Game-end (host → client as backup; game_over also travels in the snapshot)
    GameOver { winner: Army },

    // Client → host inputs
    /// Client asks host to spawn a unit for them.
    /// `tentative_stable_id` is pre-allocated by the client so both sides use
    /// the same ID and the client can spawn optimistically without waiting.
    InputSpawnUnit { unit_class: UnitClass, tentative_stable_id: u32 },
    /// Either player commanding a unit to move (client→host or host→client for smooth UX).
    MoveUnit { stable_id: u32, target_q: i32, target_r: i32 },

    // Host → client authoritative state
    /// Sent immediately when any unit spawns so the client doesn't have to wait
    /// for the next 200 ms snapshot before the unit appears.
    SpawnedUnit { stable_id: u32, unit_class: UnitClass, army: Army, q: i32, r: i32 },
    StateSnapshot(Box<GameStateSnapshot>),
}

// ---------------------------------------------------------------------------
// Messages (Bevy 0.18 event queue system)
// ---------------------------------------------------------------------------

/// Write this message to send a payload to the connected peer.
#[derive(Message, Clone)]
pub struct BroadcastNetMsg(pub NetworkMessage);

/// Fired when a lobby-phase network event arrives. Consumed by `lobby.rs`.
#[derive(Message)]
pub enum LobbyNetEvent {
    PeerConnected(PeerId),
    Message(NetworkMessage),
}

/// Fired for game-lifecycle events consumed outside networking.rs.
#[derive(Message)]
pub enum GameNetEvent {
    StartGame,
    GameOver { winner: Army },
    Snapshot(Box<GameStateSnapshot>),
}

// ---------------------------------------------------------------------------
// Systems
// ---------------------------------------------------------------------------

fn poll_socket(
    mut socket: Option<ResMut<MatchboxSocket>>,
    mut spawn_queue: Option<ResMut<UnitSpawnQueue>>,
    mut commands: Commands,
    unit_query: Query<(Entity, &StableId, &Unit, &UnitStats)>,
    player_army: Option<Res<LocalPlayerArmy>>,
    map_config: Option<Res<HexMapConfig>>,
    obstacles: Option<Res<Obstacles>>,
    occupancy: Option<Res<Occupancy>>,
    hex_grid: Option<Res<HexPathfindingGrid>>,
    mut pending_moves: Option<ResMut<PendingMoveQueue>>,
    mut lobby_events: MessageWriter<LobbyNetEvent>,
    mut game_events: MessageWriter<GameNetEvent>,
    mut net_broadcast: MessageWriter<BroadcastNetMsg>,
    net_mode: Res<MultiplayerMode>,
    state: Res<State<LoadingState>>,
) {
    let Some(ref mut socket) = socket else { return };

    let peer_changes = socket.update_peers();
    for (peer, peer_state) in peer_changes {
        match peer_state {
            PeerState::Connected => {
                info!("Peer connected: {:?}", peer);
                lobby_events.write(LobbyNetEvent::PeerConnected(peer));
            }
            PeerState::Disconnected => {
                info!("Peer disconnected: {:?}", peer);
            }
        }
    }

    let messages = socket.channel_mut(0).receive();
    for (_peer, packet) in messages {
        let Ok(msg) = bincode::deserialize::<NetworkMessage>(&*packet) else {
            warn!("Failed to deserialize incoming network message");
            continue;
        };

        match &msg {
            // Lobby
            NetworkMessage::Hello
            | NetworkMessage::LobbyAssignment { .. }
            | NetworkMessage::LobbyReady => {
                lobby_events.write(LobbyNetEvent::Message(msg));
            }

            // Game-start handshake
            NetworkMessage::ReadyToPlay => {
                if *net_mode == MultiplayerMode::Host {
                    info!("Peer is ready — broadcasting GameStart");
                    net_broadcast.write(BroadcastNetMsg(NetworkMessage::GameStart));
                    game_events.write(GameNetEvent::StartGame);
                }
            }
            NetworkMessage::GameStart => {
                game_events.write(GameNetEvent::StartGame);
            }
            NetworkMessage::GameOver { winner } => {
                game_events.write(GameNetEvent::GameOver { winner: *winner });
            }

            // Client → host: spawn request
            NetworkMessage::InputSpawnUnit { unit_class, tentative_stable_id } => {
                if *net_mode == MultiplayerMode::Host
                    && *state.get() == LoadingState::Playing
                {
                    let (Some(ref player_army), Some(ref mut spawn_queue)) =
                        (player_army.as_ref(), spawn_queue.as_mut())
                    else {
                        continue;
                    };
                    let client_army = player_army.0.opponent();
                    spawn_queue.requests.push(UnitSpawnRequest {
                        unit_class: *unit_class,
                        army: client_army,
                        spawn_pos: None,
                        skip_validation: false,
                        stable_id: Some(*tentative_stable_id),
                    });
                }
            }

            // Movement (either direction — MoveUnit is applied to the peer's unit)
            NetworkMessage::MoveUnit { stable_id, target_q, target_r } => {
                if *state.get() == LoadingState::Playing {
                    let (
                        Some(ref map_config),
                        Some(ref obstacles),
                        Some(ref occupancy),
                        Some(ref hex_grid),
                        Some(ref mut pending),
                    ) = (
                        map_config.as_ref(),
                        obstacles.as_ref(),
                        occupancy.as_ref(),
                        hex_grid.as_ref(),
                        pending_moves.as_mut(),
                    ) else {
                        continue;
                    };
                    let found = apply_move_unit(
                        *stable_id, *target_q, *target_r,
                        &unit_query, &mut commands, map_config, obstacles, occupancy, hex_grid,
                    );
                    if !found {
                        pending.items.push((*stable_id, *target_q, *target_r));
                    }
                }
            }

            // Host → client: unit spawned (immediate, don't wait for snapshot)
            NetworkMessage::SpawnedUnit { stable_id, unit_class, army, q, r } => {
                if *net_mode == MultiplayerMode::Client {
                    if let Some(ref mut sq) = spawn_queue {
                        let already_exists = unit_query.iter().any(|(_, sid, _, _)| sid.0 == *stable_id);
                        let already_queued = sq.requests.iter().any(|req| req.stable_id == Some(*stable_id));
                        if !already_exists && !already_queued {
                            sq.requests.push(UnitSpawnRequest {
                                unit_class: *unit_class,
                                army: *army,
                                spawn_pos: Some((*q, *r)),
                                skip_validation: true,
                                stable_id: Some(*stable_id),
                            });
                        }
                    }
                }
            }

            // Host → client state snapshot
            NetworkMessage::StateSnapshot(snapshot) => {
                if *net_mode == MultiplayerMode::Client {
                    game_events.write(GameNetEvent::Snapshot(snapshot.clone()));
                }
            }
        }
    }
}

fn send_messages(
    mut socket: Option<ResMut<MatchboxSocket>>,
    mut reader: MessageReader<BroadcastNetMsg>,
) {
    let Some(ref mut socket) = socket else {
        reader.clear();
        return;
    };

    let peers: Vec<PeerId> = socket.connected_peers().collect();
    if peers.is_empty() {
        reader.clear();
        return;
    }

    for msg in reader.read() {
        let Ok(bytes) = bincode::serialize(&msg.0) else {
            warn!("Failed to serialize outgoing network message");
            continue;
        };
        let packet = bytes.into_boxed_slice();
        for &peer in &peers {
            socket.channel_mut(0).send(packet.clone(), peer);
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers / retry
// ---------------------------------------------------------------------------

/// Try to apply a MoveUnit command. Returns true if the unit was found and the
/// movement was (or could be) applied; false if the unit doesn't exist yet.
fn apply_move_unit(
    stable_id: u32,
    target_q: i32,
    target_r: i32,
    unit_query: &Query<(Entity, &StableId, &Unit, &UnitStats)>,
    commands: &mut Commands,
    map_config: &HexMapConfig,
    obstacles: &Obstacles,
    occupancy: &Occupancy,
    hex_grid: &HexPathfindingGrid,
) -> bool {
    let Some((entity, _, unit, stats)) = unit_query
        .iter()
        .find(|(_, sid, _, _)| sid.0 == stable_id)
    else {
        return false;
    };
    let start = (unit.q, unit.r);
    let goal = (target_q, target_r);
    let blocking: std::collections::HashSet<(i32, i32)> = occupancy
        .positions
        .iter()
        .filter(|&&pos| pos != start && pos != goal)
        .chain(obstacles.positions.iter())
        .copied()
        .collect();
    if let Some(waypoints) =
        find_path_waypoints(start, goal, &map_config.valid_cells, &blocking, hex_grid)
    {
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
    true
}

/// Retry move commands that arrived before the target unit was spawned.
fn apply_pending_moves(
    mut pending: ResMut<PendingMoveQueue>,
    unit_query: Query<(Entity, &StableId, &Unit, &UnitStats)>,
    mut commands: Commands,
    map_config: Option<Res<HexMapConfig>>,
    obstacles: Option<Res<Obstacles>>,
    occupancy: Option<Res<Occupancy>>,
    hex_grid: Option<Res<HexPathfindingGrid>>,
) {
    if pending.items.is_empty() {
        return;
    }
    let (Some(map_config), Some(obstacles), Some(occupancy), Some(hex_grid)) =
        (map_config.as_ref(), obstacles.as_ref(), occupancy.as_ref(), hex_grid.as_ref())
    else {
        return;
    };
    pending.items.retain(|&(stable_id, target_q, target_r)| {
        !apply_move_unit(
            stable_id, target_q, target_r,
            &unit_query, &mut commands, map_config, obstacles, occupancy, hex_grid,
        )
    });
}

// ---------------------------------------------------------------------------
// Plugin
// ---------------------------------------------------------------------------

pub struct NetworkingPlugin;

impl Plugin for NetworkingPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(MultiplayerMode::default())
            .init_resource::<PendingMoveQueue>()
            .add_message::<BroadcastNetMsg>()
            .add_message::<LobbyNetEvent>()
            .add_message::<GameNetEvent>()
            .add_systems(
                Update,
                (poll_socket, send_messages)
                    .run_if(not(in_state(LoadingState::TitleScreen))),
            )
            .add_systems(
                Update,
                apply_pending_moves.run_if(in_state(LoadingState::Playing)),
            );
    }
}
