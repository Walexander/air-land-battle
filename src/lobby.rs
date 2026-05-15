use bevy::prelude::*;
use bevy_matchbox::prelude::*;
use rand::Rng;

use crate::loading::{LoadingState, SelectedMap, MAPS};
use crate::networking::{BroadcastNetMsg, LobbyNetEvent, MultiplayerMode, NetworkMessage};
use crate::units::{Army, LocalPlayerArmy};

// ---------------------------------------------------------------------------
// Resources
// ---------------------------------------------------------------------------

#[derive(Resource, Default, PartialEq, Eq, Clone, Copy, Debug)]
pub enum LobbyStatus {
    #[default]
    Connecting,
    WaitingForPeer,
    Handshaking,
    Ready,
}

// ---------------------------------------------------------------------------
// Components
// ---------------------------------------------------------------------------

#[derive(Component)]
struct LobbyScreen;

#[derive(Component)]
struct LobbyStatusText;

#[derive(Component)]
struct LobbyCancelButton;

// ---------------------------------------------------------------------------
// Matchbox server URL (switch with --features dev)
// ---------------------------------------------------------------------------

#[cfg(feature = "dev")]
const MATCHBOX_URL: &str = "ws://127.0.0.1:3536/air-land-battle?next=2";

#[cfg(not(feature = "dev"))]
const MATCHBOX_URL: &str = "wss://matchbox.johanhelsing.studio/air-land-battle?next=2";

// ---------------------------------------------------------------------------
// Systems
// ---------------------------------------------------------------------------

fn setup_lobby(mut commands: Commands) {
    commands
        .spawn((
            Node {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                justify_content: JustifyContent::Center,
                align_items: AlignItems::Center,
                flex_direction: FlexDirection::Column,
                row_gap: Val::Px(24.0),
                ..default()
            },
            BackgroundColor(Color::srgb(0.04, 0.04, 0.08)),
            LobbyScreen,
        ))
        .with_children(|parent| {
            parent.spawn((
                Text::new("MULTIPLAYER LOBBY"),
                TextFont { font_size: 48.0, ..default() },
                TextColor(Color::srgb(0.9, 0.85, 0.5)),
            ));

            parent.spawn((
                Text::new("Connecting to server…"),
                TextFont { font_size: 28.0, ..default() },
                TextColor(Color::srgb(0.7, 0.7, 0.7)),
                LobbyStatusText,
            ));

            // Cancel button
            parent
                .spawn((
                    Button,
                    Node {
                        width: Val::Px(200.0),
                        height: Val::Px(48.0),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        border: UiRect::all(Val::Px(2.0)),
                        margin: UiRect::top(Val::Px(16.0)),
                        ..default()
                    },
                    BackgroundColor(Color::srgb(0.2, 0.08, 0.08)),
                    BorderColor::all(Color::srgb(0.6, 0.2, 0.2)),
                    LobbyCancelButton,
                ))
                .with_children(|btn| {
                    btn.spawn((
                        Text::new("Cancel"),
                        TextFont { font_size: 22.0, ..default() },
                        TextColor(Color::srgb(1.0, 0.4, 0.4)),
                    ));
                });
        });
}

/// Open the matchbox WebRTC socket when entering the lobby.
fn open_socket(mut commands: Commands) {
    info!("Opening matchbox socket: {}", MATCHBOX_URL);
    commands.insert_resource(MatchboxSocket::new_reliable(MATCHBOX_URL));
    commands.insert_resource(LobbyStatus::Connecting);
}

/// Advances LobbyStatus from Connecting → WaitingForPeer once we have our own
/// socket ID (i.e. we've successfully connected to the signaling server).
fn check_signaling_connection(
    mut socket: Option<ResMut<MatchboxSocket>>,
    mut status: ResMut<LobbyStatus>,
) {
    if *status != LobbyStatus::Connecting {
        return;
    }
    if let Some(ref mut socket) = socket {
        if socket.id().is_some() {
            info!("Connected to signaling server, waiting for peer…");
            *status = LobbyStatus::WaitingForPeer;
        }
    }
}

fn cleanup_lobby(mut commands: Commands, query: Query<Entity, With<LobbyScreen>>) {
    for entity in &query {
        commands.entity(entity).despawn();
    }
}

/// Update the status text node to reflect `LobbyStatus`.
fn update_status_text(
    status: Res<LobbyStatus>,
    mode: Res<MultiplayerMode>,
    mut text_query: Query<&mut Text, With<LobbyStatusText>>,
) {
    if !status.is_changed() {
        return;
    }
    let msg = match *status {
        LobbyStatus::Connecting => "Connecting to server…".to_string(),
        LobbyStatus::WaitingForPeer => match *mode {
            MultiplayerMode::Host => "Connected! Waiting for opponent…".to_string(),
            _ => "Connected! Waiting for host…".to_string(),
        },
        LobbyStatus::Handshaking => "Opponent found! Starting game…".to_string(),
        LobbyStatus::Ready => "Ready!".to_string(),
    };
    for mut text in &mut text_query {
        text.0 = msg.clone();
    }
}

/// React to lobby-phase network events (peer connected, messages received).
fn handle_lobby_events(
    mut events: MessageReader<LobbyNetEvent>,
    mut status: ResMut<LobbyStatus>,
    mode: Res<MultiplayerMode>,
    mut broadcast: MessageWriter<BroadcastNetMsg>,
    mut commands: Commands,
    mut next_state: ResMut<NextState<LoadingState>>,
    mut player_army: ResMut<LocalPlayerArmy>,
) {
    for event in events.read() {
        match event {
            LobbyNetEvent::PeerConnected(_peer) => {
                *status = LobbyStatus::WaitingForPeer;
                // Both sides send Hello immediately on connection.
                broadcast.write(BroadcastNetMsg(NetworkMessage::Hello));
            }

            LobbyNetEvent::Message(msg) => match msg {
                NetworkMessage::Hello => {
                    *status = LobbyStatus::Handshaking;

                    if *mode == MultiplayerMode::Host {
                        // Host randomly assigns teams and picks a map.
                        let mut rng = rand::thread_rng();
                        let map_idx = rng.gen_range(0..MAPS.len());
                        let (_label, map_path) = MAPS[map_idx];

                        // Randomly assign who gets Red. Peer gets the other army.
                        let peer_army = if rng.gen_bool(0.5) { Army::Red } else { Army::Blue };
                        let local_army = peer_army.opponent();

                        // Apply to local state
                        player_army.0 = local_army;
                        commands.insert_resource(SelectedMap(map_path.to_string()));

                        // Tell peer their assignment
                        broadcast.write(BroadcastNetMsg(NetworkMessage::LobbyAssignment {
                            peer_army,
                            map_path: map_path.to_string(),
                        }));
                    }
                }

                NetworkMessage::LobbyAssignment { peer_army, map_path } => {
                    // We are the client: apply the assignment the host sent us.
                    player_army.0 = *peer_army;
                    commands.insert_resource(SelectedMap(map_path.clone()));

                    // Tell host we're ready, then immediately start loading.
                    broadcast.write(BroadcastNetMsg(NetworkMessage::LobbyReady));
                    *status = LobbyStatus::Ready;
                    next_state.set(LoadingState::Loading);
                }

                NetworkMessage::LobbyReady => {
                    // Host receives this from the client — host starts loading.
                    *status = LobbyStatus::Ready;
                    next_state.set(LoadingState::Loading);
                }

                _ => {}
            },
        }
    }
}

fn handle_cancel_button(
    mut interaction_query: Query<
        (&Interaction, &mut BackgroundColor),
        (Changed<Interaction>, With<LobbyCancelButton>),
    >,
    mut next_state: ResMut<NextState<LoadingState>>,
    mut mode: ResMut<MultiplayerMode>,
    mut commands: Commands,
) {
    for (interaction, mut bg) in &mut interaction_query {
        match interaction {
            Interaction::Pressed => {
                *mode = MultiplayerMode::Singleplayer;
                commands.close_socket();
                next_state.set(LoadingState::TitleScreen);
            }
            Interaction::Hovered => {
                *bg = BackgroundColor(Color::srgb(0.3, 0.1, 0.1));
            }
            Interaction::None => {
                *bg = BackgroundColor(Color::srgb(0.2, 0.08, 0.08));
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Plugin
// ---------------------------------------------------------------------------

pub struct LobbyPlugin;

impl Plugin for LobbyPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<LobbyStatus>()
            .add_systems(OnEnter(LoadingState::Lobby), (setup_lobby, open_socket))
            .add_systems(OnExit(LoadingState::Lobby), cleanup_lobby)
            .add_systems(
                Update,
                (
                    check_signaling_connection,
                    update_status_text,
                    handle_lobby_events,
                    handle_cancel_button,
                )
                    .run_if(in_state(LoadingState::Lobby)),
            );
    }
}
