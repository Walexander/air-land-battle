use bevy::prelude::*;
use bevy::audio::{AudioSink, Volume};
use crate::loading::LoadingState;
use crate::launch_pads::GameTimer;
use crate::Paused;

pub struct MusicPlugin;

impl Plugin for MusicPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(MusicState::default())
            .add_systems(OnEnter(LoadingState::Playing), setup_music)
            .add_systems(Update, (
                check_music_switch,
                handle_crossfade,
                handle_music_pause,
                handle_fade_in,
            ).run_if(in_state(LoadingState::Playing)));
    }
}

#[derive(Resource)]
struct MusicState {
    in_danger_zone: bool,
    crossfade_progress: Option<CrossfadeDirection>,
}

enum CrossfadeDirection {
    ToIntense(f32),
    ToCalm(f32),
}

impl Default for MusicState {
    fn default() -> Self {
        Self {
            in_danger_zone: false,
            crossfade_progress: None,
        }
    }
}

#[derive(Component)]
struct MusicTrack {
    track_type: TrackType,
    target_volume: f32,
}

#[derive(Component)]
struct FadingIn {
    elapsed: f32,
    duration: f32,
    target_volume: f32,
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum TrackType {
    Calm,
    Intense,
}

fn setup_music(_commands: Commands, _asset_server: Res<AssetServer>) {
    // Main music disabled
}

fn check_music_switch(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    game_timer: Res<GameTimer>,
    mut music_state: ResMut<MusicState>,
    music_query: Query<&MusicTrack>,
) {
    let in_danger_zone = game_timer.time_remaining < 5.0;

    // Check if we're entering the danger zone
    if in_danger_zone && !music_state.in_danger_zone && music_state.crossfade_progress.is_none() {
        // Check if we already have an intense track
        let has_intense = music_query.iter().any(|t| t.track_type == TrackType::Intense);

        if !has_intense {
            music_state.in_danger_zone = true;
            music_state.crossfade_progress = Some(CrossfadeDirection::ToIntense(0.0));

            println!("🎵 Entering danger zone - switching to intense music");

            // Spawn the intense track at volume 0
            commands.spawn((
                AudioPlayer::<AudioSource>(asset_server.load("sounds/music/track_08.ogg")),
                PlaybackSettings::LOOP.with_volume(Volume::Linear(0.0)),
                MusicTrack {
                    track_type: TrackType::Intense,
                    target_volume: 0.025,
                },
            ));
        }
    }
    // Check if we're exiting the danger zone (game reset or timer increased)
    else if !in_danger_zone && music_state.in_danger_zone && music_state.crossfade_progress.is_none() {
        // Check if we have an intense track
        let has_intense = music_query.iter().any(|t| t.track_type == TrackType::Intense);

        if has_intense {
            music_state.in_danger_zone = false;
            // Main music disabled — just fade out the intense track without crossfading to calm
            music_state.crossfade_progress = Some(CrossfadeDirection::ToCalm(0.0));
        }
    }
}

fn handle_crossfade(
    time: Res<Time>,
    mut music_state: ResMut<MusicState>,
    mut commands: Commands,
    mut music_query: Query<(Entity, &MusicTrack, &mut AudioSink)>,
) {
    if let Some(direction) = &music_state.crossfade_progress {
        // Crossfade duration: 2 seconds
        let crossfade_duration = 2.0;

        let (progress, fade_out_track, fade_in_track) = match direction {
            CrossfadeDirection::ToIntense(p) => (*p, TrackType::Calm, TrackType::Intense),
            CrossfadeDirection::ToCalm(p) => (*p, TrackType::Intense, TrackType::Calm),
        };

        let new_progress = progress + time.delta_secs() / crossfade_duration;

        if new_progress >= 1.0 {
            // Crossfade complete
            music_state.crossfade_progress = None;

            // Remove the faded-out track
            for (entity, track, _) in music_query.iter() {
                if track.track_type == fade_out_track {
                    commands.entity(entity).despawn();
                    println!("🎵 Crossfade complete, removed {:?} track", fade_out_track);
                }
            }
        } else {
            // Update progress
            music_state.crossfade_progress = Some(match direction {
                CrossfadeDirection::ToIntense(_) => CrossfadeDirection::ToIntense(new_progress),
                CrossfadeDirection::ToCalm(_) => CrossfadeDirection::ToCalm(new_progress),
            });

            // Update volumes during crossfade
            for (_, track, mut sink) in music_query.iter_mut() {
                if track.track_type == fade_out_track {
                    // Fade out: volume goes from target_volume to 0
                    let volume = track.target_volume * (1.0 - new_progress);
                    sink.set_volume(Volume::Linear(volume));
                } else if track.track_type == fade_in_track {
                    // Fade in: volume goes from 0 to target_volume
                    let volume = track.target_volume * new_progress;
                    sink.set_volume(Volume::Linear(volume));
                }
            }
        }
    }
}

fn handle_music_pause(
    paused: Res<Paused>,
    music_query: Query<&AudioSink, With<MusicTrack>>,
) {
    if paused.is_changed() {
        for sink in music_query.iter() {
            if paused.0 {
                sink.pause();
            } else {
                sink.play();
            }
        }
    }
}

fn handle_fade_in(
    time: Res<Time>,
    mut commands: Commands,
    mut fade_query: Query<(Entity, &mut FadingIn, &mut AudioSink)>,
) {
    for (entity, mut fade, mut sink) in fade_query.iter_mut() {
        fade.elapsed += time.delta_secs();

        if fade.elapsed >= fade.duration {
            // Fade complete, set to target volume and remove component
            sink.set_volume(Volume::Linear(fade.target_volume));
            commands.entity(entity).remove::<FadingIn>();
            println!("🎵 Music fade-in complete");
        } else {
            // Calculate current volume based on progress
            let progress = fade.elapsed / fade.duration;
            let current_volume = fade.target_volume * progress;
            sink.set_volume(Volume::Linear(current_volume));
        }
    }
}
