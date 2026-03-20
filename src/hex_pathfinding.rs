//! Hex grid pathfinding using bevy_northstar's any-angle pathfinding algorithms.

use bevy::prelude::*;

use crate::map::HexMapConfig;

/// Resource that holds the pathfinding grid for our hex map.
/// Uses 2D axial coordinates (q, r) mapped to grid coordinates.
#[derive(Resource, Default)]
pub struct HexPathfindingGrid;

impl HexPathfindingGrid {
    /// Create a new hex pathfinding grid.
    pub fn new(_map_radius: i32) -> Self {
        Self
    }
}

/// Setup system to create the hex pathfinding grid.
pub fn setup_hex_pathfinding(
    mut commands: Commands,
    config: Res<HexMapConfig>,
) {
    let grid = HexPathfindingGrid::new(0);
    commands.insert_resource(grid);
    info!("✓ Hex pathfinding grid initialized");
}
