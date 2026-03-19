use bevy::prelude::*;
use std::collections::{HashMap, HashSet};
use std::io::Cursor;
use std::path::Path;

use crate::loading::LoadingState;

// ---------------------------------------------------------------------------
// Tile GID constants (from hexagonAll_sheet, firstgid=1)
// ---------------------------------------------------------------------------

pub const TILE_GRASS: u32 = 51;
pub const TILE_LAUNCH_PAD: u32 = 66;
pub const TILE_CRYSTAL: u32 = 36;

// ---------------------------------------------------------------------------
// Resource
// ---------------------------------------------------------------------------

#[derive(Resource, Default)]
pub struct MapDefinition {
    pub tile_map: HashMap<(i32, i32), u32>, // axial → GID; only active (non-zero) tiles
    pub obstacles: HashSet<(i32, i32)>,     // includes HQ positions
    pub hq_red: Option<(i32, i32)>,
    pub hq_blue: Option<(i32, i32)>,
    pub spawn_red: Vec<(i32, i32)>,
    pub spawn_blue: Vec<(i32, i32)>,
    pub launch_pads: Vec<Vec<(i32, i32)>>,
    pub crystal_fields: Vec<(i32, i32)>,
    pub base_red_polygon: Vec<(f32, f32)>,         // game world (wx, wz) outline vertices
    pub base_blue_polygon: Vec<(f32, f32)>,
    pub launch_pad_polygons: Vec<Vec<(f32, f32)>>, // one per launch-pad object
    pub boundary_polygons: Vec<Vec<(f32, f32)>>,   // filled black non-playable regions
    pub loaded: bool,
}

// ---------------------------------------------------------------------------
// Plugin
// ---------------------------------------------------------------------------

pub struct MapLoaderPlugin;

impl Plugin for MapLoaderPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(MapDefinition::default())
            .add_systems(OnEnter(LoadingState::Loading), load_map_data);
    }
}

// ---------------------------------------------------------------------------
// Synchronous map loading
// ---------------------------------------------------------------------------

fn load_map_data(mut map_def: ResMut<MapDefinition>) {
    let tmx_path = Path::new("assets/maps/Side By Side.tmx");

    // Step 1: parse tile layer directly from the CSV in the raw file.
    match std::fs::read_to_string(tmx_path) {
        Ok(content) => parse_tile_csv(&content, &mut map_def),
        Err(e) => error!("map_loader: failed to read TMX: {e}"),
    }

    // Step 2: parse object layers (Bases, Obstacles) via the tiled crate.
    let reader = |path: &Path| -> Result<Cursor<Vec<u8>>, std::io::Error> {
        if path.extension().and_then(|e| e.to_str()) == Some("tsx") {
            let stub = br#"<?xml version="1.0" encoding="UTF-8"?>
<tileset version="1.8" name="stub" tilewidth="1" tileheight="1" tilecount="0" columns="0">
</tileset>"#;
            Ok(Cursor::new(stub.to_vec()))
        } else {
            std::fs::read(path).map(Cursor::new)
        }
    };

    let mut loader = tiled::Loader::with_reader(reader);
    match loader.load_tmx_map(tmx_path) {
        Ok(map) => parse_object_layers(&map, &mut map_def),
        Err(e) => error!("map_loader: failed to load TMX objects: {e}"),
    }

    // Step 3: remove tiles whose centres fall inside a boundary polygon, then
    // re-derive crystal_fields and launch_pads from the filtered tile_map.
    if !map_def.boundary_polygons.is_empty() {
        let polys = map_def.boundary_polygons.clone(); // avoid split-borrow
        map_def.tile_map.retain(|&(q, r), _| {
            const HEX_WIDTH: f32 = 128.0;
            const HEX_HEIGHT: f32 = HEX_WIDTH * 0.866_025_4;
            let wx = HEX_HEIGHT * (q as f32 + r as f32 * 0.5);
            let wz = HEX_WIDTH * 0.75 * r as f32;
            !polys.iter().any(|poly| point_in_polygon(wx, wz, poly))
        });
        let pad_cells: Vec<(i32, i32)> = map_def.tile_map.iter()
            .filter(|&(_, &g)| g == TILE_LAUNCH_PAD)
            .map(|(&pos, _)| pos)
            .collect();
        map_def.launch_pads = group_connected_cells(&pad_cells);
        map_def.crystal_fields = map_def.tile_map.iter()
            .filter(|&(_, &g)| g == TILE_CRYSTAL)
            .map(|(&pos, _)| pos)
            .collect();
        info!(
            "map_loader: after boundary filter → {} tiles, {} launch pads, {} crystal fields",
            map_def.tile_map.len(), map_def.launch_pads.len(), map_def.crystal_fields.len(),
        );
    }
}

/// Ray-cast point-in-polygon test (game world XZ coordinates).
fn point_in_polygon(wx: f32, wz: f32, poly: &[(f32, f32)]) -> bool {
    let n = poly.len();
    let mut inside = false;
    let mut j = n - 1;
    for i in 0..n {
        let (xi, zi) = poly[i];
        let (xj, zj) = poly[j];
        if ((zi > wz) != (zj > wz)) && (wx < (xj - xi) * (wz - zi) / (zj - zi) + xi) {
            inside = !inside;
        }
        j = i;
    }
    inside
}

// ---------------------------------------------------------------------------
// Step 1: tile layer (CSV)
// ---------------------------------------------------------------------------

/// Parse "Tile Layer 1" CSV data, populate `tile_map`, derive launch pads and
/// crystal fields from GID values.
fn parse_tile_csv(content: &str, def: &mut MapDefinition) {
    // Locate the first <data encoding="csv"> block.
    let tag = "<data encoding=\"csv\">";
    let Some(start) = content.find(tag) else {
        error!("map_loader: no CSV tile layer found");
        return;
    };
    let after = &content[start + tag.len()..];
    let Some(end) = after.find("</data>") else {
        error!("map_loader: unterminated CSV tile layer");
        return;
    };
    let csv = &after[..end];

    // Collect non-empty lines first so the row index matches Tiled's row number exactly.
    let rows: Vec<&str> = csv.lines().map(|l| l.trim()).filter(|l| !l.is_empty()).collect();
    for (row, line) in rows.iter().enumerate() {
        for (col, cell) in line.split(',').enumerate() {
            let cell = cell.trim();
            if cell.is_empty() {
                continue;
            }
            // Parse as u64 first to handle large flip-bit values, then mask.
            let raw: u64 = cell.parse().unwrap_or(0);
            let gid = (raw & 0x1FFF_FFFF) as u32; // strip flip/rotate bits
            if gid > 0 {
                let axial = tiled_to_axial(col as i32, row as i32);
                def.tile_map.insert(axial, gid);
            }
        }
    }

    // Derive crystal fields from GID=36 cells.
    def.crystal_fields = def.tile_map.iter()
        .filter(|&(_, &g)| g == TILE_CRYSTAL)
        .map(|(&pos, _)| pos)
        .collect();

    // Derive launch pads: BFS-group connected GID=66 cells.
    let pad_cells: Vec<(i32, i32)> = def.tile_map.iter()
        .filter(|&(_, &g)| g == TILE_LAUNCH_PAD)
        .map(|(&pos, _)| pos)
        .collect();
    def.launch_pads = group_connected_cells(&pad_cells);

    info!(
        "map_loader: tile layer → {} active tiles, {} launch pads, {} crystal fields",
        def.tile_map.len(),
        def.launch_pads.len(),
        def.crystal_fields.len(),
    );
}

/// BFS connected-component grouping for hex cells (axial neighbours).
fn group_connected_cells(cells: &[(i32, i32)]) -> Vec<Vec<(i32, i32)>> {
    let cell_set: HashSet<(i32, i32)> = cells.iter().cloned().collect();
    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    let mut groups: Vec<Vec<(i32, i32)>> = Vec::new();

    for &cell in cells {
        if visited.contains(&cell) {
            continue;
        }
        let mut group = Vec::new();
        let mut stack = vec![cell];
        while let Some((q, r)) = stack.pop() {
            if !visited.insert((q, r)) {
                continue;
            }
            group.push((q, r));
            for neighbour in [(q+1,r),(q-1,r),(q,r+1),(q,r-1),(q+1,r-1),(q-1,r+1)] {
                if cell_set.contains(&neighbour) && !visited.contains(&neighbour) {
                    stack.push(neighbour);
                }
            }
        }
        groups.push(group);
    }
    groups
}

// ---------------------------------------------------------------------------
// Step 2: object layers
// ---------------------------------------------------------------------------

fn parse_object_layers(map: &tiled::Map, def: &mut MapDefinition) {
    use tiled::LayerType;

    for layer in map.layers() {
        let LayerType::Objects(obj_layer) = layer.layer_type() else {
            continue;
        };
        match layer.name.as_str() {
            "Obstacles" => {
                for obj in obj_layer.objects() {
                    match obj.name.as_str() {
                        "Mountain" | "Silo" => {
                            let pos = tile_obj_to_axial(obj.x, obj.y);
                            def.obstacles.insert(pos);
                        }
                        _ => {}
                    }
                }
            }
            "Bases" => {
                for obj in obj_layer.objects() {
                    match obj.name.as_str() {
                        "Red HQ" => {
                            let pos = tile_obj_to_axial(obj.x, obj.y);
                            def.hq_red = Some(pos);
                            def.obstacles.insert(pos);
                        }
                        "Blue HQ" => {
                            let pos = tile_obj_to_axial(obj.x, obj.y);
                            def.hq_blue = Some(pos);
                            def.obstacles.insert(pos);
                        }
                        "Red Base" => {
                            if let tiled::ObjectShape::Polygon { points } = &obj.shape {
                                def.base_red_polygon = polygon_to_world(obj.x, obj.y, points);
                                info!("map_loader: Red Base polygon {} pts", def.base_red_polygon.len());
                            }
                        }
                        "Blue Base" => {
                            if let tiled::ObjectShape::Polygon { points } = &obj.shape {
                                def.base_blue_polygon = polygon_to_world(obj.x, obj.y, points);
                                info!("map_loader: Blue Base polygon {} pts", def.base_blue_polygon.len());
                            }
                        }
                        _ => {}
                    }
                }
            }
            "Launch Pads" => {
                for obj in obj_layer.objects() {
                    if let tiled::ObjectShape::Polygon { points } = &obj.shape {
                        def.launch_pad_polygons.push(polygon_to_world(obj.x, obj.y, points));
                    }
                }
                info!("map_loader: {} launch pad polygons", def.launch_pad_polygons.len());
            }
            "Spawn Points" => {
                for obj in obj_layer.objects() {
                    match obj.name.as_str() {
                        "Red Spawn" => {
                            def.spawn_red.push(tile_obj_to_axial(obj.x, obj.y));
                        }
                        "Blue Spawn" => {
                            def.spawn_blue.push(tile_obj_to_axial(obj.x, obj.y));
                        }
                        _ => {}
                    }
                }
            }
            "Boundaries" => {
                for obj in obj_layer.objects() {
                    if let tiled::ObjectShape::Polygon { points } = &obj.shape {
                        def.boundary_polygons.push(polygon_to_world(obj.x, obj.y, points));
                    }
                }
                info!("map_loader: {} boundary polygons", def.boundary_polygons.len());
            }
            _ => {}
        }
    }

    def.loaded = true;
    info!(
        "map_loader: objects → {} obstacles, hq_red={:?}, hq_blue={:?}, spawn_red={}, spawn_blue={}",
        def.obstacles.len(),
        def.hq_red,
        def.hq_blue,
        def.spawn_red.len(),
        def.spawn_blue.len(),
    );
}

// ---------------------------------------------------------------------------
// Coordinate helpers
// ---------------------------------------------------------------------------

/// Convert tiled polygon relative points to world XZ coords, stripping any closing duplicate.
/// Tiled always repeats the first vertex as the last — ear-clipping requires unique vertices.
fn polygon_to_world(obj_x: f32, obj_y: f32, points: &[(f32, f32)]) -> Vec<(f32, f32)> {
    let mut pts: Vec<(f32, f32)> = points.iter()
        .map(|&(dx, dy)| tiled_pixel_to_world_xz(obj_x + dx, obj_y + dy))
        .collect();
    if let (Some(&first), Some(&last)) = (pts.first(), pts.last()) {
        if (first.0 - last.0).abs() < 0.01 && (first.1 - last.1).abs() < 0.01 {
            pts.pop();
        }
    }
    pts
}

/// Tiled absolute pixel position → game world (wx, wz) continuously (no rounding).
///
/// Derived from `tiled_to_axial` + `axial_to_world_pos`:
///   wx = (HEX_HEIGHT / 120) * (px − 840)
///   wz = (96 / 105) * (py − 70) − 288
/// where 840 is the Tiled x of the center tile (col=6, row=3 odd), and
/// 288 = 96 * 3 accounts for the r=0 centering.
pub fn tiled_pixel_to_world_xz(px: f32, py: f32) -> (f32, f32) {
    const HEX_HEIGHT: f32 = 128.0 * 0.866_025_4;
    let wx = (HEX_HEIGHT / 120.0) * (px - 840.0);
    let wz = (96.0 / 105.0) * (py - 70.0) - 288.0;
    (wx, wz)
}

/// Tile-object bottom-left pixel → game axial (q, r).
pub fn tile_obj_to_axial(obj_x: f32, obj_y: f32) -> (i32, i32) {
    // Tile objects anchor at bottom-left; shift to centre.
    let center_px = obj_x + 60.0;
    let center_py = obj_y - 70.0;
    let (col, row) = pixel_to_tiled(center_px, center_py);
    tiled_to_axial(col, row)
}

/// Pixel centre → Tiled (col, row).
/// tilewidth=120, hexsidelength=70, staggeraxis=y, staggerindex=odd
/// row_step = (140 - 70) + 70 = 140 - (140-70)/2*... actually (tileheight + hexsidelength) / 2 = 105
fn pixel_to_tiled(center_px: f32, center_py: f32) -> (i32, i32) {
    let row = ((center_py - 70.0) / 105.0).round() as i32;
    let col = if row % 2 == 0 {
        ((center_px - 60.0) / 120.0).round() as i32
    } else {
        ((center_px - 120.0) / 120.0).round() as i32
    };
    (col, row)
}

/// Tiled odd-r offset (col, row) → game axial (q, r).
/// Centers the 13×7 Tiled map: center tile (col=6, row=3) → (q=0, r=0).
pub fn tiled_to_axial(col: i32, row: i32) -> (i32, i32) {
    let r = row - 3;
    let q = col - (row - row.rem_euclid(2)) / 2 - 5;
    (q, r)
}
