use bevy::prelude::*;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy::asset::RenderAssetUsages;
use std::collections::{HashMap, HashSet};
use std::io::Cursor;
use std::path::Path;

use crate::loading::{LoadingState, MapButton, MAPS};

pub struct MinimapPlugin;

/// Stores one generated minimap `Handle<Image>` per map path.
#[derive(Resource, Default)]
pub struct MinimapImages {
    pub images: HashMap<&'static str, Handle<Image>>,
}

/// Marker for the panel that frames the minimap.
#[derive(Component)]
pub struct MinimapPanel;

/// Marker for the `ImageNode` that displays the minimap.
#[derive(Component)]
pub struct MinimapDisplay;

impl Plugin for MinimapPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<MinimapImages>()
            .add_systems(OnEnter(LoadingState::TitleScreen), generate_minimaps)
            .add_systems(
                Update,
                update_minimap_on_hover.run_if(in_state(LoadingState::TitleScreen)),
            );
    }
}

// ---------------------------------------------------------------------------
// Systems
// ---------------------------------------------------------------------------

fn generate_minimaps(
    mut minimap_images: ResMut<MinimapImages>,
    mut images: ResMut<Assets<Image>>,
) {
    for &(_, path) in MAPS {
        let image = build_minimap(path);
        let handle = images.add(image);
        minimap_images.images.insert(path, handle);
    }
}

fn update_minimap_on_hover(
    interaction_query: Query<(&Interaction, &MapButton), Changed<Interaction>>,
    mut display_query: Query<&mut ImageNode, With<MinimapDisplay>>,
    minimap_images: Res<MinimapImages>,
) {
    for (interaction, map_button) in &interaction_query {
        if matches!(interaction, Interaction::Hovered | Interaction::Pressed) {
            if let Some(handle) = minimap_images.images.get(map_button.0.as_str()) {
                for mut img in &mut display_query {
                    img.image = handle.clone();
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Image generation
// ---------------------------------------------------------------------------

// Pixel dimensions of each hex cell in the minimap image (pointy-top hexagons).
const CELL_W: i32 = 10;
const CELL_H: i32 = 12;
const ROW_STEP: i32 = 9; // 3/4 * CELL_H — vertical distance between hex row centres
const STAGGER: i32 = 5;  // CELL_W / 2 — odd-row horizontal offset
const MARGIN: i32 = 3;


// RGBA color palette
const BG:       [u8; 4] = [12,  15,  25, 255];
const TILE:     [u8; 4] = [85,  95, 112, 255];
const OBSTACLE: [u8; 4] = [52,  46,  36, 255];
const LAUNCHPAD:[u8; 4] = [200, 168, 42,  255];
const SPAWN_R:  [u8; 4] = [200, 58,  58,  255];
const SPAWN_B:  [u8; 4] = [58,  78, 200,  255];
const HQ_R:     [u8; 4] = [240, 28,  28,  255];
const HQ_B:     [u8; 4] = [28,  48, 240,  255];
const CRYSTAL:  [u8; 4] = [48, 200, 200,  255];

fn build_minimap(tmx_path: &str) -> Image {
    let path = Path::new(tmx_path);
    let Ok(content) = std::fs::read_to_string(path) else {
        warn!("minimap: could not read {tmx_path}");
        let (w, h) = (141u32, 72u32);
        let data: Vec<u8> = BG.iter().cloned().cycle().take((w * h * 4) as usize).collect();
        return finish_image(data, w, h);
    };

    // Parse tile CSV → (col, row) cells that have tiles
    let tile_cells = parse_csv_cells(&content);

    if tile_cells.is_empty() {
        let (w, h) = (141u32, 72u32);
        let data: Vec<u8> = BG.iter().cloned().cycle().take((w * h * 4) as usize).collect();
        return finish_image(data, w, h);
    }

    // Row extent (for vertical sizing)
    let min_row = tile_cells.iter().map(|&(_, r)| r).min().unwrap();
    let max_row = tile_cells.iter().map(|&(_, r)| r).max().unwrap();
    let min_col = tile_cells.iter().map(|&(c, _)| c).min().unwrap();

    // Compute each cell's pixel x using the ORIGINAL row parity for correct stagger
    let cell_xs: Vec<i32> = tile_cells.iter()
        .map(|&(col, row)| (col - min_col) * CELL_W + if row % 2 != 0 { STAGGER } else { 0 })
        .collect();

    // Tight horizontal bounds → equal MARGIN on both sides
    let px_min = *cell_xs.iter().min().unwrap();
    let px_max = cell_xs.iter().max().unwrap() + CELL_W;
    let x_base = MARGIN - px_min;

    let n_rows = max_row - min_row + 1;
    let img_w = (px_max - px_min + 2 * MARGIN) as u32;
    let img_h = ((n_rows - 1) * ROW_STEP + CELL_H + 2 * MARGIN) as u32;

    let mut data: Vec<u8> = BG.iter().cloned().cycle().take((img_w * img_h * 4) as usize).collect();

    // Parse object layers → typed axial sets
    let info = parse_objects(tmx_path);

    // Paint each cell
    for (i, &(col, row)) in tile_cells.iter().enumerate() {
        let axial = crate::map_loader::tiled_to_axial(col, row);
        let color = if info.hq_red == Some(axial) {
            HQ_R
        } else if info.hq_blue == Some(axial) {
            HQ_B
        } else if info.obstacles.contains(&axial) {
            OBSTACLE
        } else if info.launch_pads.contains(&axial) {
            LAUNCHPAD
        } else if info.spawn_red.contains(&axial) {
            SPAWN_R
        } else if info.spawn_blue.contains(&axial) {
            SPAWN_B
        } else if info.crystals.contains(&axial) {
            CRYSTAL
        } else {
            TILE
        };
        let bx = x_base + cell_xs[i];
        let by = MARGIN + (row - min_row) * ROW_STEP;
        paint_cell(&mut data, img_w, bx, by, color);
    }

    finish_image(data, img_w, img_h)
}

fn paint_cell(data: &mut [u8], img_w: u32, bx: i32, by: i32, color: [u8; 4]) {
    let hw = CELL_W / 2;
    let hh = CELL_H / 2;
    for dy in 0..CELL_H {
        for dx in 0..CELL_W {
            // Pointy-top hex interior test (pixel centre relative to cell centre)
            let ax = (dx - hw).abs();
            let ay = (dy - hh).abs();
            if ax <= hw && 2 * ay * CELL_W + ax * CELL_H <= CELL_H * CELL_W {
                let x = (bx + dx) as u32;
                let y = (by + dy) as u32;
                if x < img_w {
                    let idx = ((y * img_w + x) * 4) as usize;
                    if idx + 3 < data.len() {
                        data[idx..idx + 4].copy_from_slice(&color);
                    }
                }
            }
        }
    }
}

fn finish_image(data: Vec<u8>, width: u32, height: u32) -> Image {
    let mut img = Image::new(
        Extent3d { width, height, depth_or_array_layers: 1 },
        TextureDimension::D2,
        data,
        TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::RENDER_WORLD,
    );
    img.sampler = bevy::image::ImageSampler::nearest();
    img
}

// ---------------------------------------------------------------------------
// CSV parsing
// ---------------------------------------------------------------------------

fn parse_csv_cells(content: &str) -> Vec<(i32, i32)> {
    let tag = "<data encoding=\"csv\">";
    let Some(start) = content.find(tag) else { return Vec::new(); };
    let after = &content[start + tag.len()..];
    let Some(end) = after.find("</data>") else { return Vec::new(); };
    let csv = &after[..end];

    let rows: Vec<&str> = csv.lines().map(|l| l.trim()).filter(|l| !l.is_empty()).collect();
    let mut cells = Vec::new();
    for (row, line) in rows.iter().enumerate() {
        for (col, cell) in line.split(',').enumerate() {
            let raw: u64 = cell.trim().parse().unwrap_or(0);
            let gid = (raw & 0x1FFF_FFFF) as u32;
            if gid > 0 {
                cells.push((col as i32, row as i32));
            }
        }
    }
    cells
}

// ---------------------------------------------------------------------------
// Object layer parsing
// ---------------------------------------------------------------------------

struct MapInfo {
    obstacles:  HashSet<(i32, i32)>,
    launch_pads: HashSet<(i32, i32)>,
    spawn_red:  HashSet<(i32, i32)>,
    spawn_blue: HashSet<(i32, i32)>,
    hq_red:     Option<(i32, i32)>,
    hq_blue:    Option<(i32, i32)>,
    crystals:   HashSet<(i32, i32)>,
}

fn parse_objects(tmx_path: &str) -> MapInfo {
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
    let Ok(map) = loader.load_tmx_map(Path::new(tmx_path)) else {
        return MapInfo {
            obstacles: HashSet::new(),
            launch_pads: HashSet::new(),
            spawn_red: HashSet::new(),
            spawn_blue: HashSet::new(),
            hq_red: None,
            hq_blue: None,
            crystals: HashSet::new(),
        };
    };

    let mut obstacles:   HashSet<(i32, i32)> = HashSet::new();
    let mut launch_pads: HashSet<(i32, i32)> = HashSet::new();
    let mut spawn_red:   HashSet<(i32, i32)> = HashSet::new();
    let mut spawn_blue:  HashSet<(i32, i32)> = HashSet::new();
    let mut hq_red:   Option<(i32, i32)> = None;
    let mut hq_blue:  Option<(i32, i32)> = None;
    let mut crystals: HashSet<(i32, i32)> = HashSet::new();
    let mut base_red_poly:  Vec<(f32, f32)> = Vec::new();
    let mut base_blue_poly: Vec<(f32, f32)> = Vec::new();
    let mut pad_polys: Vec<Vec<(f32, f32)>> = Vec::new();

    for layer in map.layers() {
        let tiled::LayerType::Objects(obj_layer) = layer.layer_type() else { continue; };
        match layer.name.as_str() {
            "Obstacles" => {
                for obj in obj_layer.objects() {
                    obstacles.insert(crate::map_loader::tile_obj_to_axial(obj.x, obj.y));
                }
            }
            "Bases" => {
                for obj in obj_layer.objects() {
                    match obj.name.as_str() {
                        "Red HQ"   => { hq_red  = Some(crate::map_loader::tile_obj_to_axial(obj.x, obj.y)); }
                        "Blue HQ"  => { hq_blue = Some(crate::map_loader::tile_obj_to_axial(obj.x, obj.y)); }
                        "Red Base" => {
                            if let tiled::ObjectShape::Polygon { points } = &obj.shape {
                                base_red_poly = crate::map_loader::polygon_to_world(obj.x, obj.y, obj.rotation, points);
                            }
                        }
                        "Blue Base" => {
                            if let tiled::ObjectShape::Polygon { points } = &obj.shape {
                                base_blue_poly = crate::map_loader::polygon_to_world(obj.x, obj.y, obj.rotation, points);
                            }
                        }
                        _ => {}
                    }
                }
            }
            "Launch Pads" => {
                for obj in obj_layer.objects() {
                    if let tiled::ObjectShape::Polygon { points } = &obj.shape {
                        pad_polys.push(crate::map_loader::polygon_to_world(obj.x, obj.y, obj.rotation, points));
                    }
                }
            }
            "Spawn Points" => {
                for obj in obj_layer.objects() {
                    let cell = crate::map_loader::tile_obj_to_axial(obj.x, obj.y);
                    match obj.name.as_str() {
                        "Red Spawn"  => { spawn_red.insert(cell); }
                        "Blue Spawn" => { spawn_blue.insert(cell); }
                        _ => {}
                    }
                }
            }
            "Crystals" => {
                for obj in obj_layer.objects() {
                    crystals.insert(crate::map_loader::tile_obj_to_axial(obj.x, obj.y));
                }
            }
            _ => {}
        }
    }

    // Enumerate cells inside base polygons and add as obstacles
    for poly in &[&base_red_poly, &base_blue_poly] {
        if !poly.is_empty() {
            for cell in crate::map_loader::cells_in_polygon(poly) {
                obstacles.insert(cell);
            }
        }
    }
    // Enumerate cells inside launch pad polygons
    for poly in &pad_polys {
        for cell in crate::map_loader::cells_in_polygon(poly) {
            launch_pads.insert(cell);
        }
    }

    MapInfo { obstacles, launch_pads, spawn_red, spawn_blue, hq_red, hq_blue, crystals }
}
