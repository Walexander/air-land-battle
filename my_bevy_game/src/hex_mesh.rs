use std::f32::consts::PI;

use bevy::{
    prelude::*,
    render::{mesh::Indices, pipeline::PrimitiveTopology},
};

fn main() {
    App::build()
        .add_plugins(DefaultPlugins)
        .add_startup_system(setup.system())
        .run();
}

fn setup(
    commands: &mut Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    commands
        .spawn(Camera3dBundle {
            transform: Transform::from_translation(Vec3::new(0., 0., 10.))
                .looking_at(Vec3::zero(), Vec3::unit_y()),
            ..Default::default()
        })
        .spawn(LightBundle {
            transform: Transform::from_translation(Vec3::new(0., 0., 10.)),
            ..Default::default()
        });
    commands.spawn(PbrBundle {
        material: materials.add(Color::rgb_u8(255, 0, 0).into()),
        mesh: meshes.add(hex_mesh()),
        ..Default::default()
    });
}

fn hex_mesh() -> Mesh {
    let center = ([0., 0., 0.], [0., 0., 1.], [0., 0.]);

    let x = |root: f32| (root * 2. * PI / 6.).cos();
    let y = |root: f32| (root * 2. * PI / 6.).sin();

    let spike0 = ([1., 0., 0.], [0., 0., 1.], [0., 0.]);
    let spike1 = ([x(1.), y(1.), 0.], [0., 0., 1.], [0., 0.]);
    let spike2 = ([x(2.), y(2.), 0.], [0., 0., 1.], [0., 0.]);
    let spike3 = ([x(3.), y(3.), 0.], [0., 0., 1.], [0., 0.]);
    let spike4 = ([x(4.), y(4.), 0.], [0., 0., 1.], [0., 0.]);
    let spike5 = ([x(5.), y(5.), 0.], [0., 0., 1.], [0., 0.]);
    let vertices = [center, spike0, spike1, spike2, spike3, spike4, spike5];
    let mut positions = Vec::with_capacity(6);
    let mut normals = Vec::with_capacity(6);
    let mut uvs = Vec::with_capacity(6);

    for (position, normal, uv) in vertices.iter() {
        positions.push(*position);
        normals.push(*normal);
        uvs.push(*uv);
    }

    let indices = Indices::U32(vec![0, 1, 2, 0, 2, 3, 0, 3, 4, 0, 4, 5, 0, 5, 6, 0, 6, 1]);

    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);
    mesh.set_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.set_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.set_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
    mesh.set_indices(Some(indices));
    mesh
}
