#import bevy_pbr::forward_io::VertexOutput

@group(#{MATERIAL_BIND_GROUP}) @binding(0)
var<uniform> team_color: vec4<f32>;

@group(#{MATERIAL_BIND_GROUP}) @binding(1)
var base_texture: texture_2d<f32>;
@group(#{MATERIAL_BIND_GROUP}) @binding(2)
var base_sampler: sampler;

@group(#{MATERIAL_BIND_GROUP}) @binding(3)
var mask_texture: texture_2d<f32>;
@group(#{MATERIAL_BIND_GROUP}) @binding(4)
var mask_sampler: sampler;

@fragment
fn fragment(mesh: VertexOutput) -> @location(0) vec4<f32> {
    let base = textureSample(base_texture, base_sampler, mesh.uv);
    let mask = textureSample(mask_texture, mask_sampler, mesh.uv).r;

    let color = mix(base.rgb, team_color.rgb, mask);

    return vec4<f32>(color, base.a);
}
