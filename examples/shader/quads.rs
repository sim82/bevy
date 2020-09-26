use bevy::{
    input::mouse::MouseMotion,
    prelude::*,
    render::{
        mesh::shape,
        pipeline::{DynamicBinding, PipelineDescriptor, PipelineSpecialization, RenderPipeline},
        render_graph::{base, AssetRenderResourcesNode, RenderGraph},
        renderer::RenderResources,
        shader::{ShaderStage, ShaderStages},
    },
};
mod crystal;
use crystal::map::Vec3i;
use rand::{thread_rng, Rng};
/// This example illustrates how to create a custom material asset and a shader that uses that material
fn main() {
    App::build()
        .add_default_plugins()
        .add_plugin(FlyCameraPlugin)
        .add_asset::<MyMaterial>()
        .add_startup_system(setup.system())
        // .add_system(blink_system.system())
        .run();
}

#[derive(RenderResources, Default)]
struct MyMaterial {
    pub color: Color,
    pub direction: u32,
    pub translate: Vec3,
}

const VERTEX_SHADER: &str = r#"
#version 450
layout(location = 0) in vec3 Vertex_Position;
layout(set = 0, binding = 0) uniform Camera {
    mat4 ViewProj;
};
layout(set = 1, binding = 0) uniform Transform {
    mat4 Model;
};
layout(set = 1, binding = 1) uniform MyMaterial_color {
    vec4 color;
};
layout(set = 1, binding = 2) uniform MyMaterial_direction {
    uint direction;
};    
layout(set = 1, binding = 3) uniform MyMaterial_translate {
    vec3 translate;
};    
void main() {
    mat4 modelx0 = mat4(0.125, 0.0, 0.0, 0.0, 0.0, 0.125, 0.0, 0.0, 0.0, 0.0, 0.125, 0.0, 0.0, 0.0, 0.125, 1.0);
    mat4 modelx1 = mat4(-0.125, 0.0, 0.0, 0.0, 0.0, 0.125, 0.0, 0.0, 0.0, 0.0, -0.125, 0.0, 0.0, 0.0, -0.125, 1.0);
    mat4 modelx2 = mat4(0.0, 0.0, -0.125, 0.0, 0.0, 0.125, 0.0, 0.0, 0.125, 0.0, 0.0, 0.0, 0.125, 0.0, 0.0, 1.0);
    mat4 modelx3 = mat4(0.0, -0.0, 0.125, 0.0, 0.0, 0.125, 0.0, 0.0, -0.125, 0.0, 0.0, 0.0, -0.125, 0.0, 0.0, 1.0);
    mat4 modelx4 = mat4(-0.125, 0.0, 0.0, 0.0, 0.0, 0.0, 0.125, 0.0, 0.0, 0.125, 0.0, 0.0, 0.0, 0.125, 0.0, 1.0);
    mat4 modelx5 = mat4(-0.125, -0.0, 0.0, 0.0, 0.0, 0.0, -0.125, 0.0, 0.0, -0.125, 0.0, 0.0, 0.0, -0.125, 0.0, 1.0);
    mat4 modelx[6] = mat4[6](modelx0, modelx1, modelx2, modelx3, modelx4, modelx5);
    mat4 trans_mat = mat4(1.0);
    trans_mat[3] = vec4(translate, 1.0);
    gl_Position = ViewProj * Model * trans_mat * modelx[direction] * vec4(Vertex_Position, 1.0);
}
"#;

const FRAGMENT_SHADER: &str = r#"
#version 450
layout(location = 0) out vec4 o_Target;
layout(set = 1, binding = 1) uniform MyMaterial_color {
    vec4 color;
};
void main() {
    o_Target = color;
}
"#;

fn setup(
    mut commands: Commands,
    mut pipelines: ResMut<Assets<PipelineDescriptor>>,
    mut shaders: ResMut<Assets<Shader>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<MyMaterial>>,
    mut render_graph: ResMut<RenderGraph>,
) {
    // Create a new shader pipeline
    let pipeline_handle = pipelines.add(PipelineDescriptor::default_config(ShaderStages {
        vertex: shaders.add(Shader::from_glsl(ShaderStage::Vertex, VERTEX_SHADER)),
        fragment: Some(shaders.add(Shader::from_glsl(ShaderStage::Fragment, FRAGMENT_SHADER))),
    }));

    // Add an AssetRenderResourcesNode to our Render Graph. This will bind MyMaterial resources to our shader
    render_graph.add_system_node(
        "my_material",
        AssetRenderResourcesNode::<MyMaterial>::new(true),
    );

    // Add a Render Graph edge connecting our new "my_material" node to the main pass node. This ensures "my_material" runs before the main pass
    render_graph
        .add_node_edge("my_material", base::node::MAIN_PASS)
        .unwrap();

    // Create a new material
    let material0 = materials.add(MyMaterial {
        color: Color::rgb(1.0, 0.0, 0.0),
        direction: 0,
        translate: Vec3::zero(),
    });

    let material1 = materials.add(MyMaterial {
        color: Color::rgb(0.0, 1.0, 0.0),
        direction: 2,
        translate: Vec3::zero(),
    });
    let material2 = materials.add(MyMaterial {
        color: Color::rgb(0.0, 0.0, 1.0),
        direction: 4,
        translate: Vec3::zero(),
    });

    let material3 = materials.add(MyMaterial {
        color: Color::rgb(1.0, 1.0, 0.0),
        direction: 1,
        translate: Vec3::zero(),
    });

    let material4 = materials.add(MyMaterial {
        color: Color::rgb(0.0, 1.0, 1.0),
        direction: 3,
        translate: Vec3::zero(),
    });
    let material5 = materials.add(MyMaterial {
        color: Color::rgb(1.0, 0.0, 1.0),
        direction: 5,
        translate: Vec3::zero(),
    });

    let pipelines = RenderPipelines::from_pipelines(vec![RenderPipeline::specialized(
        pipeline_handle,
        // NOTE: in the future you wont need to manually declare dynamic bindings
        PipelineSpecialization {
            dynamic_bindings: vec![
                // Transform
                DynamicBinding {
                    bind_group: 1,
                    binding: 0,
                },
                // MyMaterial_color
                DynamicBinding {
                    bind_group: 1,
                    binding: 1,
                },
                // MyMaterial_direction
                DynamicBinding {
                    bind_group: 1,
                    binding: 2,
                },
                // MyMaterial_translate
                DynamicBinding {
                    bind_group: 1,
                    binding: 3,
                },
            ],
            ..Default::default()
        },
    )]);

    let shape = shape::Quad {
        size: Vec2::new(2.0, 2.0),
        flip: false,
    };
    // Setup our world
    commands
        // cube
        .spawn(MeshComponents {
            mesh: meshes.add(Mesh::from(shape.clone())),
            render_pipelines: pipelines.clone(),
            transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
            ..Default::default()
        })
        .with(material0)
        .spawn(MeshComponents {
            mesh: meshes.add(Mesh::from(shape.clone())),
            render_pipelines: pipelines.clone(),
            transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
            ..Default::default()
        })
        .with(material1)
        .spawn(MeshComponents {
            mesh: meshes.add(Mesh::from(shape.clone())),
            render_pipelines: pipelines.clone(),
            transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
            ..Default::default()
        })
        .with(material2)
        .spawn(MeshComponents {
            mesh: meshes.add(Mesh::from(shape.clone())),
            render_pipelines: pipelines.clone(),
            transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
            ..Default::default()
        })
        .with(material3)
        .spawn(MeshComponents {
            mesh: meshes.add(Mesh::from(shape.clone())),
            render_pipelines: pipelines.clone(),
            transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
            ..Default::default()
        })
        .with(material4)
        .spawn(MeshComponents {
            mesh: meshes.add(Mesh::from(shape.clone())),
            render_pipelines: pipelines.clone(),
            transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
            ..Default::default()
        })
        .with(material5)
        // camera
        .spawn(Camera3dComponents {
            transform: Transform::new(Mat4::face_toward(
                Vec3::new(5.0, 5.0, 20.0),
                Vec3::new(5.0, 5.0, 0.0),
                Vec3::new(0.0, 1.0, 0.0),
            )),
            ..Default::default()
        })
        .with(FlyCamera::default());

    let bm = crystal::read_map("assets/maps/hidden_ramp.txt").expect("could not read file");
    let mut planes = crystal::PlanesSep::new();
    planes.create_planes(&bm);

    for (i, p) in planes.planes_iter().cloned().enumerate() {
        let point = &p.cell;
        let dir = match p.dir {
            crystal::Dir::ZxPos => 4,
            crystal::Dir::ZxNeg => 5,
            crystal::Dir::YzPos => 2,
            crystal::Dir::YzNeg => 3,
            crystal::Dir::XyPos => 0,
            crystal::Dir::XyNeg => 1,
        };
        println!("spawn");
        commands
            .spawn(MeshComponents {
                mesh: meshes.add(Mesh::from(shape::Quad {
                    size: Vec2::new(2.0, 2.0),
                    flip: false,
                })),
                render_pipelines: pipelines.clone(),
                // transform: Transform::from_translation(point.into_vec3() * 0.25),
                ..Default::default()
            })
            // .with(match dir {
            //     0 => material0,
            //     2 => material1,
            //     4 => material2,
            //     1 => material3,
            //     3 => material4,
            //     5 => material5,
            //     _ => panic!("unhandled dir"),
            // })
            .with(materials.add(MyMaterial {
                color: match p.dir {
                    crystal::Dir::ZxPos => Color::rgb(0.0, 0.0, 1.0),
                    crystal::Dir::ZxNeg => Color::rgb(0.0, 1.0, 1.0),
                    crystal::Dir::YzPos => Color::rgb(0.0, 1.0, 0.0),
                    crystal::Dir::YzNeg => Color::rgb(0.0, 1.0, 1.0),
                    crystal::Dir::XyPos => Color::rgb(1.0, 0.0, 0.0),
                    crystal::Dir::XyNeg => Color::rgb(1.0, 1.0, 0.0),
                },
                direction: dir,
                translate: point.into_vec3() * 0.25,
            }));
    }
}

fn blink_system(
    mut materials: ResMut<Assets<MyMaterial>>,
    material_handle: Mut<Handle<MyMaterial>>,
) {
    // println!("blink");
    let material = materials.get_mut(&material_handle).unwrap();
    material.color.r = thread_rng().gen::<f32>();
    material.color.g = thread_rng().gen::<f32>();
    material.color.b = thread_rng().gen::<f32>();
    // println!("update {:?}", material.color);
}

pub struct FlyCamera {
    /// The speed the FlyCamera moves at. Defaults to `1.0`
    pub speed: f32,
    /// The maximum speed the FlyCamera can move at. Defaults to `0.5`
    pub max_speed: f32,
    /// The sensitivity of the FlyCamera's motion based on mouse movement. Defaults to `3.0`
    pub sensitivity: f32,
    /// The amount of deceleration to apply to the camera's motion. Defaults to `1.0`
    pub friction: f32,
    /// The current pitch of the FlyCamera in degrees. This value is always up-to-date, enforced by [FlyCameraPlugin](struct.FlyCameraPlugin.html)
    pub pitch: f32,
    /// The current pitch of the FlyCamera in degrees. This value is always up-to-date, enforced by [FlyCameraPlugin](struct.FlyCameraPlugin.html)
    pub yaw: f32,
    /// The current velocity of the FlyCamera. This value is always up-to-date, enforced by [FlyCameraPlugin](struct.FlyCameraPlugin.html)
    pub velocity: Vec3,
    /// Key used to move forward. Defaults to `W`
    pub key_forward: KeyCode,
    /// Key used to move backward. Defaults to `S
    pub key_backward: KeyCode,
    /// Key used to move left. Defaults to `A`
    pub key_left: KeyCode,
    /// Key used to move right. Defaults to `D`
    pub key_right: KeyCode,
    /// Key used to move up. Defaults to `Space`
    pub key_up: KeyCode,
    /// Key used to move forward. Defaults to `LShift`
    pub key_down: KeyCode,
}
impl Default for FlyCamera {
    fn default() -> Self {
        Self {
            speed: 1.0,
            max_speed: 0.5,
            sensitivity: 3.0,
            friction: 1.0,
            pitch: 0.0,
            yaw: 0.0,
            velocity: Vec3::zero(),
            key_forward: KeyCode::W,
            key_backward: KeyCode::S,
            key_left: KeyCode::A,
            key_right: KeyCode::D,
            key_up: KeyCode::Space,
            key_down: KeyCode::LShift,
        }
    }
}

fn forward_vector(rotation: &Quat) -> Vec3 {
    rotation.mul_vec3(Vec3::unit_z()).normalize()
}

fn forward_walk_vector(rotation: &Quat) -> Vec3 {
    let f = forward_vector(rotation);
    let f_flattened = Vec3::new(f.x(), 0.0, f.z()).normalize();
    f_flattened
}

fn strafe_vector(rotation: &Quat) -> Vec3 {
    // Rotate it 90 degrees to get the strafe direction
    Quat::from_rotation_y(90.0f32.to_radians())
        .mul_vec3(forward_walk_vector(rotation))
        .normalize()
}

fn movement_axis(input: &Res<Input<KeyCode>>, plus: KeyCode, minus: KeyCode) -> f32 {
    let mut axis = 0.0;
    if input.pressed(plus) {
        axis += 1.0;
    }
    if input.pressed(minus) {
        axis -= 1.0;
    }
    axis
}

fn camera_movement_system(
    time: Res<Time>,
    keyboard_input: Res<Input<KeyCode>>,
    mut query: Query<(&mut FlyCamera, &mut Transform)>,
) {
    for (mut options, mut transform) in &mut query.iter() {
        let axis_h = movement_axis(&keyboard_input, options.key_right, options.key_left);
        let axis_v = movement_axis(&keyboard_input, options.key_backward, options.key_forward);

        let axis_float = movement_axis(&keyboard_input, options.key_up, options.key_down);

        let any_button_down = axis_h != 0.0 || axis_v != 0.0 || axis_float != 0.0;

        let rotation = transform.rotation();
        let accel: Vec3 = ((strafe_vector(&rotation) * axis_h)
            + (forward_walk_vector(&rotation) * axis_v)
            + (Vec3::unit_y() * axis_float))
            * options.speed;

        let friction: Vec3 = if options.velocity.length() != 0.0 && !any_button_down {
            options.velocity.normalize() * -1.0 * options.friction
        } else {
            Vec3::zero()
        };

        options.velocity += accel * time.delta_seconds;

        // clamp within max speed
        if options.velocity.length() > options.max_speed {
            options.velocity = options.velocity.normalize() * options.max_speed;
        }

        let delta_friction = friction * time.delta_seconds;

        options.velocity = if (options.velocity + delta_friction).sign() != options.velocity.sign()
        {
            Vec3::zero()
        } else {
            options.velocity + delta_friction
        };
        transform.translate(options.velocity);
        // *translation += options.velocity;
        // println!("cms: {:?} {:?}", *transform, options.velocity);
    }
}

#[derive(Default)]
struct State {
    mouse_motion_event_reader: EventReader<MouseMotion>,
}

fn mouse_motion_system(
    time: Res<Time>,
    mut state: ResMut<State>,
    mouse_motion_events: Res<Events<MouseMotion>>,
    mouse_button_input: Res<Input<MouseButton>>,
    mut query: Query<(&mut FlyCamera, &mut Transform)>,
) {
    if !mouse_button_input.pressed(MouseButton::Left) {
        return;
    }
    let mut delta: Vec2 = Vec2::zero();
    for event in state.mouse_motion_event_reader.iter(&mouse_motion_events) {
        delta += event.delta;
    }
    if delta == Vec2::zero() {
        return;
    }

    for (mut options, mut transform) in &mut query.iter() {
        options.yaw -= delta.x() * options.sensitivity * time.delta_seconds;
        options.pitch += delta.y() * options.sensitivity * time.delta_seconds;

        if options.pitch > 89.9 {
            options.pitch = 89.9;
        }
        if options.pitch < -89.9 {
            options.pitch = -89.9;
        }
        println!("pitch: {}, yaw: {}", options.pitch, options.yaw);

        let yaw_radians = options.yaw.to_radians();
        let pitch_radians = options.pitch.to_radians();

        transform.set_rotation(
            Quat::from_axis_angle(Vec3::unit_y(), yaw_radians)
                * Quat::from_axis_angle(-Vec3::unit_x(), pitch_radians),
        );
    }
}

pub struct FlyCameraPlugin;

impl Plugin for FlyCameraPlugin {
    fn build(&self, app: &mut AppBuilder) {
        app.init_resource::<State>()
            .add_system(camera_movement_system.system())
            .add_system(mouse_motion_system.system());
    }
}
