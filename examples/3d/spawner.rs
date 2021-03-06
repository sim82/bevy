use bevy::{
    diagnostic::{FrameTimeDiagnosticsPlugin, PrintDiagnosticsPlugin},
    input::mouse::MouseMotion,
    prelude::*,
};

use rand::{rngs::StdRng, Rng, SeedableRng};

/// This example spawns a large number of cubes, each with its own changing position and material
/// This is intended to be a stress test of bevy's ability to render many objects with different properties
/// For the best results, run it in release mode: ```cargo run --example spawner --release
/// NOTE: Bevy still has a number of optimizations to do in this area. Expect the performance here to go way up in the future
fn main() {
    App::build()
        .add_default_plugins()
        .add_plugin(FrameTimeDiagnosticsPlugin::default())
        .add_plugin(PrintDiagnosticsPlugin::default())
        .add_plugin(FlyCameraPlugin)
        .add_startup_system(setup.system())
        .add_system(move_cubes.system())
        .run();
}

fn move_cubes(
    time: Res<Time>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut query: Query<(&mut Transform, &Handle<StandardMaterial>)>,
) {
    for (mut transform, material_handle) in &mut query.iter() {
        let material = materials.get_mut(&material_handle).unwrap();
        transform.translate(Vec3::new(1.0, 0.0, 0.0) * time.delta_seconds);
        material.albedo =
            Color::BLUE * Vec3::splat((3.0 * time.seconds_since_startup as f32).sin());
    }
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    commands
        // light
        .spawn(LightComponents {
            transform: Transform::from_translation(Vec3::new(4.0, -4.0, 5.0)),
            ..Default::default()
        })
        // camera
        .spawn(Camera3dComponents {
            transform: Transform::new(Mat4::face_toward(
                Vec3::new(0.0, 15.0, 150.0),
                Vec3::new(0.0, 0.0, 0.0),
                Vec3::new(0.0, 0.0, 1.0),
            )),
            ..Default::default()
        })
        .with(FlyCamera::default());

    let mut rng = StdRng::from_entropy();
    let cube_handle = meshes.add(Mesh::from(shape::Cube { size: 1.0 }));
    for _ in 0..10000 {
        commands.spawn(PbrComponents {
            mesh: cube_handle,
            material: materials.add(StandardMaterial {
                albedo: Color::rgb(
                    rng.gen_range(0.0, 1.0),
                    rng.gen_range(0.0, 1.0),
                    rng.gen_range(0.0, 1.0),
                ),
                ..Default::default()
            }),
            transform: Transform::from_translation(Vec3::new(
                rng.gen_range(-50.0, 50.0),
                rng.gen_range(-50.0, 50.0),
                0.0,
            )),
            ..Default::default()
        });
    }
}

/// A set of options for initializing a FlyCamera.
/// Attach this component to a [`Camera3dComponents`](https://docs.rs/bevy/0.1.3/bevy/prelude/struct.Camera3dComponents.html) bundle to control it with your mouse and keyboard.
/// # Example
/// ```no_run
/// fn setup(mut commands: Commands) {
///		commands
/// 		.spawn(Camera3dComponents::default())
/// 		.with(FlyCamera::default());
/// }

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
        println!("cms: {:?} {:?}", *transform, options.velocity);
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
    mut query: Query<(&mut FlyCamera, &mut Transform)>,
) {
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
        // println!("pitch: {}, yaw: {}", options.pitch, options.yaw);

        let yaw_radians = options.yaw.to_radians();
        let pitch_radians = options.pitch.to_radians();

        transform.set_rotation(
            Quat::from_axis_angle(Vec3::unit_y(), yaw_radians)
                * Quat::from_axis_angle(-Vec3::unit_x(), pitch_radians),
        );
    }
}

/**
Include this plugin to add the systems for the FlyCamera bundle.
```no_run
fn main() {
    App::build().add_plugin(FlyCameraPlugin);
}
```
**/

pub struct FlyCameraPlugin;

impl Plugin for FlyCameraPlugin {
    fn build(&self, app: &mut AppBuilder) {
        app.init_resource::<State>()
            .add_system(camera_movement_system.system())
            .add_system(mouse_motion_system.system());
    }
}
