extern crate glutin_window;
extern crate graphics;
extern crate opengl_graphics;
extern crate piston;
extern crate piston_window;
extern crate specs;

mod components;
mod resources;
mod systems;

use std::time::Instant;

use piston_window::WindowSettings;
use specs::prelude::*;

use components::*;
use resources::*;
use systems::*;

fn main() {
    let mut window = WindowSettings::new("piston-test", [1024, 768])
        .exit_on_esc(true)
        .build()
        .unwrap();

    let mut world = World::new();

    world.register::<Position>();
    world.register::<Velocity>();
    world.register::<Sprite>();
    world.add_resource(DeltaTime(0.0));

    world.create_entity()
        .with(Position { x: 0.0, y: 0.0 } )
        .with(Velocity { x: 10.0, y: 0.5 } )
        .with(Sprite { })
        .build();

    let mut dispatcher = DispatcherBuilder::new()
        .with(MovementSys, "movementsys", &[])
        .with_thread_local(RenderSys { window: window })
        .build();

    dispatcher.setup(&mut world.res);

    let mut prev_time = Instant::now();
    loop {
        let curr_time = Instant::now();
        let duration = curr_time.duration_since(prev_time);
        let delta = DeltaTime(
            duration.as_secs() as f64 +
            duration.subsec_nanos() as f64 * 1e-9);
        world.add_resource(delta);
        prev_time = curr_time;

        dispatcher.dispatch(&mut world.res);

        // TODO handle ECS I/O (e.g. when has game exited? delta time?)
    }
}
