use graphics::Context;
use piston_window::{ G2d, PistonWindow };
use specs::prelude::*;

use components::*;
use resources::*;

pub struct MovementSys;

impl<'a> System<'a> for MovementSys {
    type SystemData = (
        Read<'a, DeltaTime>,
        WriteStorage<'a, Position>,
        ReadStorage<'a, Velocity>,);

    fn run(&mut self, (delta, mut pos, vel): Self::SystemData) {
        let dt = delta.0;

        for (pos, vel) in (&mut pos, &vel).join() {
            pos.x += dt * vel.x;
            pos.y += dt * vel.y;
        }
    }
}

pub struct RenderSys {
    pub window: PistonWindow
}

impl<'a> System<'a> for RenderSys {
    type SystemData = (ReadStorage<'a, Position>, );

    fn run(&mut self, data: Self::SystemData) {
        // use piston::event_loop::*;

        if let Some(event) = self.window.next() {
            self.window.draw_2d(&event, |c, g| Self::draw_func(data, c, g));
        }
    }
}

impl RenderSys {
    fn draw_func(
        (pos, ): <RenderSys as System>::SystemData,
        context: Context,
        graphics: &mut G2d) {

        use graphics::*;

        const BACKGROUND_COLOR: [f32; 4] = [0.3, 0.2, 0.3, 1.0];
        const RECTANGLE_COLOR: [f32; 4] = [0.2, 0.9, 1.0, 1.0];

        clear(BACKGROUND_COLOR, graphics);

        for pos in pos.join() {
            let rect = [pos.x, pos.y, 10., 200.] as types::Rectangle<f64>;
            let transform = context.transform;
            rectangle(RECTANGLE_COLOR, rect, transform, graphics);
        }
    }
}

// TODO create EventHandlerSys that accepts input events and flags various
// subsystems to turn on or not...
// seems a bit of a roundabout way to do things though...
// maybe just learn Amethyst instead :D
//
// pub struct IOSys;
//
// impl<'a> System<'a> for IOSys {
//     type SystemData = (Read<'a, DeltaTime>, );
//
//     fn run(&mut self, data: Self::SystemData) {
//         if let Some(event) = self.window.next() {
//             event.
//         }
//     }
// }
//
// pub struct PlayerControlSys;
//
// impl<'a> System<'a> for PlayerControlSys {
//     type SystemData = (ReadStorage<'a, Velocity>, );
// }
//
// pub struct UpdateResourceSys;
//
// impl<'a> System<'a> for UpdateResourceSys {
//     type SystemData = (Read<'a, DeltaTime>, );
// }
