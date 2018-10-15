use specs::prelude::*;

pub struct Position {
    pub x: f64,
    pub y: f64,
}

pub struct Velocity {
    pub x: f64,
    pub y: f64,
}

pub struct Sprite {

}

impl Component for Position {
    type Storage = VecStorage<Self>;
}

impl Component for Velocity {
    type Storage = VecStorage<Self>;
}

impl Component for Sprite {
    type Storage = VecStorage<Self>;
}
