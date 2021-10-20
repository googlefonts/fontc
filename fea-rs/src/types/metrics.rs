#[derive(Clone, Debug)]
pub enum Anchor {
    Coord { x: i16, y: i16 },
    Contour { x: i16, y: i16, point: u16 },
    //Device { x: i32, y: i32, x_dev: (), y_dev: (), },
    Null,
    //Named(SmolStr),
}
