use crate::coords::Location;
use serde::{Deserialize, Serialize};
use write_fonts::types::Tag;

#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct LocationSerdeRepr<T>(Vec<(Tag, T)>);

impl<T: Clone> From<LocationSerdeRepr<T>> for Location<T> {
    fn from(src: LocationSerdeRepr<T>) -> Location<T> {
        Location(src.0.into_iter().collect())
    }
}

impl<T: Clone> From<Location<T>> for LocationSerdeRepr<T> {
    fn from(src: Location<T>) -> LocationSerdeRepr<T> {
        LocationSerdeRepr(src.0.into_iter().collect())
    }
}
