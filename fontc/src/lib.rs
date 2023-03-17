//! A font compiler with aspirations of being fast and safe.

mod args;
mod config;
mod error;
pub mod work;

pub use args::Args;
pub use config::Config;
pub use error::Error;
