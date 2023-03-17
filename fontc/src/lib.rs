//! A font compiler with aspirations of being fast and safe.

mod args;
mod change_detector;
mod config;
mod error;
pub mod work;

pub use args::Args;
pub use change_detector::ChangeDetector;
pub use config::Config;
pub use error::Error;
