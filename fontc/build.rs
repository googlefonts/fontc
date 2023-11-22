use std::error::Error;
use vergen::EmitBuilder;

fn main() -> Result<(), Box<dyn Error>> {
    // Emit the instructions
    EmitBuilder::builder()
        .quiet()
        .idempotent()
        .all_cargo()
        .all_git()
        .all_rustc()
        .emit()?;
    Ok(())
}
