use std::error::Error;
use vergen_gitcl::{CargoBuilder, Emitter, GitclBuilder, RustcBuilder};

fn main() -> Result<(), Box<dyn Error>> {
    // Emit the git/cargo/rustc facts that fontc::version and `--vv` read; see
    // fontbe::version and https://github.com/googlefonts/fontc/issues/2048.
    let gitcl = GitclBuilder::default()
        // describe(tags, dirty, match): `git describe --tags --dirty --match
        // fontc-v*`. The match keeps it on fontc releases, not other crates'
        // tags. Returns VERGEN_GIT_DESCRIBE, the canonical version source.
        .describe(true, true, Some("fontc-v*"))
        // `short = false` -> full commit SHA in VERGEN_GIT_SHA, shown by `--vv`.
        .sha(false)
        .build()?;
    Emitter::new()
        .quiet()
        // On a git-less build (e.g. a published crate) the git lookups emit the
        // VERGEN_IDEMPOTENT_OUTPUT sentinel instead of failing; fontbe::version
        // detects it and falls back to the crate version.
        .idempotent()
        .add_instructions(&CargoBuilder::all_cargo()?)? // VERGEN_CARGO_* for `--vv`
        .add_instructions(&gitcl)?
        .add_instructions(&RustcBuilder::all_rustc()?)? // VERGEN_RUSTC_* for `--vv`
        .emit()?;
    Ok(())
}
