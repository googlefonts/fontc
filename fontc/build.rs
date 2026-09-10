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
    // On a git-less build (e.g. a published crate) the git lookups fail and,
    // because `fail_on_error` is off by default, vergen emits the
    // VERGEN_IDEMPOTENT_OUTPUT sentinel instead; fontbe::version detects it and
    // falls back to the crate version.
    //
    // Don't enable `.idempotent()` here: for vergen-gitcl its only effect is to
    // stop emitting `cargo:rerun-if-changed=.git/HEAD` (and the ref file), so
    // the build script would not re-run when HEAD moves and the stamped
    // version would go stale in a warm target dir.
    Emitter::new()
        .quiet()
        .add_instructions(&CargoBuilder::all_cargo()?)? // VERGEN_CARGO_* for `--vv`
        .add_instructions(&gitcl)?
        .add_instructions(&RustcBuilder::all_rustc()?)? // VERGEN_RUSTC_* for `--vv`
        .emit()?;
    // vergen honours VERGEN_GIT_DESCRIBE as an override but doesn't ask cargo to
    // watch it; without this a warm target dir keeps the previous value.
    println!("cargo:rerun-if-env-changed=VERGEN_GIT_DESCRIBE");
    Ok(())
}
