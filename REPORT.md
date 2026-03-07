# Analysis of README.md files in the fontc repository

During the analysis of the `README.md` files across the `fontc` repository and its crates, several outdated, incorrect, or incomplete statements were identified when compared to the actual source code.

My investigation was interrupted due to reaching the maximum number of turns, but here is a comprehensive report of the findings based on the analysis performed.

## 1. Dead `#license` Links
The following crates have a broken/dead `#license` anchor link in their `README.md` badges, as there is no corresponding `## License` section in the file:
* `fontc/README.md`
* `fontir/README.md`
* `fontbe/README.md`
* `fontdrasil/README.md`

## 2. `glyphs-reader/README.md`
* **Incomplete functionality description:** The README states "This crate reads Glyphs 2 and Glyphs 3 files." However, `src/lib.rs` clearly states `//! Lightweight library for reading and writing Glyphs font files.` The crate has writing capabilities (e.g., formatting outputs and writing to files), which are not mentioned in the README.

## 3. `otl-normalizer/README.md`
* **Outdated feature support:** The README states "This currently supports a subset of GPOS (kerning and marks)". However, an inspection of the codebase (`src/gdef.rs`) reveals that it also supports normalizing the `GDEF` ligature caret table. The README fails to mention GDEF support. 
* Note: `GSUB` is stubbed out but explicitly noted as not fully supported in the code.

## 4. `ufo2fontir/README.md`
* **Incomplete input format description:** The README states "This crate is a frontend for compilation of .designspace sources. It converts from .designspace to the fontc internal representation." However, the crate's `src/lib.rs` states it converts from `[Unified Font Object][UFO] files`, and the codebase natively supports parsing `.ufo` sources directly without a `.designspace` file. The README should mention UFO support.

## 5. `fontc_crater/README.md`
* **Missing JSON output documentation:** The README focuses heavily on HTML output and the `--html-only` flag. However, the crate (`src/main.rs` and `src/args.rs`) also processes and generates JSON outputs for tracking successful and failed font compilations across targets.

## 6. `ttx_diff/README.md`
* **Inaccurate source path assumptions:** The prompt requested checking `src/lib.rs` for `ttx_diff`, but `ttx_diff` is actually a pure Python package located in `src/ttx_diff/`. The README is generally accurate regarding its Python nature, but could clarify the directory structure for those expecting a Rust crate.

## 7. `fontc/README.md`
* **Grammar/Typo:** Contains the typo "It's primary task" (should be "Its").
* **Outdated Terminology:** Mentions creating a "task graph", but the execution model has been refactored to use a `Workload` and `orchestration` based system (`src/workload.rs`).
* **Incomplete Scope:** States that it "provides the command line entrypoint", but the crate (`src/lib.rs`) also intentionally exposes a programmatic library entrypoint (`pub fn generate_font`) for integrating `fontc` natively as a Rust library.

## 8. `fontir/README.md`
* **Incorrect Dependency Description:** States "It should be referenced by fontc and any frontend (_format_2fontir) crates." This is incorrect/incomplete because the backend crate, `fontbe`, heavily references `fontir` to read the Intermediate Representation and compile it into binary tables.

## 9. `fontbe/README.md`
* Mostly accurate, except for the dead `#license` link mentioned above.

## 10. `fontdrasil/README.md`
* Mostly accurate, except for the dead `#license` link mentioned above.

## 11. `glyphs2fontir/README.md`
* **Incomplete Input Format Description:** The README states it "converts from .glyphs to the fontc internal representation." However, the frontend (via `glyphs-reader` and standard `fontc` inputs like `Input::GlyphsPath`) natively supports parsing `.glyphspackage` directory structures, which is missing from the description.

## 12. `fontra2fontir/README.md`
* Overall accurate. No major discrepancies or broken links were found in this crate.
