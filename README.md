# fontmake-rs
Where in we pursue oxidizing (context: https://github.com/googlefonts/oxidize) fontmake.

Converts source to IR, and then IR to font binary. Aims to be safe, incremental, and fast.

References

   * Intermediate Representation (IR)
      * [Why IR?](https://github.com/googlefonts/oxidize/blob/main/text/2022-11-14-why-ir.md)
      * [IR notes](https://github.com/googlefonts/oxidize/blob/main/text/2022-11-08-font-compiler-ir.md).
   * Editor perspective [note from Just](https://github.com/googlefonts/oxidize/issues/21)
   * [Units](resources/text/units.md)
      * Fonts have all the best units; distinguishing between them turns out to matter.

## Plan

As of 12/6/2022 we intend to:

* Broadly, go depth first
* Push glyphs & related tables through
* Insert updated glyph building into fontmake (Python)
* Wire up feature compilation using https://github.com/cmyr/fea-rs
* Insert into fontmake
* Then finish fontc and stop using fontmake :)

Fontmake will then have incremental, parallel, recompilation, delivering user benefit at a time when fontc doesn't yet do 100% of the job.

For context see https://github.com/googlefonts/oxidize/blob/main/text/2022-07-25-PROPOSAL-build-glyphs-in-rust.md and the discussion on https://github.com/googlefonts/oxidize/pull/33.

## Sources to play with

Google Fonts has lots, you could try https://github.com/rsheeter/google_fonts_sources to get some.
Once you have them you could try building them:

```shell
cargo run --package fontc -- --source ../google_fonts_sources/sources/ofl/notosanskayahli/sources/NotoSansKayahLi.designspace
```

## Releasing

See https://github.com/googlefonts/fontations#releasing
