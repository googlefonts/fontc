# fontmake-rs
Where in we pursue oxidizing (context: https://github.com/googlefonts/oxidize) fontmake.

Converts source to IR, and then IR to font binary. Aims to be safe, incremental, and fast.

References

   * Intermediate Representation (IR)
      * [Why IR?](https://github.com/googlefonts/oxidize/blob/main/text/2022-11-14-why-ir.md)
      * [IR notes](https://github.com/googlefonts/oxidize/blob/main/text/2022-11-08-font-compiler-ir.md).
   * Editor perspective [note from Just](https://github.com/googlefonts/oxidize/issues/21)

## Sources to play with

Google Fonts has lots, you could try https://github.com/rsheeter/google_fonts_sources to get some.
Once you have them you could try building them:

```shell
cargo run --package fontc -- --source ../google_fonts_sources/sources/ofl/notosanskayahli/sources/NotoSansKayahLi.designspace
```
