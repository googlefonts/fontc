# fontmake-rs
Where in we pursue oxidizing (context: https://github.com/googlefonts/oxidize) fontmake.

## Sources to play with

Google Fonts has lots, you could try https://github.com/rsheeter/google_fonts_sources to get some.
Once you have them you could try building them:

```shell
cargo run --package fontc -- --source ../google_fonts_sources/sources/ofl/notosanskayahli/sources/NotoSansKayahLi.designspace
```