# GlyphData.xml

This directory includes XML files containing data used by glyphs.app. This data
is extracted from the glyphsLib python package using the `update.py` script
contained here.

That data in turn is takend from the [GlyphsInfo](https://github.com/schriftgestalt/GlyphsInfo)
repository.

This data is bundled into our crate using a `build.rs` script in the crate root.
