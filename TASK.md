# Debugging test_compile_color_svg_font

## Current Status
- 6 of 8 SVG integration tests pass
- `test_compile_opacity_svg_font` - PASSES (after fix to collect backdrop colors)
- `test_compile_color_svg_font` - STILL FAILS

## Failure Mode
```
assert!(font.cpal().is_ok(), "should have CPAL table");        // PASSES
assert!(font.colr().is_ok(), "should have COLR table");        // PASSES
colr.base_glyph_list().unwrap().unwrap()                       // FAILS - returns None
```

The COLR table exists, but `base_glyph_list()` returns `None`.

## Root Cause Hypothesis
The emoji glyph (`😀` = U+1F600) paint is not being added to `colr_v1_glyphs` in `fontbe/src/colr.rs`. This means either:
1. The glyph is not in `paint_graph.base_glyphs`, OR
2. The glyph's paint is not being processed by the COLR work

## Investigation Steps

### 1. Add Debug Output to Test
In `fontc/src/lib.rs:test_compile_color_svg_font`, add:

```rust
let colr = font.colr().unwrap();
// Add debug:
println!("COLR num_base_glyphs: {:?}", colr.base_glyph_list().map(|x| x.len()));
println!("COLR base_glyph_list is_some: {}", colr.base_glyph_list().is_some());
```

Run: `cargo test -p fontc test_compile_color_svg_font -- --nocapture`

### 2. Check paint_graph Contents
In `fontbe/src/colr.rs:ColrWork::exec()`, add debug at line 433:

```rust
for glyph_name in glyph_order.names() {
    let paint = paint_graph.base_glyphs.get(glyph_name);
    println!("DEBUG: glyph {} paint: {:?}", glyph_name, paint);
    // ... existing code
}
```

### 3. Trace Why Glyph Might Not Be in paint_graph
The `ColorGlyphsWork` creates the paint_graph. Check if `😀` is in `self.svgs` when `ColorGlyphsWork` is created.

In `svg2fontir/src/source.rs:ColorGlyphsWork::exec()`:
```rust
println!("DEBUG: svgs keys: {:?}", self.svgs.keys().collect::<Vec<_>>());
let result = toir::svg_to_paint_graph(&self.svgs, self.font_transform);
println!("DEBUG: paint_graph base_glyphs: {:?}", result.paint_graph.base_glyphs.keys().collect::<Vec<_>>());
```

### 4. Check Glyph Name Encoding
The glyph name is created from `char::from_u32(0x1F600).to_string()` which gives `😀`. 
Verify this is what `discover_svgs` produces in `svg2fontir/src/parse.rs`.

### 5. Check Work Scheduling
Verify `ColorGlyphsWork` completes before `ColrWork` starts. Check that `ColorGlyphsWork` has proper `also_completes()` if needed.

## Key Files
- `fontbe/src/colr.rs:417-434` - Where COLR v1 glyphs are iterated
- `svg2fontir/src/source.rs:164-175` - ColorGlyphsWork creation
- `svg2fontir/src/toir.rs:26-48` - svg_to_paint_graph function
- `fontc/src/lib.rs:5343-5366` - The failing test

## Possible Fixes
1. If paint_graph doesn't contain the glyph → fix how SVG glyphs are named/discovered
2. If paint exists but isn't processed → fix COLR work iteration logic
3. If work scheduling issue → ensure ColorGlyphsWork completes before ColrWork
