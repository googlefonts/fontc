# Glyphs 4 test files

All files here were written by Glyphs 4.1.1 (4108), driven headless with
glyphs-cli. `.formatVersion = 4` files are compared against the same source
saved by Glyphs 4 as format version 3, which fontc already reads.

## <name>/g4v3, <name>/g4v4

The `glyphs3/<name>` test file, opened in Glyphs 4 and saved with
`formatVersion=3` and `formatVersion=4` (as packages in `<name>-pkg/`). Both
should read and compile the same.
Glyphs 4 also changes some v3 content when saving (e.g. grey palette colors and
pre-3170 `Axis Mappings`), so compare the two saves rather than the original.

`Features` is `glyphs3/WghtVarWithStylisticSet` with a class, a feature prefix
and a second `ss01` feature added through the Python API, to cover the feature
files of v4 packages (including the `ss01.2.fea` name clash).

## fence/

Glyphs 4 constructs fontc does not support yet. Each file is a copy of a
`glyphs3/` test file, loaded in Glyphs 4, changed through its Python API and
saved with `formatVersion=4`:

* `master-inactive`: `WghtVar`, `font.masters[1].setActive_(False)`
* `layer-inactive`: `WghtVar`, `setActive_(False)` on a master layer of `exclam`
* `kerning-context`: `WghtVar`, `font.setKerningContext_({"A B": {master_id: -20}})`
* `traverse-anchors`: `WghtVar`, `component.traverseAnchors = False`
* `node-hoi`: `WghtVar`, `node.setHoiInfo_({"wght": {"ip": (10, 10)}})`
* `image-shape`: `WghtVar`, `layer.addShape_(GSImage.alloc().initWithPath_(png))`
* `shape-group`: `WghtVar`, `layer.createShapeGroupFromShapes_([path])`
* `shape-hidden`: `WghtVar`, `path.setHidden_(True)`
* `instance-static`, `instance-icon`, `instance-particles`: `WghtVar`, a new
  instance with `setType_(2)`, `setType_(3)` and `setType_(4)`
* `palette-ref`: `COLRv1-gradient`, a `GSPaletteColor` fill color
* `gradient-conic`: `COLRv1-gradient`, a `GSConicGradient` with the color line
  of the first gradient
* `gradient-extend`: `COLRv1-gradient`, `gradient.setExtendMode_(1)`
* `gradient-radial-angle`: `COLRv1-gradient`, `setAngle_(30)` on a radial gradient
* `gradient-start-radius`: `COLRv1-gradient`, `setStartRadius_(0.2)` on a radial
  gradient
* `shape-opacity`: `COLRv1-gradient`, `path.setOpacity_(0.5)`
* `shape-compositing`: `COLRv1-gradient`, `path.attributes["compositing"] = "multiply"`
* `smart-components`, `smart-component-axes`: `glyphs3/SmartComponents` and
  `glyphs3/SmartComponentExplicitAnchors` saved as is: v4 writes smart glyph
  axes on the glyph and layer coordinates for them, with `"-"` for the default
