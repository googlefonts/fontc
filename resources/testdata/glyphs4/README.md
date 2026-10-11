# Glyphs 4 test files

All files here were written by Glyphs 4.1.1 (4108), driven headless with
glyphs-cli. `.formatVersion = 4` files are compared against the same source
saved by Glyphs 4 as format version 3, which fontc already reads.

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
