Created by converting .glyphs files using fontmake, e.g.:

```shell
$ fontmake -o=ufo resources/testdata/glyphs3/WghtVar.glyphs
$ rm -rf resources/testdata/designspace_from_glyphs/*
$ mv master_ufo/* resources/testdata/designspace_from_glyphs/
$ rm -rf master_ufo/
```