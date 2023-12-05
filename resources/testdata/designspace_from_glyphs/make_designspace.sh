#!/usr/bin/env bash

set -o nounset
set -o errexit
set -x

function generate_designspace() {
    rm -rf master_ufo/
    fontmake -o=ufo "$1"
    mv master_ufo/* resources/testdata/designspace_from_glyphs/
    rm -rf master_ufo/
}

rm -rf resources/testdata/designspace_from_glyphs/*.designspace
rm -rf resources/testdata/designspace_from_glyphs/*.ufo

generate_designspace resources/testdata/glyphs3/IntermediateLayer.glyphs
generate_designspace resources/testdata/glyphs3/WghtVar.glyphs
generate_designspace resources/testdata/glyphs3/WghtVar_Anchors.glyphs
generate_designspace resources/testdata/glyphs3/WghtVar_NoExport.glyphs
generate_designspace resources/testdata/glyphs3/SlantedFont.glyphs
