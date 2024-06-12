# fontc_bulk

This is a binary for executing font compilation (and possibly other tasks) in
bulk.

It takes as input a JSON dictionary (as a text file) containing key/value pairs
where the keys are the names of fonts and the values are the /username/repo
portion of a github URL.

This data is derived from the cache files used by `gfautobuilder.py`: see [this
comment](https://github.com/googlefonts/fontc/issues/724#issuecomment-1977431334)
for details on how to get or rebuild this. (We restructure the data slightly to
include only the 'real_upstream' value.)

We include a file containing information for all of the known google fonts font
repositories as of June 2024, at resources/testdata/gf_fonts.json.

## font caching

by default we will checkout font sources to `build/font_cache`. You can change
this directory with the `--cache` argument.

## output

For detailed output, use the `-o/--out` flag to specify a path where we should
dump a json dictionary describing failures.
