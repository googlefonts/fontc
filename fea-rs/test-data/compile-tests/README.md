# compile-tests

Each subdirectory in this directory should contain a `glyph_order.txt` file and a
pair of directores, 'good' and 'bad', each of which contains a number of `.fea`
files. Each of these files is intended to be compiled using the provided glyph
order.

## the good

Files in the 'good' directory are expected to compile successfuly. The output is
not currently checked, although this is something I would like to do later.

## the bad

Files in the 'bad' directory are expected failures. Each of these has a
corresponding `.ERR` file, which contains the expected diagnostic output.

To update the diagnostics, pass `FEA_WRITE_TEST_OUTPUT=1` as an environment
variable when running the tests.
