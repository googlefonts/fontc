# compile-tests

Each subdirectory in this directory should contain a `glyph_order.txt` file and a
pair of directores, 'good' and 'bad', each of which contains a number of `.fea`
files. Each of these files is intended to be compiled using the provided glyph
order.

## the good

Files in the 'good' directory are expected to compile successfuly. The expected
result of compilation is represented as a `.ttx` file, stored alongside the test
case.

## the bad

Files in the 'bad' directory are expected failures. Each of these has a
corresponding `.ERR` file, which contains the expected diagnostic output; this
ensures that the error that we report for a given failure is sensible.


## adding new tests

To add a new test, you can just add a new `.fea` file to the appropriate
directory. If necessary, you can add new glyphs to `glyph_order.txt`. After
adding your test cases, you can pass `FEA_WRITE_TEST_OUTPUT=1` as an environment
variable when running the tests in order to regenerate the corresponding `.ttx`
or `.ERR` files (which you need to manually verify before commiting!)
