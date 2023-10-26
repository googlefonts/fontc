# parse tests

The tests in this folder are intended to test the output of the parsing stage,
before validation and compilation.

Each `.fea` file in the `./good` has a cooresponding `.PARSE_TREE` file that
represents the expected parse tree produced for this input.

In the `./bad` folder, each `.fea` file has a corresponding `.ERR` file that
represents the expected error output of the compiler.

During testing, we check that the output of each `.fea` file matches the
expected result.


## developement

Pass `FEA_WRITE_TEST_OUTPUT=1` as an environment argument when running tests in
order to write the generated output the the expected location. You can then
manually verify that this is correct before commiting the changes.

