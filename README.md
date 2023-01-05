# fea-rs

Parsing and compiling [Adobe OpenType feature][spec] files.

**status**: We should be able to compile most inputs. Some generated tables
may not be optimal, but should be correct. Obscure syntax may not be supported.
Please report bugs.

## quickstart

To use this tool to compile OpenType features from a UFO file:

```sh
$ cargo run PATH_TO_FONT.ufo -o my_generated_font.ttf
```

Alternatively, you can provide a path to a FEA file directly, in which case you
will also need to pass a path to a file containing the glyph order: one glyph
per line, in utf-8.

```sh
$ cargo run features.fea --glyph-order glyph_order.txt -o my_font.ttf
```

## development

To run the tests, you will need to ensure that the test-data submodule is up to
date. After cloning the repo:

```sh
$ git submodule init && git submodule update
```

## architecture sketch

The overall design of this crate is heavily inspired by the design of [rust analyzer].

### Parsing

Parsing is broken up into a lexing and then a parsing step. Lexing identifies
tokens (numbers, strings, identifiers, symbols, keywords) but has no knowledge
of the syntax of the FEA language. Parsing takes a stream of tokens, and builds
a syntax tree out of them.

The parser is "error recovering": when parsing fails, the parser skips tokens
until it finds something that might begin a valid statement in the current
context, and then tries again. Errors are collected, and reported at the end.

## AST

The AST design is adapted from the [AST in rowan][rowan ast], part of rust
analyzer. The basic idea is that when constructing an AST node, we ensure that
certain things are true about that node's contents. We can then use the type of
that node (assigned by us) to cast that node into a concrete type which knows
how to interpret that node's contents.

## Validation

After building the AST, we perform a validation pass. This checks that
statements in the tree comply with the spec: for instance, it checks if a
referenced name exists, or if a given statement is allowed in a particular table
block. Validation is also 'error recovering'; if something fails to validate we
will continue to check the rest of the tree, and report all errors at the end.

If validation succeeds, then compilation should always succeed.

## Compilation

After validation, we do a final compilation pass, which walks the tree and
assembles the various tables and lookups. This uses [fontations][] to generate
tables, which can then be added to a font.

Some general design concepts:
- in a given 'stage', collect errors as they are encountered and report them at
  the end. For instance during parsing we will continue parsing after an error
  has occurred, and report all parse errors once parsing is complete.

A goal of this tool is to focus on providing good diagnostics when encountering
errors. In particular, we do not fail immediately when encounter

[spec]: http://adobe-type-tools.github.io/afdko/OpenTypeFeatureFileSpecification.html
[rust analyzer]: https://github.com/rust-analyzer/rust-analyzer/
[rowan ast]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/syntax.md#ast
[fontations]: https://github.com/googlefonts/fontations
