# Plan: SVG → COLR/CPAL Color Font Support for fontc

## Goal

Add support for compiling a directory of SVG files into a COLR/CPAL color font,
following the established converter crate pattern (like `glyphs2fontir`, `ufo2fontir`,
`fontra2fontir`).

## Design Decisions

| Decision | Choice |
|----------|--------|
| Scope | Full color (COLR/CPAL). No monochrome, no SVG table. |
| Input | Directory of SVGs named `u{HEX}.svg` (e.g., `u0041.svg`, `u1F600.svg`) |
| Parser | `usvg` from resvg project |
| Configuration | CLI flags: `--family-name` (required), `--upem` (default 1000), `--ascender`, `--descender` |
| Naming convention | Strict `u{HEX}.svg` only. No `U+`, no `0041.svg` variations. |

## Architecture

```
svg_dir/*.svg  →  usvg parsing  →  SVG-to-IR conversion  →  fontir  →  fontbe  →  COLR/CPAL font
```

## Execution Order

> **Tracking**: Mark tasks as `[x]` when complete. Update status after each sub-plan is implemented and tested.

- [x] 1. [Create test SVG fixtures](plan/01-test-fixtures.md)
- [x] 2. [Extend IR with missing COLRv1 paint types](plan/02-ir-extensions.md)
- [x] 3. [Create `svg2fontir` crate with SVG parsing](plan/03-svg2fontir-parse.md)
- [x] 4. [Coordinate transforms: SVG viewBox → font space](plan/04-coordinate-transforms.md)
- [x] 5. [Color and gradient extraction](plan/05-color-gradient.md)
- [x] 6. [SVG hierarchy → IR Paint graph](plan/06-toir-conversion.md)
- [x] 7. [Source trait implementation](plan/07-source-trait.md)
- [x] 8. [Backend: compile new paint types to COLRv1](plan/08-backend-extensions.md)
- [x] 9. [CLI wiring and flags](plan/09-cli-wiring.md)
- [x] 10. [Integration tests](plan/10-integration-tests.md)

## Sub-Plans

Each sub-plan is a self-contained file in `plan/` with:
- What to do
- Where in the codebase
- What tests to write
- How to verify it works
