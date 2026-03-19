# Sub-Plan 1: Create Test SVG Fixtures

## What

Create minimal SVG test fixtures in `resources/testdata/svg/`. Each SVG is a
valid usvg-parseable document with `viewBox="0 0 100 100"` and absolute coords.

## Directory Structure

```
resources/testdata/svg/
  simple_glyphs/
    u0041.svg          # 'A' - single black filled path
    u0042.svg          # 'B' - single red filled path (#FF0000)
  multi_layer/
    u1F600.svg         # 😀 - 3 colored paths (mouth, eyes, face)
  gradient_glyphs/
    u2B50.svg          # ⭐ - shape with linearGradient fill
    u2605.svg          # ★ - shape with radialGradient fill
  opacity_group/
    u2728.svg          # ✨ - group with opacity="0.5" wrapping 2 paths
  nested_groups/
    u26A1.svg          # ⚡ - nested <g transform="..."> with paths
```

## SVG Conventions

- `viewBox="0 0 100 100"` on root `<svg>`
- Absolute path commands only (M, L, C, Z — no relative)
- `xmlns="http://www.w3.org/2000/svg"`
- No `<defs>` except for gradients
- No stroke-based rendering (fill only)

## Example: `simple_glyphs/u0041.svg`

```xml
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
  <path d="M10,90 L50,10 L90,90 L70,90 L55,55 L45,55 L30,90 Z" fill="#000000"/>
</svg>
```

## Files to Create

| File | Description | Key feature tested |
|------|-------------|-------------------|
| `simple_glyphs/u0041.svg` | Single path, black fill | Basic path extraction |
| `simple_glyphs/u0042.svg` | Single path, red fill | Named color glyph |
| `multi_layer/u1F600.svg` | 3 paths, different colors | Paint::Layers |
| `gradient_glyphs/u2B50.svg` | linearGradient | PaintLinearGradient |
| `gradient_glyphs/u2605.svg` | radialGradient | PaintRadialGradient |
| `opacity_group/u2728.svg` | `<g opacity="0.5">` | PaintComposite |
| `nested_groups/u26A1.svg` | `<g transform="translate(...)">` | PaintTransform |

## How to Verify

```bash
# Each SVG should parse with usvg
cargo run --example parse_test_svg -- resources/testdata/svg/simple_glyphs/u0041.svg
# (will be possible after sub-plan 3 adds usvg dependency)
```

## Depends On

Nothing — pure file creation.

## Blocks

All other sub-plans (they need these fixtures for tests).
