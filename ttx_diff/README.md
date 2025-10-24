# ttx-diff

A tool for comparing font compiler outputs between fontc (Rust) and fontmake (Python).

## Overview

`ttx-diff` is a helper utility that compares binary font outputs from two different font compilers:
- **fontc**: The Rust-based font compiler from Google Fonts
- **fontmake**: The Python-based font compiler

The tool converts each binary font to TTX (XML) format, normalizes expected differences, and provides a detailed comparison summary.

## Installation

### From PyPI

```bash
pip install ttx-diff
```

### From source

```bash
git clone https://github.com/googlefonts/fontc.git
cd fontc/ttx_diff
pip install -e .
```

## Requirements

- Python 3.10 or higher
- `fontc` and `otl-normalizer` binaries (see below)

All Python dependencies (fontmake, fonttools, etc.) are installed automatically.

### Getting fontc and otl-normalizer

The tool needs the `fontc` and `otl-normalizer` binaries. You can:

1. **Specify paths explicitly** (recommended for most users):
   ```bash
   ttx-diff --fontc_path /path/to/fontc --normalizer_path /path/to/otl-normalizer source.glyphs
   ```

2. **Add them to your PATH**: If `fontc` and `otl-normalizer` are in your PATH, they'll be found automatically

3. **Run from fontc repository**: If you run from the fontc repository root, the tool will automatically build the binaries for you

## Usage

**Note**: Unlike the original `ttx_diff.py` script, this standalone version can be run from any directory. You don't need to be in the fontc repository.

### Basic comparison

Rebuild with both fontmake and fontc and compare:

```bash
ttx-diff --fontc_path /path/to/fontc --normalizer_path /path/to/otl-normalizer path/to/source.glyphs
```

If the binaries are in your PATH:

```bash
ttx-diff path/to/source.glyphs
```

### Selective rebuild

Rebuild only fontc's font and reuse existing fontmake output:

```bash
ttx-diff --rebuild fontc path/to/source.glyphs
```

### JSON output

Output results in machine-readable JSON format, as used by the [`fontc_crater`](https://github.com/googlefonts/fontc/tree/main/fontc_crater) tool.

```bash
ttx-diff --json path/to/source.glyphs
```

### Using gftools

Compare using gftools build pipeline:

```bash
ttx-diff --compare gftools --config config.yaml path/to/source.glyphs
```

## Development

### Running tests

```bash
pip install -e .[test]
pytest
```

### Running tests with coverage

```bash
pytest --cov=ttx_diff --cov-report=html
```