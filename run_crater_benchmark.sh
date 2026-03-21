#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <targets.json>"
    exit 1
fi

TARGETS="$1"
OUT_DIR="$SCRIPT_DIR/target/crater/results"
LOG_FILE="$SCRIPT_DIR/target/crater/crater_$(date +%Y%m%d_%H%M%S).log"

# Clear old results to avoid caching
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"
mkdir -p "$(dirname "$LOG_FILE")"

# Activate Python venv required by fontc_crater (ttx_diff, fontmake, etc.)
source "$SCRIPT_DIR/venv/bin/activate"

echo "Running fontc_crater. Logs -> $LOG_FILE"
echo "This may take ~30 minutes depending on the number of targets."

RAYON_NUM_THREADS=4 RUST_LOG=debug cargo run --release -p fontc_crater -- \
    ci "$TARGETS" \
    --out "$OUT_DIR" \
    2>&1 | tee "$LOG_FILE"

echo "Done. Log saved to: $LOG_FILE"
echo "To find slow sources:"
echo "  grep 'finished' '$LOG_FILE' | sort -t 'n' -k2 -rn | head -20"
