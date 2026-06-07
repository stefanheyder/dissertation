#!/usr/bin/env bash
#
# Build the Zenodo upload archive for this thesis.
#
# Bundles the (large) `data/` tree together with the built thesis PDFs and the
# README into a single zip under /tmp, ready to upload to the Zenodo deposit
# (DOI 10.5281/zenodo.20258543). The archive keeps the repository's directory
# structure so it can be unpacked back onto the repo root (see download_data.sh).
#
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT="/tmp/ssm4epi_data-v1.0.0.zip"

if [[ ! -d data ]]; then
    echo "error: no 'data/' directory found at repo root ($ROOT)" >&2
    exit 1
fi

# Top-level entries to include alongside the data tree.
EXTRAS=(thesis.pdf thesis_print.pdf README.md)
for f in "${EXTRAS[@]}"; do
    if [[ ! -f "$f" ]]; then
        echo "warning: '$f' not found — it will be omitted from the archive" >&2
    fi
done

# Only pass extras that actually exist, so a missing PDF doesn't abort the zip.
INCLUDE=(data)
for f in "${EXTRAS[@]}"; do
    [[ -f "$f" ]] && INCLUDE+=("$f")
done

# Remove any stale archive so we don't append to an old one.
rm -f "$OUT"

echo "==> zipping ${INCLUDE[*]} -> $OUT (this is large, please wait)"
zip -r -X "$OUT" "${INCLUDE[@]}" -x '*/.DS_Store' '.DS_Store'

echo
echo "==> done"
du -h "$OUT"
echo "Upload this file to https://doi.org/10.5281/zenodo.20258543"
