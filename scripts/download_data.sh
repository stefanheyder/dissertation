#!/usr/bin/env bash
#
# Download the published data archive from Zenodo and unpack it into `data/`.
#
# Resolves the Zenodo record id from the thesis appendix
# (chapters/AA_appendix/AA_appendix.tex) so the DOI stays single-sourced, asks
# the Zenodo REST API for the zip asset of the latest version, downloads it to
# /tmp, and unpacks only the `data/` tree onto the repo root, overwriting
# existing files in place (local-only files and the local PDFs/README are kept).
#
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT="/tmp/ssm4epi_data-v1.0.0.zip"

RECORD_ID="20258543"
echo "==> Zenodo record id: $RECORD_ID"

API="https://zenodo.org/api/records/${RECORD_ID}"
echo "==> querying $API"

read -r -p "This will download roughly 6 GB of data. Continue? [y/N] " CONFIRM
if [[ ! "$CONFIRM" =~ ^[Yy]$ ]]; then
    echo "aborted"
    exit 0
fi

# Pick the download URL of the first .zip asset in the record.
DL_URL="$(curl -fsSL "$API" | python3 -c '
import sys, json
rec = json.load(sys.stdin)
for f in rec.get("files", []):
    key = f.get("key", "")
    if key.lower().endswith(".zip"):
        print(f["links"]["self"])
        break
')"

if [[ -z "${DL_URL:-}" ]]; then
    echo "error: no .zip asset found in Zenodo record $RECORD_ID" >&2
    exit 1
fi

echo "==> downloading $DL_URL"
curl -fL -o "$OUT" "$DL_URL"

echo "==> unpacking data/ into $ROOT (overwriting in place)"
unzip -o "$OUT" 'data/*' -d "$ROOT"

echo
echo "==> done"
du -h data
