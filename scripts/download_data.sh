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

APPENDIX="chapters/AA_appendix/AA_appendix.tex"
OUT="/tmp/ssm4epi_data-v1.0.0.zip"

if [[ ! -f "$APPENDIX" ]]; then
    echo "error: appendix not found at $APPENDIX" >&2
    exit 1
fi

# Parse the numeric Zenodo record id (e.g. 20258543) out of the DOI in the
# appendix. The concept DOI's record id resolves to the latest version.
RECORD_ID="$(grep -oE 'zenodo\.[0-9]+' "$APPENDIX" | head -n1 | cut -d. -f2)"
if [[ -z "${RECORD_ID:-}" ]]; then
    echo "error: could not find a 'zenodo.<id>' DOI in $APPENDIX" >&2
    exit 1
fi
echo "==> Zenodo record id: $RECORD_ID"

API="https://zenodo.org/api/records/${RECORD_ID}"
echo "==> querying $API"

# Note: this download is large (~6 GB).
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
