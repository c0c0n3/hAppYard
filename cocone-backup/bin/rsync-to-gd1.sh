#!/bin/bash

SYNC_DIR="$(cd "$(dirname "$0")/.." && pwd)"
export LOG_FILE_NAME="$(basename "$0")"
$SYNC_DIR/bin/data-to-gd1.sh sync ${@:1}

