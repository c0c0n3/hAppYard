#!/bin/bash

CONFIG_NAME=$1
SYNC_DIR="$(cd "$(dirname "$0")/.." && pwd)"

${SYNC_DIR}/bin/data-to.sh ${CONFIG_NAME} sync ${@:2}

