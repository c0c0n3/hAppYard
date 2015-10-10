#!/bin/bash

#
# data-to-gd1 <rclone command> [rclone options]
# 
# Runs the specified rclone command (should either be copy or sync) to transfer 
# data to my drive account: andrea.falconi
# E.g.
#     data-to-gd1 copy
#     data-to-gd1 sync --dry-run
#
# All output goes to stdout unless the LOG_FILE_NAME environment variable is
# defined, in which case Rclone output goes to 'log/$LOG_FILE_NAME.log' except
# for the progress updates that will still be printed to stdout.
#

REMOTE_PATH=gd:cocone/data
CONFIG_NAME="gd1"
SYNC_DIR="$(cd "$(dirname "$0")/.." && pwd)"
DATA_DIR="$(cd "$(dirname "$0")/../.." && pwd)"

CONFIG_FILE="$SYNC_DIR/config/$CONFIG_NAME.conf"
EXCLUDE_FILE="$SYNC_DIR/config/exclude.txt"

#
# rclone options:
#
# --config        specify config file location
# --exclude-from  read list of file patterns to exclude from file
# (--dry-run)     do a trial run with no permanent changes
# --log-file      log all output to a given file
# --stats         print progress updates at specified intervals
# --verbose       output info about every file processed
#
cmd="rclone"
cmd="$cmd --config=$CONFIG_FILE --exclude-from=$EXCLUDE_FILE"
if [ $LOG_FILE_NAME ]; then
    LOG_FILE="$SYNC_DIR/log/$LOG_FILE_NAME.log"
    cmd="$cmd --log-file=$LOG_FILE"
fi
cmd="$cmd --stats=30s --verbose ${@:2} $1"

if [ -f $LOG_FILE ]
    then rm -f $LOG_FILE
fi

$cmd $DATA_DIR/odds-n-ends $REMOTE_PATH
$cmd $DATA_DIR/playground $REMOTE_PATH
$cmd $DATA_DIR/projects $REMOTE_PATH

