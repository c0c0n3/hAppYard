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
# NOTES
# 1. 'copy' the source to the destination. 
# Doesn’t transfer unchanged files. Doesn’t delete files from the destination.
# 2. 'sync' the source to the destination, changing the destination only. 
# Doesn’t transfer unchanged files. Destination is updated to match source, 
# including deleting files if necessary. Since this can cause data loss, test 
# first with the --dry-run flag.
# 3. MD5SUM. Normally rclone will look at modification time and size of files to 
# see if they are equal. If you pass the --checksum option, then rclone checks 
# MD5SUM and size to determine if files are equal.
#

REMOTE_PATH=gd:cocone/data
CONFIG_FILE="config/gd1.conf"
EXCLUDE_FILE="config/exclude.txt"

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
    LOG_FILE="log/$LOG_FILE_NAME.log"
    cmd="$cmd --log-file=$LOG_FILE"
fi
cmd="$cmd --stats=30s --verbose ${@:2} $1"

if [ -f $LOG_FILE ]
    then rm -f $LOG_FILE
fi

$cmd ../odds-n-ends $REMOTE_PATH
$cmd ../playground $REMOTE_PATH
$cmd ../projects $REMOTE_PATH
