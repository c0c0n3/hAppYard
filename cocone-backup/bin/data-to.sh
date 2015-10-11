#!/bin/bash

#
# data-to <config name> <rclone command> [rclone options]
# 
# Runs the specified rclone command (should either be copy or sync) to transfer 
# data to my drive account 'andrea.falconi' if config name is 'gd1' or to my
# other account 'andrea.falconi.cell' if config name is 'gd2'.
# E.g.
#     data-to gd1 copy
#     data-to gd1 sync --dry-run
#
# Rclone output goes to 'log/name.cmd.opts.log' except for the progress updates
# that will still be printed to stdout; here 'name' is the config name argument
# (i.e. 'gd1' or 'gd2'), 'cmd' is the Rclone command argument (i.e. 'copy' or
# 'sync'), and 'opts' is any given Rclone option argument (e.g. '--dry-run').
#
# Assumptions
# -----------
# 1. The 'config' directory has a Rclone config file called 'gd1.conf' for my
# 'andrea.falconi' account and one called 'gd2.conf' for my other account; both
# files define a Google Drive remote called 'gd'.
# 2. The 'config' directory has a file called 'gd1.local-dirs.txt' to list the
# local directories containing the data to transfer to 'andrea.falconi';
# likewise 'gd2.local-dirs.txt' lists what to transfer to 'andrea.falconi.cell'.
# 3. Both my Drive accounts have the following directory structure:
#    [drive root]/cocone/data
#

# script arguments
CONFIG_NAME="$1"
RCLONE_CMD="$2"
RCLONE_OPTS="${@:3}"

# remote and local root paths
REMOTE_ROOT="gd:cocone/data"
SYNC_DIR="$(cd "$(dirname "$0")/.." && pwd)"

# config files
CONFIG_FILE="${SYNC_DIR}/config/${CONFIG_NAME}.conf"
LOCAL_DIRS_FILE="${SYNC_DIR}/config/${CONFIG_NAME}.local-dirs.txt"
EXCLUDE_FILE="${SYNC_DIR}/config/exclude.txt"

# log file path-name
LOG_FILE="${SYNC_DIR}/log/${CONFIG_NAME}.${RCLONE_CMD}"
if [ -n "${RCLONE_OPTS}" ]; then
    LOG_FILE="${LOG_FILE}.${RCLONE_OPTS}"
fi
LOG_FILE="${LOG_FILE}.log"

# Build and run Rclone command line.
# rclone options:
#
# --config        specify config file location
# --exclude-from  read list of file patterns to exclude from file
# (--dry-run)     do a trial run with no permanent changes
# --log-file      output to given file
# --stats         print progress updates at specified intervals
# --verbose       output info about every file processed
#
function runCmd {
    localDir=$1
    remoteDir=${REMOTE_ROOT}/$(basename "${localDir}")

    # make sure the dir is in drive, e.g. if localDir=/Volumes/data/kb then we
    # wanna create kb under gd:conone/data/ if not there already.
    # NOTES
    # 1. rclone mkdir does nothing if the remote dir already exists.
    # 2. we need to run this command even if we have a --dry-run option as
    #    the dry run would not be meaningful if the remote dir is not there.
    rclone --config="${CONFIG_FILE}"            \
           --log-file="${LOG_FILE}" --verbose   \
           mkdir "${remoteDir}"

    # recursively transfer data from ${localDir} to ${remoteDir}
    rclone --config="${CONFIG_FILE}" --exclude-from="${EXCLUDE_FILE}" \
           --log-file="${LOG_FILE}" --stats=30s --verbose             \
           ${RCLONE_OPTS} ${RCLONE_CMD} "${localDir}" "${remoteDir}"
}

function removePreviousLogFile {
    if [ -f "${LOG_FILE}" ]; then
        rm -f "${LOG_FILE}"
    fi
}

# Parse config file listing directories to read data from into LOCAL_DIRS array.
LOCAL_DIRS=()
function parseLocalDirsFile {
    # read in file listing local directories from which to transfer data;
    # each line will be in its own array slot.
    # see: http://stackoverflow.com/questions/11393817/bash-read-lines-in-file-into-an-array
    IFS=$'\n' read -d '' -r -a lines < "${LOCAL_DIRS_FILE}"

    # take each line starting with a '/' as a local directory from which to
    # transfer data to Google Drive; ignore any other line that doesn't start
    # with a '/'.
    for line in "${lines[@]}"
    do
        if [ ${line:0:1} == '/' ]; then
            LOCAL_DIRS+=($line)
        fi
    done
}

# Execute, providing basic feedback.

removePreviousLogFile
parseLocalDirsFile

echo ===========================================================================
echo BEGIN ${RCLONE_CMD} to ${CONFIG_NAME}
echo SOURCE DIRS: "${LOCAL_DIRS[@]}"
echo LOG FILE: ${LOG_FILE}

for srcDir in "${LOCAL_DIRS[@]}"
do
    runCmd "${srcDir}"
done

echo END ${RCLONE_CMD} to ${CONFIG_NAME}
echo ===========================================================================

