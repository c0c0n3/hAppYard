#!/bin/bash

#
# Output the log file lines that are strictly relevant to the data transfer.
# I think any of the lines matching the regex below are not relevant...
#

grep -v \
     -E ':( Unchanged | Size and modification | Finished reading | Reading | Excluding )' \
     $1
