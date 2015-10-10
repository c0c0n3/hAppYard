#!/bin/bash

TARGET_DIR=/Volumes/data/_sync_

mkdir -p $TARGET_DIR/bin
mkdir -p $TARGET_DIR/config
mkdir -p $TARGET_DIR/log

cp bin/*.sh $TARGET_DIR/bin/
chmod +x $TARGET_DIR/bin/*.sh

cp config/* $TARGET_DIR/config/

