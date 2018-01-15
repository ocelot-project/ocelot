#!/usr/bin/env bash

# TODO: actually provision a flash drive into an Ocelot live system

USER=`whoami`
WORKING_PATH="/tmp/ocelot/$USER/mkimage"
SCRIPT=$(readlink -f "$0")
SCRIPT_PATH=`dirname $SCRIPT`
EXEC_PATH=`pwd`

rm -rf "$WORKING_PATH";

mkdir -p "$WORKING_PATH" &&
    cd "$WORKING_PATH" &&
    nix-build '<nixpkgs/nixos>' \
              -A config.system.build.isoImage \
              -I nixos-config="$SCRIPT_PATH/make-image.nix" &&
    cd "$EXEC_PATH" &&
    cp --no-preserve=mode "$WORKING_PATH/result/iso"/* . &&
    rm -rf "$WORKING_PATH"

