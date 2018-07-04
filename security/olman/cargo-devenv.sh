#!/usr/bin/env bash

if command -v nix-shell 2>/dev/null >/dev/null; then
    nix-shell --run "cargo $*"
else
    cargo $*
fi
