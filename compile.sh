#!/usr/bin/env bash
set -e

run() {
    echo "$@"
    "$@"
}

# Haskell
run ghc --make haskell/Main.hs \
    -o site/multicopter        \
    -ihaskell                  \
    -hidir haskell/interfaces  \
    -odir haskell/objs

# Coffee
run coffee --output site/static/ --compile coffee/*.coffee

# Static
run cp -r static/ site/
