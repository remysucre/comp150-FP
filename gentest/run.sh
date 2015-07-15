#!/bin/sh
set -e
ghc --make $1.hs -O0 -fforce-recomp $2 -rtsopts
./$1 +RTS -hT -i0.05 || true
hp2ps -e8in -c $1.hp
display $1.ps
