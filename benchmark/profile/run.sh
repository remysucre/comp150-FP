#!/bin/sh
ghc -O0 --make $1.hs -prof -auto-all -caf-all -fforce-recomp -rtsopts
time ./$1 1e6 +RTS -h -p -K100M
hp2ps -e8in -c $1.hp
display $1.ps
