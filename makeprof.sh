#!/bin/sh

ghc Main.hs -o hrgrep -O2 -fforce-recomp -package mtl -package bytestring -prof -auto-all -caf-all

