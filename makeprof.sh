#!/bin/sh

ghc Main.hs -o hgrep -O2 -fforce-recomp -package mtl -package bytestring -package parallel -prof -auto-all -caf-all

