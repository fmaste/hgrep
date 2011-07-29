#!/bin/sh

rm hgrep
ghc Main.hs -o hgrep -O2 -fforce-recomp -threaded -package mtl -package bytestring -package parallel

