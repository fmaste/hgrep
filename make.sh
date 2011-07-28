#!/bin/sh

ghc Main.hs -o hrgrep -O2 -fforce-recomp -threaded -package mtl -package bytestring -package parallel

