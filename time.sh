#!/bin/sh

./make.sh
echo
echo "--------------------------------------------------------------------------------"
echo
time -p grep lala /usr/share/dict/words -o -n
echo
echo "--------------------------------------------------------------------------------"
echo
time -p ./hgrep lala /usr/share/dict/words +RTS -K100000000

