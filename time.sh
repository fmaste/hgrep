#!/bin/sh

./make.sh
echo
echo "--------------------------------------------------------------------------------"
echo
time grep lala /usr/share/dict/words -o -n
echo
echo "--------------------------------------------------------------------------------"
echo
time ./hrgrep lala /usr/share/dict/words +RTS -K100000000

