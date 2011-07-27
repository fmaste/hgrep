#!/bin/sh

./make.sh
echo "\n"
echo "--------------------------------------------------------------------------------"
echo "\n"
time grep lala /usr/share/dict/words -o -n -u
echo "\n"
echo "--------------------------------------------------------------------------------"
echo "\n"
time ./hrgrep lala /usr/share/dict/words

