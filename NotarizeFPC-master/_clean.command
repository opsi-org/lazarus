#!/bin/sh
# change to working directory to location of command file: http://hints.macworld.com/article.php?story=20041217111834902
here="`dirname \"$0\"`"
cd "$here" || exit 1
rm -f hello
rm -f *.dmg
rm -f *.txt
rm -f *.o
rm -rf _CodeSignature
rm -rf lib
rm -rf backup
