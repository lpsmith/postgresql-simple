#!/bin/bash

echo "Generating CHANGELOG.md"
echo

cat <<HEADER > CHANGELOG.md
For the full changelog, see
<https://github.com/lpsmith/postgresql-simple/blob/master/CHANGES.md>

HEADER

perl -ne '$n++ if /^###/; exit if $n==2; print' CHANGES.md >> CHANGELOG.md

cat CHANGELOG.md
rm -f ./dist/*.tar
rm -f ./dist/*.tar.gz
cabal sdist
cd dist
wc *.tar.gz
gunzip *.gz
wc *.tar
zopfli *.tar
wc *.tar.gz
cd ..
rm -f ./dist/*.tar
