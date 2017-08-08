#!/bin/bash 

stack haddock

rm -rf doc
docs=$(stack path | sed -n -e 's/^.*local-doc-root: //p')
cp -a $docs .