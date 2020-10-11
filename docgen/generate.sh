#!/bin/sh
./marlin2json.py _gcode/*.md > marlin.json
if [ -n "$(find ../doc/ -mindepth 1 -maxdepth 1 -type f -name '*.md' -print -quit)" ]
then
	./marlin2json.py ../doc/*.md > override.json
else
	echo '[]' > override.json
fi
./tsv2json.py ../doc/generic.tsv > generic.json
./json2eldoc.py marlin.json \
  --override override.json \
  --fallback generic.json \
  > ../gcode-mode-doc.el
