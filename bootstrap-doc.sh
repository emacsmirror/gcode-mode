#!/bin/sh
# bootstrap initial descriptions from the RepRap wiki
curl 'https://reprap.org/mediawiki/api.php?page=G-code&action=parse&prop=wikitext&format=json' | \
  jq -r '.parse.wikitext."*"|@text' | \
  sed -ne 's/^====* \([GMTD][^: ]\+\):? \(.*\) =*=$/\1\t\2/p' | \
  perl -a -n -F'[\t\n]' -e 'print "(puthash \"$F[0]\" \"$F[1]\" hash)\n"'
