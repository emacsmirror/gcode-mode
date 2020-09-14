#!/bin/sh
# bootstrap initial descriptions from the RepRap wiki
curl -s 'https://reprap.org/mediawiki/index.php?title=G-code&action=raw' | \
  sed -ne 's/^\(====*\)\s*\([GMTD][^\s:]\+\)[\s:]\+\(.*\S\+\)\s*\1$/\2\t\3/p'
