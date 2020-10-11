gcode-mode.el documentation extraction
======================================

Overview
--------

Documentation lookup of G-Code commands is *incredibly* useful, given
the instructions themselves are not self-explicative and rely on
brute-force memorization instead.

Two documentation sources are currently used:

- The `RepRap Wiki`_ G-Code documentation page
- The `Marlin Documentation`_ project

The RapRap Wiki is the most comprehensive and contains instructions not
only for RepRap firmware, but for most open-source alternatives as well.
Sadly, the Wiki formatting is not well standardized across the various
instructions on the page, making it very hard to extract for machine
consumption.

The Marlin Documentation is mostly relevant for Marlin itself and
derivatives, however it's extremely well formatted, using a combination
YAML and Markdown.

For this reason, the Marlin documentation is currently favored as it
includes short descriptions for all G-Code parameters, while a
hand-edited RepRap summary is being used to fill the gaps.

The Marlin Documentation is parsed directly from its Jekill source files
and converted into a JSON representation by `./marlin2json.py`, which
also converts all Markdown and HTML into simple text strings.

The list of instructions and short descriptions are extracted from the
RepRap Wiki using `./reprap2tsv.sh`. This will generate a 2-column
table, with one line for each instruction. Such table has been
hand-edited for consistency and put into `doc/generic.tsv`. The table
can be converted to (a subset of) the same JSON representation using
`./tsv2json.py`.

Finally, the JSON files can be merged using various strategies using
`./json2eldoc.py`, which produces the final `gcode-mode-doc.el` file.

Most of this procedure is currently a bit rough around the edges, but
can be run by calling `./generate.sh`.


Quick-build procedure
---------------------

Clone the `Marlin Documentation`_ repository and symlink the `_gcode`
directory into `docgen/`, then run `./generate.sh` to rebuild all
documentation into `gcode-mode-doc.el` automatically::

  cd docgen/
  ln -s /path/to/MarlinDocumentation/_gcode .
  ./generate.sh

Updating the RepRap descriptions harder. Run::

  ./reprap2tsv.sh > ../doc/generic-new.tsv

To extract the all descriptions, then diff/update the files by comparing
manually the results. The current file has been heavily hand-edited.

The generated documentation is currently checked-in into the repository
after review.


What's missing
--------------

We can override/augment descriptions of existing or new instructions by
placing additional Jekill files (same format as Marlin's) into `doc/`.
This is not currently used.

The JSON files are annotated with the G-Code flavor for future use.
Allowing to select a documentation flavor is not currently implemented.

The generated documentation already includes all variants/flavors of
each instruction, but only the first entry is currently shown.

Improving extraction of the RepRap documentation to include arguments
would be a boon. To do this, cooperation with the RepRap Wiki community
should probably be the way to go. If we could switch slowly to
consistent formatting one instruction at-a-time (using a macro, for
example) we would allow improved descriptions across the board instead
of relying on hand-editing and local overrides.


.. _RepRap Wiki: https://reprap.org/wiki/G-code
.. _Marlin Documentation: https://github.com/MarlinFirmware/MarlinDocumentation/
