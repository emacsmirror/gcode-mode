gcode-mode.el: Simple G-Code major mode
=======================================

``gcode-mode`` performs basic syntax highlighting on G-Code files
(mostly aimed at 3D printers), also providing optional instruction
lookup with ElDoc.

Once installed, all gcode files automatically open in this mode.
To also automatically enable ElDoc in G-Code files use:

.. code:: elisp

  (add-hook 'gcode-mode-hook 'eldoc-mode)

ElDoc will provide brief descriptions of the current instruction at
point. Embedded documentation is provided thanks to both the `RepRap
Wiki`_ and the `Marlin Documentation`_ projects.

Look at the `<docgen/>`_ for some documentation about the extraction and
processing of docstrings.

This package is fully documented in the source and maintained through MELPA:

https://melpa.org/#/gcode-mode

.. _RepRap Wiki: https://reprap.org/wiki/G-code
.. _Marlin Documentation: https://github.com/MarlinFirmware/MarlinDocumentation/
