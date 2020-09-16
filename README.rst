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
point.

This package is fully documented in the source and maintained through MELPA:

https://melpa.org/#/gcode-mode
