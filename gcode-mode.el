;;; gcode-mode.el --- Simple G-Code mode  -*- lexical-binding: t -*-

;; Author: Yuri D'Elia <wavexx@thregr.org>
;; Version: 0.1
;; URL: https://gitlab.com/wavexx/gcode-mode.el
;; Package-Requires: ((emacs "24.4"))
;; Keywords: gcode, languages, highlight, syntax

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; `gcode-mode' performs basic syntax highlighting on G-Code files
;; (mostly aimed at 3D printers).

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gco\\(?:de\\)?\\'" . gcode-mode))

;;;###autoload
(define-derived-mode gcode-mode prog-mode "G-Code"
  "Major mode for G-Code instructions."

  ;; handle comments with syntax-table
  (modify-syntax-entry ?\; "<")
  (modify-syntax-entry ?\n ">")

  ;; M/G instructions
  (font-lock-add-keywords
   nil
   '(("^\\s-*\\([MG][0-9]+\\)\\(\\(?:\\.[0-9]*\\)?\\)\\_>"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face) ; Prusa subtype extension
      ("\\_<[A-Z]" nil nil (0 font-lock-variable-name-face))))))

(provide 'gcode-mode)

;;; gcode-mode.el ends here
