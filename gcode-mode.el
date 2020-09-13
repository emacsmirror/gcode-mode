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


;; Documentation handling
(defvar gcode-mode--doc-hash
  nil
  "G-Code documentation table (lazy-loaded).")

(autoload 'gcode-mode--doc-build "gcode-mode-doc.el")

(defun gcode-mode--eldoc (callback)
  "Lookup current G-Code instruction at point.

Calls CALLBACK with the documentation for the G-Code instruction at point."

  ;; lazily initialize documentation
  (unless gcode-mode--doc-hash
    (setq gcode-mode--doc-hash (gcode-mode--doc-build)))

  ;; lookup symbol
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\s-*\\(?:N[0-9]+\\s-+\\)?\\([MG][0-9]+\\)\\(\\(?:\\.[0-9]*\\)?\\)\\_>")
      (let* ((code (match-string-no-properties 1))
	     (subtype (match-string-no-properties 2))
	     (full (concat code subtype))
	     (doc (gethash full gcode-mode--doc-hash)))
	(unless doc
	  ;; attempt to lookup main code if full doesn't exist
	  (setq doc (gethash code gcode-mode--doc-hash)))
	(when doc
	  (funcall callback doc
		   :thing full
		   :face 'gcode-mode-gcode-face))))))


;; Main code

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
      ("\\_<[A-Z]" nil nil (0 font-lock-variable-name-face)))))

  ;; eldoc
  (if (boundp 'eldoc-documentation-functions) ; Emacs>=28
      (add-hook 'eldoc-documentation-functions #'gcode-mode--eldoc nil t)
    (setq-local eldoc-documentation-function #'gcode-mode--eldoc)))

(provide 'gcode-mode)

;;; gcode-mode.el ends here
