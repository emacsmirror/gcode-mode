;;; gcode-mode.el --- Simple G-Code major mode  -*- lexical-binding: t -*-

;; Author: Yuri D'Elia <wavexx@thregr.org>
;; Version: 0.1
;; URL: https://gitlab.com/wavexx/gcode-mode.el
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
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
;; (mostly aimed at 3D printers), also providing optional instruction
;; lookup with ElDoc.
;;
;; Once installed, all gcode files automatically open in this mode.
;; To also automatically enable ElDoc in G-Code files use:
;;
;; (add-hook 'gcode-mode-hook 'eldoc-mode)
;;
;; ElDoc will provide brief descriptions of the current instruction at
;; point.

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gco\\(?:de\\)?\\'" . gcode-mode))


;; Customizable faces
(defgroup gcode-mode-faces nil
  "Faces used in gcode-mode"
  :group 'faces)

(defface gcode-mode-line-number-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used for line numbers"
  :group 'gcode-mode-faces)

(defface gcode-mode-checksum-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used for checksums"
  :group 'gcode-mode-faces)

(defface gcode-mode-gcode-face
  '((t :inherit font-lock-builtin-face))
  "Face used for main G-Code instructions"
  :group 'gcode-mode-faces)

(defface gcode-mode-mcode-face
  '((t :inherit font-lock-keyword-face))
  "Face used for main M-Code instructions"
  :group 'gcode-mode-faces)

(defface gcode-mode-dcode-face
  '((t :inherit font-lock-warning-face))
  "Face used for main D-Code instructions"
  :group 'gcode-mode-faces)

(defface gcode-mode-subtype-face
  '((t :weight bold))
  "Face used for highlighting subtypes of the form GX.Y"
  :group 'gcode-mode-faces)

(defface gcode-mode-argument-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for G-Code argument names"
  :group 'gcode-mode-faces)


;; Documentation handling
(defvar gcode-mode--doc-hash
  nil
  "G-Code documentation table (lazy-loaded).")

(autoload 'gcode-mode--doc-build "gcode-mode-doc.el")

(defun gcode-mode--doc-format (instr entries param)
  "Format the retrieved documentation entry/es for display."
  (mapconcat #'identity entries " | "))

(defun gcode-mode--instr-face (instr)
  "Return the appropriate face for the current G-Code instruction."
  (let ((code (string-to-char instr)))
    (cond ((equal ?M code) 'gcode-mode-mcode-face)
	  ((equal ?D code) 'gcode-mode-dcode-face)
	  (t 'gcode-mode-gcode-face))))

(defun gcode-mode--eldoc-core ()
  "Lookup current G-Code instruction at point."

  ;; lazily initialize documentation
  (unless gcode-mode--doc-hash
    (setq gcode-mode--doc-hash (gcode-mode--doc-build)))

  ;; lookup symbol
  (save-excursion
    (let ((pos (point)))
      (beginning-of-line)
      (when (looking-at "^\\s-*\\(?:N[0-9]+\\s-+\\)?\\([GMTD]-?\\)0*\\([0-9]+\\)\\(\\.[0-9]*\\)?\\_>")
	(let* ((args-pos (match-end 0))
	       (code (concat (match-string-no-properties 1) (match-string-no-properties 2)))
	       (subtype (replace-regexp-in-string "0+$" "" (or (match-string-no-properties 3) "")))
	       (instr (concat code subtype))
	       (entries (gethash instr gcode-mode--doc-hash)))
	  (unless entries
	    ;; attempt to lookup main code if instr doesn't exist
	    (setq entries (gethash code gcode-mode--doc-hash)
		  instr code))
	  (when entries
	    (let ((face (gcode-mode--instr-face code))
		  (param))
	      ;; fetch current parameter
	      (goto-char pos)
	      (when (and (> pos args-pos)
			 (not (nth 4 (syntax-ppss)))
			 (looking-back "\\_<\\([A-Z]\\)\\S-*" args-pos))
		(setq param (match-string-no-properties 1)))
	      ;; return final documentation
	      (list (gcode-mode--doc-format instr entries param)
		    :thing instr :face face))))))))

(defun gcode-mode--eldoc-compat ()
  "Lookup current G-Code instruction at point for old versions of eldoc."
  (let ((ret (gcode-mode--eldoc-core)))
    (when ret
      (let* ((doc (car ret))
	     (attrs (cdr ret))
	     (thing (plist-get attrs :thing))
	     (face (plist-get attrs :face)))
	(concat (propertize thing 'face face) ": " doc)))))

(defun gcode-mode--eldoc-function (callback)
  "Lookup current G-Code instruction at point and call CALLBACK."
  (when callback
    (let ((ret (gcode-mode--eldoc-core)))
      (when ret
	(apply callback ret)))))


;; Main code

;;;###autoload
(define-derived-mode gcode-mode prog-mode "G-Code"
  "Major mode for G-Code instructions."

  ;; handle comments with syntax-table
  (modify-syntax-entry ?\; "<")
  (modify-syntax-entry ?\n ">")

  (font-lock-add-keywords
   nil
   '(;; line numbers
     ("^\\s-*\\(N[0-9]+\\)\\_>" (1 'gcode-mode-line-number-face))
     ;; checksums
     ("\\(\\*[0-9]+\\)\\s-*\\(?:$\\|\\s<\\)" (1 'gcode-mode-checksum-face))
     ;; instructions + subtype
     ("^\\s-*\\(?:N[0-9]+\\s-+\\)?\\([GMTD]-?[0-9]+\\(\\.[0-9]*\\)?\\)\\_>"
      (1 (gcode-mode--instr-face (match-string-no-properties 1)))
      (2 'gcode-mode-subtype-face prepend t)
      ;; arguments
      ("\\_<[A-Z]" nil nil (0 'gcode-mode-argument-face)))))

  ;; eldoc
  (if (boundp 'eldoc-documentation-functions) ; Emacs>=28
      (add-hook 'eldoc-documentation-functions #'gcode-mode--eldoc-function nil t)
    (setq-local eldoc-documentation-function #'gcode-mode--eldoc-compat)))

(provide 'gcode-mode)

;;; gcode-mode.el ends here
