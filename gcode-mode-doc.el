;;; gcode-mode-eldoc.el --- G-Code documentation entries  -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(defun gcode-mode--doc-build ()
  "Generate G-Code documentation entries"
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "G1" "Move" hash)
    hash))

;;; gcode-mode-doc.el ends here
