;;; citar-pdf.el --- citar and pdf-tools inegration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Bruce D'Arcus

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides integration code for working with PDF files from within Citar.

;;; Code:

(require 'citar)
(require 'pdf-occur)

;;;###autoload
(defun citar-pdf-search-contents (keys-entries &optional str)
  "Search PDFs files using KEYS-ENTRIES.

Optional search STR can be provided in non-interactive contexts."
  (interactive (list (citar-select-refs)))
  (let ((files (citar-file--files-for-multiple-entries
                (citar--ensure-entries keys-entries)
                citar-library-paths
                '("pdf")))
        (search-str (or str (read-string "Search string: "))))
    (pdf-occur-search files search-str t)))

(eval-after-load 'embark
  (when (boundp 'embark-multitarget-actions)
    (add-to-list 'embark-multitarget-actions #'citar-pdf-search-contents)))

(provide 'citar-pdf)
;;; citar-pdf.el ends here
