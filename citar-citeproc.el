;;; citar-citeproc.el --- Citeproc reference support for citar -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
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

;;  Provides functions for formatting bibliographic references according to
;;  CSL styles, using 'citeproc-el'.

;;  To use: load this file, set the required directory paths
;;  'citar-citeproc-csl-locales-dir' and 'citar-citeproc-csl-styles-dir', set
;;  'citar-format-reference-function' to 'citar-citeproc-format-reference',
;;  and call one of the general reference functions, either
;;  'citar-insert-reference' or 'citar-copy-reference'.

;;  To set a CSL style, either set 'cite-citeproc-csl-style' manually to the
;;  path to the desired CSL style file or call
;;  'citar-citeproc-select-csl-style' to choose from a style file located in
;;  'citar-citeproc-csl-style-dir'.

;;  If a CSL style is not set before running 'citar-citeproc-format-reference',
;;  the user will be prompted to set a style.

;;  A CSL style can also be set by calling 'citar-insert-reference' or
;;  'citar-copy-reference' with a prefix-argument.

;;; Code:

(require 'citar)
(require 'citeproc-el)

(defcustom citar-citeproc-csl-styles-dir nil
  "Path to CSL styles dir."
  :group 'citar
  :type 'string)

(defcustom citar-citeproc-csl-locales-dir nil
  "Path to CSL locales dir, required for 'citar-citeproc-format-reference'."
  :group 'citar
  :type 'string)

(defvar citar-citeproc-csl-style nil
  "Path to CSL style dir, for use with 'citar-citeproc-format-reference'.")

(defun citar-citeproc-select-csl-style ()
  (interactive)
  (let* ((list (directory-files citar-citeproc-csl-styles-dir nil "csl"))
         (style (completing-read "Select CSL style file: " list nil t)))
    (setq citar-citeproc-csl-style (concat citar-citeproc-csl-styles-dir style))))

(defun citar-citeproc-format-reference (keys-entries)
  "Return formatted reference(s) for KEYS-ENTRIES via 'citeproc-el'.
  Formatting follows CSL style set in 'citar-citeproc-csl-style'.
  With prefix-argument, select CSL style."
  (when (or (eq citar-citeproc-csl-style nil)
            current-prefix-arg)
    (citar-citeproc-select-csl-style))
  (let* ((itemids (mapcar (lambda (x) (car x)) keys-entries))
         (proc (citeproc-create citar-citeproc-csl-style
			        (citeproc-hash-itemgetter-from-any citar-bibliography)
			        (citeproc-locale-getter-from-dir citar-citeproc-csl-locales-dir)
			        "en-US"))
         (references (car (progn
                            (citeproc-add-uncited itemids proc)
                            (citeproc-render-bib proc 'plain)
                            ))))
    references))

(provide 'citar-citeproc)
;;; citar-citeproc.el ends here
