;;; citar-latex.el --- Latex adapter for citar -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: February 27, 2021
;; License: GPL-3.0-or-later
;; Version: 0.4
;; Homepage: https://github.com/bdarcus/citar
;; Package-Requires: ((emacs "26.3"))
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
;;
;;; Commentary:
;;
;; A small package that provides the functions required to use citar
;; with latex.
;;
;; Simply loading this file will enable manipulating the citations with
;; commands provided by citar.
;;
;;; Code:

(require 'citar)
(require 'tex)
(require 'reftex-parse)

(defvar citar-major-mode-functions)

(defcustom citar-latex-cite-commands
  '((("cite" "Cite" "citet" "Citet" "parencite"
      "Parencite" "footcite" "footcitetext" "textcite" "Textcite"
      "smartcite" "Smartcite" "cite*" "parencite*" "autocite"
      "Autocite" "autocite*" "Autocite*" "citeauthor" "Citeauthor"
      "citeauthor*" "Citeauthor*" "citetitle" "citetitle*" "citeyear"
      "citeyear*" "citedate" "citedate*" "citeurl" "fullcite"
      "footfullcite" "notecite" "Notecite" "pnotecite" "Pnotecite"
      "fnotecite") . (["Prenote"] ["Postnote"] t))
    (("nocite" "supercite") . nil))
  "Citation commands and their argument specs.

The argument spec is the same as the args argument of
`TeX-parse-macro'. When calling `citar-insert-citation' the keys
will be inserted at the position where `TeX-parse-macro' leaves
the point."
  :group 'citar-latex
  :type '(alist :key-type (repeat string)
                :value-type sexp))

(defcustom citar-latex-prompt-for-extra-arguments t
  "Whether to prompt for additional arguments when inserting a citation."
  :group 'citar-latex
  :type 'boolean)

;;;###autoload
(defun citar-latex-local-bib-files ()
  "Local bibliographic for latex retrieved using reftex."
  (reftex-access-scan-info t)
  (ignore-errors (reftex-get-bibfile-list)))

;;;###autoload
(defun citar-latex-keys-at-point ()
  "Return a list of keys at point in a latex buffer."
    (when (citar-latex-is-a-cite-command (TeX-current-macro))
      (split-string (thing-at-point 'list t) "," t "[{} ]+")))

;;;###autoload
(defun citar-latex-insert-keys (keys)
  "Insert comma sperated KEYS in a latex buffer."
  (insert (string-join keys ", ")))

(defvar citar-latex-cite-command-history nil
  "Variable for history of cite commands.")

;;;###autoload
(defun citar-latex-insert-citation (keys &optional command)
  "Insert a citation consisting of KEYS.

If the command is inside a citation command keys are added to it. Otherwise
a new command is started.

If the optional COMMAND is provided use it, otherwise prompt for one.

The availiable commands and how to provide them arguments are configured
by `citar-latex-cite-commands'.

If `citar-latex-prompt-for-extra-arguments' is `nil`, every
command is assumed to have a single argument into which keys are
inserted."
  (when keys
    (if (citar-latex-is-a-cite-command (TeX-current-macro))
        (progn (skip-chars-forward "^,}")
               (unless (equal ?} (preceding-char)) (insert ", ")))
      (let ((macro (or command
                       (completing-read "Cite command: "
                                        (seq-mapcat #'car citar-latex-cite-commands)
                                        nil nil nil
                                        'citar-latex-cite-command-history nil nil))))
        (TeX-parse-macro macro
                         (when citar-latex-prompt-for-extra-arguments
                           (cdr (citar-latex-is-a-cite-command macro))))))
    (citar-latex-insert-keys keys)
    (skip-chars-forward "^}") (forward-char 1)))

(defun citar-latex-is-a-cite-command (command)
  "Return element of `citar-latex-cite-commands` containing COMMAND."
  (seq-find (lambda (x) (member command (car x)))
                 citar-latex-cite-commands))

(provide 'citar-latex)
;;; citar-latex.el ends here
