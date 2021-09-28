;;; bibtex-actions-latex.el --- Latex adapter for bibtex-actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: February 27, 2021
;; License: GPL-3.0-or-later
;; Version: 0.4
;; Homepage: https://github.com/bdarcus/bibtex-actions
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
;; A small package that provides the functions required to use bibtex-actions
;; with latex.
;;
;; Simply loading this file will enable manipulating the citations with
;; commands provided by bibtex-actions.
;;
;;; Code:

(require 'bibtex-actions)
(require 'tex)
(require 'reftex-parse)

(defcustom bibtex-actions-latex-cite-commands
  '((("cite" "Cite" "parencite"
      "Parencite" "footcite" "footcitetext" "textcite" "Textcite"
      "smartcite" "Smartcite" "cite*" "parencite*" "autocite"
      "Autocite" "autocite*" "Autocite*" "citeauthor" "Citeauthor"
      "citeauthor*" "Citeauthor*" "citetitle" "citetitle*" "citeyear"
      "citeyear*" "citedate" "citedate*" "citeurl" "fullcite"
      "footfullcite" "notecite" "Notecite" "pnotecite" "Pnotecite"
      "fnotecite") . (["Prenote"] ["Postnote"] t))
    (("nocite" "supercite") . nil))
  "Citation commands and their argument specs. The argument spec is the same
as the args argument of `TeX-parse-macro'. When calling
`bibtex-actions-insert-citation' the keys will be inserted at the position
where `TeX-parse-macro' leaves the point."
  :group 'bibtex-actions-latex
  :type '(alist :key-type (repeat string)
                :value-type sexp))

(defcustom bibtex-actions-latex-prompt-for-extra-arguments t
  "Whether to prompt for additional arguments when inserting a citation."
  :group 'bibtex-actions-latex
  :type 'boolean)

(add-to-list 'bibtex-actions-major-mode-functions
             '((latex-mode) .
               ((local-bib-files . bibtex-actions-latex--local-bib-files)
                (insert-keys . bibtex-actions-latex--insert-keys)
                (insert-citations . bibtex-actions-latex--insert-citations)
                (keys-at-point . bibtex-actions-latex--keys-at-point))))

(defun bibtex-actions-latex--local-bib-files ()
  "Retrieve local bibliographic files for a latex buffer using reftex."
  (reftex-locate-bibliography-files (TeX-master-directory)))

(defun bibtex-actions-latex--keys-at-point ()
  "Returns a list of keys at point in a latex buffer."
    (when (bibtex-actions-latex-is-a-cite-command (TeX-current-macro))
      (split-string (thing-at-point 'list t) "," t "[{} ]+")))

(defun bibtex-actions-latex--insert-keys (keys)
  "Insert comma sperated KEYS in a latex buffer."
  (insert (string-join keys ", ")))

(defvar bibtex-actions-latex-cite-command-history nil
  "Variable for history of cite commands")

(defun bibtex-actions-latex--insert-citations (keys &optional command)
  "Insert a citation consisting of KEYS.

If the command is inside a citation command keys are added to it. Otherwise
a new command is started.
If the optional COMMAND is provided use it, otherwise prompt for one.
The availiable commands and how to provide them arguments are configured
by `bibtex-actions-latex-cite-commands'. If
`bibtex-actions-latex-prompt-for-extra-arguments' is `nil`, every
command is assumed to have a single argument into which keys are inserted."
  (when keys
    (if (bibtex-actions-latex-is-a-cite-command (TeX-current-macro))
        (progn (skip-chars-forward "^,}")
               (unless (equal ?} (preceding-char)) (insert ", ")))
      (let ((macro (or command
                       (completing-read "Cite command: "
                                        (seq-mapcat #'car bibtex-actions-latex-cite-commands)
                                        nil nil nil
                                        'bibtex-actions-latex-cite-command-history nil nil))))
        (TeX-parse-macro macro
                         (when bibtex-actions-latex-prompt-for-extra-arguments
                           (cdr (bibtex-actions-latex-is-a-cite-command macro))))))
    (bibtex-actions-latex--insert-keys keys)
    (skip-chars-forward "^}") (forward-char 1)))

(defun bibtex-actions-latex-is-a-cite-command (command)
  "Returns element of `bibtex-actions-latex-cite-commands` containing COMMAND"
  (seq-find (lambda (x) (member command (car x)))
                 bibtex-actions-latex-cite-commands))

(provide 'bibtex-actions-latex)
;;; bibtex-actions-latex.el ends here
