;;; citar-markdown.el --- Markdown adapter for citar -*- lexical-binding: t; -*-
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
;; with markdown.
;;
;; Simply loading this file will enable manipulating the citations with
;; commands provided by citar.
;;
;;; Code:

(require 'citar)

(defvar citar-major-mode-functions)

(defcustom citar-markdown-prompt-for-extra-arguments t
  "Whether to prompt for additional arguments when inserting a citation."
  ;; REVIEW this maybe shouldn't be mode specific?
  :group 'citar-markdown
  :type 'boolean)

(add-to-list 'citar-major-mode-functions
             '((markdown-mode) .
               ((local-bib-files . citar-markdown--local-bib-files)
                (insert-keys . citar-markdown--insert-keys)
                (insert-citation . citar-markdown--insert-citation)
                (keys-at-point . citar-markdown--keys-at-point))))

(defun citar-markdown--local-bib-files ()
  "Return local bib files for markdown."
  nil)

(defun citar-markdown--keys-at-point ()
  "Return a list of keys at point in a markdown buffer."
  (error "TODO"))

(defun citar-markdown--insert-keys (keys)
  "Insert comma sperated KEYS in a markdown buffer."
  (insert (mapconcat (lambda (k) (concat "@" k)) keys "; ")))

(defun citar-markdown--insert-citation (keys)
  "Insert a pandoc-style citation consisting of KEYS."
  (let* ((prenote  (if citar-markdown-prompt-for-extra-arguments
                       (read-from-minibuffer "Prenote: ")))
         (postnote (if citar-markdown-prompt-for-extra-arguments
                       (read-from-minibuffer "Postnote: ")))
         (prenote  (if (string= "" prenote)  "" (concat prenote  " ")))
         (postnote (if (string= "" postnote) "" (concat ", " postnote))))
    (insert (format "[%s%s%s]"
                    prenote
                    (mapconcat (lambda (k) (concat "@" k)) keys "; ")
                    postnote))))

(provide 'citar-markdown)
;;; citar-markdown.el ends here
