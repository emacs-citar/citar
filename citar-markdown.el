;;; citar-markdown.el --- Markdown adapter for citar -*- lexical-binding: t; -*-

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

;; A small package that provides the functions required to use citar
;; with markdown.

;; Simply loading this file will enable manipulating the citations with
;; commands provided by citar.

;;; Code:

(require 'citar)

(defvar citar-major-mode-functions)

(defcustom citar-markdown-prompt-for-extra-arguments t
  "Whether to prompt for additional arguments when inserting a citation."
  ;; REVIEW this maybe shouldn't be mode specific?
  :group 'citar-markdown
  :type 'boolean)

;;;###autoload
(defun citar-markdown-insert-keys (keys)
  "Insert comma sperated KEYS in a markdown buffer."
  (insert (mapconcat (lambda (k) (concat "@" k)) keys "; ")))

;;;###autoload
(defun citar-markdown-insert-citation (keys)
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

(defconst citar-markdown-regex-citation-key
  "\\(-?@\\([[:alnum:]_][[:alnum:]_:.#$%&+?<>~/-]*\\)\\)"
  ;; borrowed from pandoc-mode
  "Regular expression for a citation key.")

;;;###autoload
(defun citar-markdown-key-at-point ()
  "Return a citation key at point for pandoc markdown citations."
  (save-excursion
    (let* ((beg (progn
                  (skip-chars-backward
                   citar-markdown-regex-citation-key)
                  (point)))
           (end (progn
                  (skip-chars-forward
                   citar-markdown-regex-citation-key)
                  (point)))
           (str (buffer-substring-no-properties beg end)))
      (cadr (split-string str "[@;]")))))

(provide 'citar-markdown)
;;; citar-markdown.el ends here
