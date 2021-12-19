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
(require 'thingatpt)

(defvar citar-major-mode-functions)

(defcustom citar-markdown-prompt-for-extra-arguments t
  "Whether to prompt for additional arguments when inserting a citation."
  ;; REVIEW this maybe shouldn't be mode specific?
  :group 'citar-markdown
  :type 'boolean)

(defconst citar-markdown-citation-key-regexp
  (concat "-?@"                         ; @ preceded by optional -
          "\\(?:"
          "{\\(?1:.*?\\)}"              ; brace-delimited key
          "\\|"
          "\\(?1:[[:alnum:]_][[:alnum:]]*\\(?:[:.#$%&+?<>~/-][[:alnum:]]+\\)*\\)"
          "\\)")
  "Regular expression for a Pandoc citation key.
Captures the actual key in group 1.  Implements the syntax
specified at URL
'https://pandoc.org/MANUAL.html#citation-syntax'.")

;;;###autoload
(defun citar-markdown-insert-keys (keys)
  "Insert semicolon-separated and @-prefixed KEYS in a markdown buffer."
  (insert (mapconcat (lambda (k) (concat "@" k)) keys "; ")))

;;;###autoload
(defun citar-markdown-insert-citation (keys &optional invert-prompt)
  "Insert a pandoc-style citation consisting of KEYS.
  
If the point is inside a citation, add new keys after the current
key.  

If point is immediately after the opening \[, add new keys
to the beginning of the citation."
  (let* ((citation (citar-markdown-citation-at-point))
         (keys (if citation (seq-difference keys (car citation)) keys))
         (keyconcat (mapconcat (lambda (k) (concat "@" k)) keys "; "))
         (prompt (xor invert-prompt citar-markdown-prompt-for-extra-arguments)))
    (when keys
      (if (or (not citation)
              (= (point) (cadr citation))
              (= (point) (cddr citation)))
          (let* ((prenote (when prompt (read-from-minibuffer "Prenote: ")))
                 (postnote (when prompt (read-from-minibuffer "Postnote: ")))
                 (prenote (if (string= "" prenote)  "" (concat prenote " ")))
                 (postnote (if (string= "" postnote) "" (concat ", " postnote))))
            (insert (format "[%s%s%s]" prenote keyconcat postnote)))
        (if (= (point) (1+ (cadr citation)))
            (save-excursion (insert keyconcat "; "))
          (skip-chars-forward "^;]" (cddr citation))
          (insert "; " keyconcat))))))

;;;###autoload
(defun citar-markdown-insert-edit (&optional arg)
  "Prompt for keys and call `citar-markdown-insert-citation.
With ARG non-nil, rebuild the cache before offering candidates."
  (citar-markdown-insert-citation
   (citar--extract-keys (citar-select-refs :rebuild-cache arg))))

;;;###autoload
(defun citar-markdown-key-at-point ()
  "Return citation key at point (with its bounds) for pandoc markdown citations.
Returns (KEY . BOUNDS), where KEY is the citation key at point
and BOUNDS is a pair of buffer positions.  Citation keys are
found using `citar-markdown-citation-key-regexp`.  Returns nil if
there is no key at point."
  (interactive)
  (when (thing-at-point-looking-at citar-markdown-citation-key-regexp)
    (cons (match-string-no-properties 1)
          (cons (match-beginning 0) (match-end 0)))))

;;;###autoload
(defun citar-markdown-citation-at-point ()
  "Return keys of citation at point.
Find balanced expressions starting and ending with square
brackets and containing at least one citation key (matching
`citar-markdown-citation-key-regexp`).  Return (KEYS . BOUNDS),
where KEYS is a list of the found citation keys and BOUNDS is a
pair of buffer positions indicating the start and end of the
citation."
  (save-excursion
    (cond
     ((eq ?\[ (char-after)) (forward-char))
     ((eq ?\] (char-before)) (backward-char)))
    (seq-some                           ; for each opening paren
     (lambda (startpos)                 ; return keys in balanced [ ] expr
       (when-let ((endpos (and (eq ?\[ (char-after startpos))
                               (scan-lists startpos 1 0))))
         (let (keys)
           (goto-char startpos)
           (while (re-search-forward citar-markdown-citation-key-regexp endpos t)
             (push (match-string-no-properties 1) keys))
           (when keys
             (cons (nreverse keys) (cons startpos endpos))))))
     (reverse (nth 9 (syntax-ppss))))))

;;;###autoload
(defun citar-markdown-list-keys ()
  "Returns a list of all keys from markdown citations in buffer."
  (save-excursion
    (let (matches)
      (goto-char (point-min))
      (while (re-search-forward citar-markdown-citation-key-regexp nil t)
        (push (match-string-no-properties 1) matches))
      (delete-dups (nreverse matches)))))

(provide 'citar-markdown)
;;; citar-markdown.el ends here
