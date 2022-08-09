;;; citar-latex.el --- Latex adapter for citar -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2021-2022 Bruce D'Arcus
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A small package that provides the functions required to use citar
;; with latex.

;; Loading this file will enable manipulating the citations with
;; commands provided by citar.

;;; Code:

(require 'citar)
(require 'tex nil t)
(require 'reftex-parse)
(require 'reftex-cite)

(defvar TeX-esc)

(declare-function TeX-find-macro-boundaries "ext:tex")
(declare-function TeX-parse-macro "ext:tex")

(defvar citar-major-mode-functions)

(defcustom citar-latex-cite-commands
  '((("cite" "Cite" "citet" "Citet" "citep" "Citep" "parencite"
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

(defcustom citar-latex-prompt-for-cite-style t
  "Whether to prompt for a citation command when inserting."
  :group 'citar
  :type '(radio (const :tag "Prompt for a command" t)
                (const :tag "Do not prompt for a command" nil))
  :safe 'always)

(defcustom citar-latex-default-cite-command "cite"
  "Default command for citations.

Must be in `citar-latex-cite-commands'. Used when as a cite
command when prompting for one is disabled, and as the default
entry when it is enabled."
  :group 'citar
  :type 'string
  :safe 'always)

(defcustom citar-latex-prompt-for-extra-arguments t
  "Whether to prompt for additional arguments when inserting a citation."
  :group 'citar-latex
  :type 'boolean)

;;;###autoload
(defun citar-latex-local-bib-files ()
  "Local bibliographic for latex retrieved using reftex."
  (ignore-errors
    (reftex-access-scan-info t)
    (reftex-get-bibfile-list)))

;;;###autoload
(defun citar-latex-key-at-point ()
  "Return citation key at point with its bounds.

The return value is (KEY . BOUNDS), where KEY is the citation key
at point and BOUNDS is a pair of buffer positions.

Return nil if there is no key at point."
  (save-excursion
    (when-let* ((bounds (citar-latex--macro-bounds))
                (keych "^,{}")
                (beg (progn (skip-chars-backward keych (car bounds)) (point)))
                (end (progn (skip-chars-forward keych (cdr bounds)) (point)))
                (pre (buffer-substring-no-properties (car bounds) beg))
                (post (buffer-substring-no-properties end (cdr bounds))))
      (and (string-match-p "{\\([^{}]*,\\)?\\'" pre)  ; preceded by { ... ,
           (string-match-p "\\`\\(,[^{}]*\\)?}" post) ; followed by , ... }
           (goto-char beg)
           (looking-at (concat "[[:space:]]*\\([" keych "]+?\\)[[:space:]]*[,}]"))
           (cons (match-string-no-properties 1)
                 (cons (match-beginning 1) (match-end 1)))))))

;;;###autoload
(defun citar-latex-citation-at-point ()
  "Find citation macro at point and extract keys.

Find brace-delimited strings inside the bounds of the macro,
splits them at comma characters, and trims whitespace.

Return (KEYS . BOUNDS), where KEYS is a list of the found
citation keys and BOUNDS is a pair of buffer positions indicating
the start and end of the citation macro."
  (save-excursion
    (when-let ((bounds (citar-latex--macro-bounds)))
      (let ((keylists nil))
        (goto-char (car bounds))
        (while (re-search-forward "{\\([^{}]*\\)}" (cdr bounds) 'noerror)
          (push (split-string (match-string-no-properties 1) "," t "[[:space:]]*")
                keylists))
        (cons (apply #'append (nreverse keylists))
              bounds)))))

(defun citar-latex--macro-bounds ()
  "Return the bounds of the citation macro at point.

Return a pair of buffer positions indicating the beginning and
end of the enclosing citation macro, or nil if point is not
inside a citation macro."
  (unless (fboundp 'TeX-find-macro-boundaries)
    (error "Please install AUCTeX"))
  (save-excursion
    (when-let* ((bounds (TeX-find-macro-boundaries))
                (macro (progn (goto-char (car bounds))
                              (looking-at (concat (regexp-quote TeX-esc)
                                                  "\\([@A-Za-z]+\\)"))
                              (match-string-no-properties 1))))
      (when (citar-latex--is-a-cite-command macro)
        bounds))))

(defvar citar-latex-cite-command-history nil
  "Variable for history of cite commands.")

;;;###autoload
(defun citar-latex-insert-citation (keys &optional invert-prompt command)
  "Insert a citation consisting of KEYS.

If the command is inside a citation command keys are added to it. Otherwise
a new command is started.

If the optional COMMAND is provided use it (ignoring INVERT-PROMPT).
Otherwise prompt for a citation command, depending on the value of
`citar-latex-prompt-for-cite-style'. If INVERT-PROMPT is non-nil, invert
whether or not to prompt.

The availiable commands and how to provide them arguments are configured
by `citar-latex-cite-commands'.

If `citar-latex-prompt-for-extra-arguments' is nil, every
command is assumed to have a single argument into which keys are
inserted."
  (when keys
    (if-let ((bounds (citar-latex--macro-bounds)))
        (pcase-exhaustive (progn (skip-chars-forward "^,{}" (cdr bounds))
                                 (following-char))
          ((guard (= (point) (cdr bounds))) ; couldn't find any of ",{}"
           (insert "{}")
           (backward-char))
          ((or ?{ ?,)                   ; insert after following "{" or ","
           (forward-char)
           (unless (looking-at-p "[[:space:]]*[},]")
             (insert ",")
             (backward-char)))
          (?}                           ; insert before "}"
           (skip-chars-backward "[:space:]")
           (unless (member (preceding-char) '(?{ ?,))
             (insert ","))))
      (let ((macro
             (or command
                 (if (xor invert-prompt citar-latex-prompt-for-cite-style)
                     (citar-latex--select-command)
                   citar-latex-default-cite-command))))
        (TeX-parse-macro macro
                         (when citar-latex-prompt-for-extra-arguments
                           (cdr (citar-latex--is-a-cite-command macro))))))
    (insert (string-join keys ","))
    (skip-chars-forward "^}") (forward-char 1)))

;;;###autoload
(defun citar-latex-insert-edit (&optional _arg)
  "Prompt for keys and call `citar-latex-insert-citation.
With ARG non-nil, rebuild the cache before offering candidates."
  (citar-latex-insert-citation (citar-select-refs)))

(defun citar-latex--select-command ()
  "Complete a citation command for LaTeX."
  (completing-read "Cite command: "
                   (seq-mapcat #'car citar-latex-cite-commands)
                   nil nil nil
                   'citar-latex-cite-command-history
                   citar-latex-default-cite-command nil))

(defun citar-latex--is-a-cite-command (command)
  "Return element of `citar-latex-cite-commands' containing COMMAND."
  (seq-find (lambda (x) (member command (car x)))
            citar-latex-cite-commands))

;;;###autoload
(defalias 'citar-latex-list-keys #'reftex-all-used-citation-keys)

(provide 'citar-latex)
;;; citar-latex.el ends here
