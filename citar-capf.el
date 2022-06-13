;;; citar-capf.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus
;;
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;  Citation key 'completion-at-point' for org, markdown, or latex.
;;
;;  Supports org-cite in org, and pandoc in markdown.
;;
;;; Code:

(require 'citar)

(defvar org-element-citation-key-re)
(defvar org-element-citation-prefix-re)

(declare-function org-element-at-point "ext:org-element")
(declare-function org-element-property "ext:org-element")
(declare-function citar--ref-completion-table "citar")

(defvar citar-capf--properties
  (list :exit-function #'citar-capf--exit
        :exclusive 'no)
  "Completion extra properties for `citar-capf'.")

(defun citar-capf--exit (str _status candidates)
  "Return key for STR from CANDIDATES hash."
  (insert (cadr (assoc str candidates))))

;;;###autoload
(defun citar-capf ()
  "Citation key 'completion-at-point' for org, markdown, or latex."
  (let ((citar-capf-latex-regexp
         "\\(?:cite\\(?:\\(?:[pt]\\*\\|[pt]\\)?{\\)\\)\\([[:alnum:]_-]*,\\)*\\([[:alnum:]_-]*\\)")
        (citar-capf-markdown-regexp
         (concat "-?@"                         ; @ preceded by optional -
                 "\\(?:"
                 "{\\(?1:.*?\\)}"              ; brace-delimited key
                 "\\|"
                 "\\(?1:[[:alnum:]_][[:alnum:]]*\\(?:[:.#$%&+?<>~/-][[:alnum:]]+\\)*\\)"
                 "\\)")))
    ;; only list candidates in certain contexts
    (when
        ;; conditional recognition of citation key by mode
        (cond
         ;; latex-mode
         ((derived-mode-p 'latex-mode)
          (looking-back citar-capf-latex-regexp 2))
         ;; org-mode
         ((and (derived-mode-p 'org-mode)
               (let ((element (org-element-at-point)))
                 (or (eq 'citation (org-element-at-point element))
                     (and (or (eq ?@ (char-before))
                              (looking-back org-element-citation-key-re
                                            (line-beginning-position)))
                          (let ((origin (point)))
                            (save-excursion
                              (and (re-search-backward org-element-citation-prefix-re
                                                       (org-element-property
                                                        :begin element)
                                                       t)
                                   (not (search-forward "]" origin t))))))))))
          ;; markdown-mode
          ((and (derived-mode-p 'markdown-mode)
                (or (eq ?@ (char-before))
                    (looking-back citar-capf-markdown-regexp
                                  (line-beginning-position))))))
      ;; Get and insert candidate
      (let* ((candidates (citar--get-candidates))
             (begin (save-excursion (backward-word) (point)))
             (end (point)))
        (append (list begin end candidates)
                citar-capf--properties))))

(provide 'citar-capf)
;;; citar-capf.el ends here
