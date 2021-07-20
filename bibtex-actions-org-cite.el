;;; bibtex-actions-org-cite.el --- Org-cite support for bibtex-actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: July 11, 2021
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/bdarcus/bibtex-actions
;; Package-Requires: ((emacs "26.3")(org "9.5"))
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
;;  This is a small package that intergrates bibtex-actions and org-cite.  It
;;  provides a simple org-cite processor with "follow" and "insert" capabilties.
;;
;;  Simply load this file and it will configure them for 'org-cite.'
;;
;;; Code:

(require 'bibtex-actions)
(require 'org)
(require 'oc)
(require 'oc-basic)
(require 'oc-csl)
(require 'citeproc)
(require 'embark)

(declare-function bibtex-actions-at-point "bibtex-actions")
(declare-function org-open-at-point "org")

(defface bibtex-actions-org-cite-style-preview
    '((t :foreground "forestgreen"
         :underline nil
         :weight bold))
  "Face for org-cite previews."
  :group 'bibtex-actions-org-cite)

(defcustom bibtex-actions-org-cite-preview-targets '("natbib" "csl")
  "Export processor targets for which to display previews."
  ;; REVIEW not sure this is the best approach. Bennfit, though, is this can
  ;; also configure order in the UI.
  :group 'bibtex-actions-org-cite
  :type '(choice
          (const biblatex)
	  (const csl)
	  (const natbib)))

;;; Internal variables

(defvar bibtex-actions--csl-processor-cache nil
  "Cache for the citation preview processor.")

(make-variable-buffer-local 'bibtex-actions--csl-processor-cache)

(defun bibtex-actions--csl-processor ()
  "Return a `citeproc-el' processor for style preview."
  (or bibtex-actions--csl-processor-cache
      (let* ((bibliography (org-cite-list-bibliography-files))
	     (processor
	      (citeproc-create
	       org-cite-csl--fallback-style-file
	       (org-cite-csl--itemgetter bibliography)
	       (org-cite-csl--locale-getter))))
	(setq bibtex-actions--csl-processor-cache processor)
	processor)))

;; TODO convert to defcustoms

(defvar bibtex-actions-org-cite-style-preview-alist
  '((natbib . bibtex-actions-org-cite--natbib-style-preview)
    (biblatex . bibtex-actions-org-cite--biblatex-style-preview)))

(defvar bibtex-actions-org-cite--biblatex-style-preview
  '(;; Default "nil" style.
    ("/" . "\\autocite")
    ("/b" . "\\cite")
    ("/c" . "\\Autocite")
    ("/bc" . "\\Cite")
    ;; "text" style.
    ("text" . "\\textcite")
    ("text/c" .  "\\Textcite")
    ;; "nocite" style.
    ("nnocite" . "\\nocite")
    ;; "author" style.
    ("author/c" . "\\Citeauthor*")
    ("author/f" . "\\citeauthor")
    ("author/cf" . "\\Citeauthor")
    ("author" . "\\citeauthor*")
    ;; "locators" style.
    ("locators/b" . "\\notecite")
    ("locators/c" . "\\Pnotecite")
    ("locators/bc" . "\\Notecite")
    ("locators" . "\\pnotecite")
    ;; "noauthor" style.
    ("noauthor" .  "\\autocite*")))

(defvar bibtex-actions-org-cite--natbib-style-preview
  '(;; Default ("nil") style.
    ("/" . "\\citep")
    ("/b" . "\\citealp")
    ("/c" . "\\Citep")
    ("/f" . "\\citep*")
    ("/bc" .  "\\Citealp")
    ("/bf" . "\\citealp*")
    ("/cf" . "\\Citep*")
    ("/bcf" . "\\Citealp*")
    ;; "text" style.
    ("text" . "\\citet")
    ("text/b" . "\\citealt")
    ("text/c" . "\\Citet")
    ("text/f" . "\\citet*")
    ("text/bc" . "\\Citealt")
    ("text/bf"  .   "\\citealt*")
    ("text/cf" .    "\\Citet*")
    ("text/bcf" . "\\Citealt*")
    ;; "author" style.
    ("author" . "\\citeauthor")
    ("author/c" . "\\Citeauthor")
    ("author/f" . "\\citeauthor*")
    ;; "noauthor" style.
    ("noauthor" . "\\citeyearpar")
    ("noauthor/b" .   "\\citeyear")
    ;; "nocite" style.
    ("nocite" .  "\\nocite")))


;TODO
;(defvar bibtex-actions-org-cite-open-default

;; Org-cite processor

(defun bibtex-actions-org-cite-insert (&optional multiple)
  "Return a list of keys when MULTIPLE, or else a key string."
  (let ((references (bibtex-actions-read)))
    (if multiple
        references
      (car references))))

(defun bibtex-actions-org-cite-follow (_datum _arg)
  "Follow processor for org-cite."
  (call-interactively bibtex-actions-at-point-function))

(org-cite-register-processor 'bibtex-actions-org-cite
  :insert (org-cite-make-insert-processor
           #'bibtex-actions-org-cite-insert
           #'bibtex-actions-org-cite-select-style)
  :follow #'bibtex-actions-org-cite-follow)

(defun bibtex-actions-org-cite-select-style ()
"Complete a citation style for org-cite with preview."
  (interactive)
  (let ((oc-styles (bibtex-actions-org-cite--styles-candidates)))
    (completing-read "Select style: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . bibtex-actions-org-cite--style-preview-annote)
                             (cycle-sort-function . identity)
                             (display-sort-function . identity)
                             (group-function . bibtex-actions-org-cite--styles-group-fn))
                           (complete-with-action action oc-styles str pred))))))

(defun bibtex-actions-org-cite--styles-candidates ()
  "Generate candidate list."
  bibtex-actions-org-cite--natbib-style-preview)

(defun bibtex-actions-org-cite--make-style-previews (style &optional _citation)
  "Return rendered candidate string for STYLE for TARGET preview."
  ;; FIX doesn't create the full string
  (let ((preview-targets bibtex-actions-org-cite-preview-targets))
  (concat
   (when (member 'natbib preview-targets)
     (cdr (assoc style bibtex-actions-org-cite--natbib-style-preview)))
   (when (member 'biblatex preview-targets)
     (cdr (assoc style bibtex-actions-org-cite--biblatex-style-preview)))
   (when (member 'csl preview-targets) nil))))

(defun bibtex-actions-org-cite--styles-group-fn (style transform)
  "Return group title of STYLE or TRANSFORM the candidate.
This is a group-function that groups org-cite style/variant
strings by style."
    (let ((short-style
           (if (string-match "^/[bcf]*" style) "default"
             (car (split-string style "/")))))
    (if transform
        ;; Use the candidate string as is.
        (concat "  " (truncate-string-to-width style 20 nil 32))
      ;; Transform for grouping and display.
      (cond
       ((string= short-style "default") "Default")
       ((string= short-style "author") "Author-Only")
       ((string= short-style "locators") "Locators-Only")
       ((string= short-style "text") "Textual/Narrative")
       ((string= short-style "nocite") "No Cite")
       ((string= short-style "noauthor") "Suppress Author")))))

(defun bibtex-actions-csl-render-citation (citation)
  "Render CITATION."
  (let ((proc (bibtex-actions--csl-processor)))
    (citeproc-clear proc)
    (let* ((info (list :cite-citeproc-processor proc))
	   (cit-struct (org-cite-csl--create-structure citation info)))
      (citeproc-append-citations (list cit-struct) proc)
      (car (citeproc-render-citations proc 'plain t)))))

;(defun bibtex-actions-org-cite--style-preview (style output)
;  "Return preview from org-cite STYLE for OUTPUT."
;  ;; This should be added to oc.el.
;  (let oc-styles (cdr (assoc output bibtex-actions-org-cite-style-preview-alist))
;       (cdr (assoc style oc-styles))))

(defun bibtex-actions-org-cite--style-preview-annote (style &optional _citation)
  "Annotate STYLE with CITATION preview."
  ;; Let's start with simple.

  (let* ((preview (bibtex-actions-org-cite--make-style-previews style))
         ;; TODO look at how define-face does this.
         (formatted-preview (truncate-string-to-width preview 50 nil 32)))
    (propertize formatted-preview 'face 'bibtex-actions-org-cite-style-preview)))

;;; Embark target finder

(defun bibtex-actions-org-cite-citation-finder ()
  "Return org-cite citation keys at point as a list for `embark'."
  (when-let ((keys (bibtex-actions-get-key-org-cite)))
    (cons 'oc-citation (bibtex-actions--stringify-keys keys))))

;;; Keymap

(defvar bibtex-actions-org-cite-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" '("cite | insert/edit" . org-cite-insert))
    (define-key map "o" '("cite | open-at-point" . bibtex-actions-open))
    (define-key map "e" '("cite | open/edit entry" . bibtex-actions-open-entry))
    (define-key map "n" '("cite | open notes" . bibtex-actions-open-notes))
    (define-key map (kbd "RET") '("cite | default action" . bibtex-actions-run-default-action))
    map)
  "Keymap for 'bibtex-actions-org-cite' `embark' at-point functionality.")

;; Bibtex-actions-org-cite configuration

(setq org-cite-follow-processor 'bibtex-actions-org-cite)
(setq org-cite-insert-processor 'bibtex-actions-org-cite)
(setq bibtex-actions-at-point-function 'embark-dwim)

;; Embark configuration for org-cite

(add-to-list 'embark-target-finders 'bibtex-actions-org-cite-citation-finder)
(add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
(add-to-list 'embark-keymap-alist '(oc-citation . bibtex-actions-org-cite-map))

(provide 'bibtex-actions-org-cite)
;;; bibtex-actions-org-cite.el ends here
