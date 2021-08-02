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
(declare-function org-cite-make-insert-processor "oc")

(defface bibtex-actions-org-cite-style-preview
  ;; Not sure if this is the best parent face.
    '((t :inherit minibuffer-prompt))
  "Face for org-cite previews."
  :group 'oc-bibtex-actions)

(defcustom oc-bibtex-actions-preview-target 'natbib
  "Export processor target for which to display previews."
  ;; REVIEW not sure this is the best approach.
  :group 'oc-bibtex-actions
  :type '(choice
          (const biblatex)
          (const csl)
          (const natbib)))

;;; Internal variables

(defvar oc-bibtex-actions--csl-processor-cache nil
  "Cache for the citation preview processor.")

(make-variable-buffer-local 'bibtex-actions--csl-processor-cache)

(defun oc-bibtex-actions--csl-processor ()
  "Return a `citeproc-el' processor for style preview."
  (or oc-bibtex-actions--csl-processor-cache
      (let* ((bibliography (org-cite-list-bibliography-files))
             (processor
              (citeproc-create
               org-cite-csl--fallback-style-file
               (org-cite-csl--itemgetter bibliography)
               (org-cite-csl--locale-getter))))
        (setq oc-bibtex-actions--csl-processor-cache processor)
        processor)))

;; TODO maybe connvert to defcustoms. But this is not really the right approach;
;; better to just run the export processors to get the previews. But we need
;; citation context for that, or some other solution to have a citation to
;; process.

(defvar oc-bibtex-actions-style-preview-alist
  '((natbib .
            (;; Default style.
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
    (biblatex .
              ( ;; Default style.
               ("/" . "\\autocite")
               ("/b" . "\\cite")
               ("/c" . "\\Autocite")
               ("/bc" . "\\Cite")
               ;; "text" style.
               ("text" . "\\textcite")
               ("text/c" .  "\\Textcite")
               ;; "nocite" style.
               ("nocite" . "\\nocite")
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
    (csl .
         (
          ("/" . "(de Loas, 2019)")
          ("/c" . "(De Loas, 2019")
          ("text" . "de Loas (2019)")
          ("text" ' "De Loas (2019)")))))

;TODO
;(defvar oc-bibtex-actions-open-default

;;; Org-cite processors

;; NOTE I may move some or all of these to a separate project

(defun oc-bibtex-actions-insert (&optional multiple)
  "Return a list of keys when MULTIPLE, or else a key string."
  (let ((references (bibtex-actions-read)))
    (if multiple
        references
      (car references))))

(defun oc-bibtex-actions-follow (_datum _arg)
  "Follow processor for org-cite."
  (call-interactively bibtex-actions-at-point-function))

(defun oc-bibtex-actions-select-style ()
"Complete a citation style for org-cite with preview."
  (interactive)
  (let* ((oc-styles (oc-bibtex-actions--styles-candidates))
         (style
          (completing-read
           "Styles: "
           (lambda (str pred action)
             (if (eq action 'metadata)
                 `(metadata
                   (annotation-function . oc-bibtex-actions--style-preview-annote)
                   (cycle-sort-function . identity)
                   (display-sort-function . identity)
                   (group-function . oc-bibtex-actions--styles-group-fn))
               (complete-with-action action oc-styles str pred)))))
         (style-final (string-trim style)))
    (if (string= style-final "/") "" style-final)))

(defun oc-bibtex-actions--styles-candidates ()
  "Generate candidate list."
  ;; TODO extract the style+variant strings from 'org-cite-support-styles'.
  (cl-loop for style in
           (cdr (assoc oc-bibtex-actions-preview-target
                        oc-bibtex-actions-style-preview-alist))
           collect (cons
                    (concat "  " (truncate-string-to-width (car style) 20 nil 32)) (cdr style))))

(defun oc-bibtex-actions--styles-group-fn (style transform)
  "Return group title of STYLE or TRANSFORM the candidate.
This is a group-function that groups org-cite style/variant
strings by style."
    (let* ((style-str (string-trim style))
           (short-style
            (if (string-match "^/[bcf]*" style-str) "default"
              (car (split-string style-str "/")))))
    (if transform
        ;; Use the candidate string as is, but add back whitespace alignment.
        (concat "  " (truncate-string-to-width style-str 20 nil 32))
      ;; Transform for grouping and display.
      (cond
       ((string= short-style "default") "Default")
       ((string= short-style "author") "Author-Only")
       ((string= short-style "locators") "Locators-Only")
       ((string= short-style "text") "Textual/Narrative")
       ((string= short-style "nocite") "No Cite")
       ((string= short-style "noauthor") "Suppress Author")))))

(defun oc-bibtex-actions-csl-render-citation (citation)
  "Render CITATION."
  ;; TODO hook this up to previews.
  (let ((proc (oc-bibtex-actions--csl-processor)))
    (citeproc-clear proc)
    (let* ((info (list :cite-citeproc-processor proc))
	   (cit-struct (org-cite-csl--create-structure citation info)))
      (citeproc-append-citations (list cit-struct) proc)
      (car (citeproc-render-citations proc 'plain t)))))

(defun oc-bibtex-actions--style-preview-annote (style &optional _citation)
  "Annotate STYLE with CITATION preview."
  ;; TODO rather than use the alist, run the export processors on the citation..
  (let* ((preview (cdr (assoc style (oc-bibtex-actions--styles-candidates))))
         ;; TODO look at how define-face does this.
         (formatted-preview (truncate-string-to-width preview 50 nil 32)))
    (propertize formatted-preview 'face 'oc-bibtex-actions-style-preview)))

;;; Embark target finder

(defun oc-bibtex-actions-citation-finder ()
  "Return org-cite citation keys at point as a list for `embark'."
  (when-let ((keys (bibtex-actions-get-key-org-cite)))
    (cons 'oc-citation (bibtex-actions--stringify-keys keys))))

;;; Keymap

(defvar oc-bibtex-actions-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" '("edit citation" . org-cite-insert))
    (define-key map (kbd "o") '("open source document" . bibtex-actions-open))
    (define-key map (kbd "e") '("open bibtex entry" . bibtex-actions-open-entry))
    (define-key map (kbd "l") '("open source URL or DOI" . bibtex-actions-open-link))
    (define-key map (kbd "n") '("open notes" . bibtex-actions-open-notes))
    (define-key map (kbd "p") '("open PDF" . bibtex-actions-open-pdf))
    (define-key map (kbd "r") '("refresh library" . bibtex-actions-refresh))
    (define-key map (kbd "RET") '("default action" . bibtex-actions-run-default-action))
    map)
  "Keymap for 'oc-bibtex-actions' `embark' at-point functionality.")

;; Embark configuration for org-cite

(add-to-list 'embark-target-finders 'oc-bibtex-actions-citation-finder)
(add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
(add-to-list 'embark-keymap-alist '(oc-citation . oc-bibtex-actions-map))
(when (boundp 'embark-pre-action-hooks)
  ;; Ensure that Embark ignores the target for 'org-cite-insert'.
  (add-to-list 'embark-pre-action-hooks '(org-cite-insert embark--ignore-target)))

;; Load this last.

(org-cite-register-processor 'oc-bibtex-actions
  :insert (org-cite-make-insert-processor
           #'oc-bibtex-actions-insert
         ;  #'org-cite-basic--complete-style)
           #'oc-bibtex-actions-select-style)
  :follow #'oc-bibtex-actions-follow)

(provide 'oc-bibtex-actions)
;;; oc-bibtex-actions.el ends here
