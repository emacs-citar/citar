;;; oc-bibtex-insert.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bruce>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: July 27, 2021
;; Modified: July 27, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/bruce/oc-bibtex-insert
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'bibtex-actions)
(require 'citeproc)

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


;;;; Insert-keys

(defun oc-bibtex-actions-insert (&optional multiple)
  "Return a list of keys when MULTIPLE, or else a key string."
  (let ((references (bibtex-actions-read)))
    (if multiple
        references
      (car references))))

;;;; Insert-style

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

(org-cite-register-processor 'oc-bibtex-actions-insert
  :insert (org-cite-make-insert-processor
           #'oc-bibtex-actions-insert
         ;  #'org-cite-basic--complete-style)
           #'oc-bibtex-actions-select-style))

(provide 'oc-bibtex-insert)
;;; oc-bibtex-insert.el ends here
