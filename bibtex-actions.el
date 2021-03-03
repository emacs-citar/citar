;;; bibtex-actions.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus, Maxime Treca
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Author: Maxime Treca <https://github.com/mtreca>
;; Created: February 27, 2021
;; Modified: March 3, 2021
;; Version: 0.0.2
;; Keywords: bib, files
;; Homepage: https://github.com/bdarcus/bibtex-actions
;; Package-Requires: ((emacs "26.3") (bibtex-completion "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'bibtex-completion)

(defvar bibtex-actions-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'bibtex-actions-open-any)
    (define-key map (kbd "p") 'bibtex-actions-open-pdf)
    (define-key map (kbd "u") 'bibtex-actions-open-url-or-doi)
    (define-key map (kbd "c") 'bibtex-actions-insert-citation)
    (define-key map (kbd "r") 'bibtex-actions-insert-reference)
    (define-key map (kbd "k") 'bibtex-actions-insert-key)
    (define-key map (kbd "b") 'bibtex-actions-insert-bibtex)
    (define-key map (kbd "a") 'bibtex-actions-add-pdf-attachment)
    (define-key map (kbd "e") 'bibtex-actions-edit-notes)
    (define-key map (kbd "s") 'bibtex-actions-show-entry)
    (define-key map (kbd "l") 'bibtex-actions-add-pdf-to-library)
    map)
  "Keymap for 'bibtex-actions'.")

;; one can reset the backend function using fset; maybe there's a
;; more elegant way to do this?
(fset 'bibtex-actions-read-backend `bibtex-actions--completing-read)

(defun bibtex-actions--read ()
  "Select BibTeX entries in completion system."
  ;; define a completion function that defaults to completing-read, but can be overridden
  (bibtex-actions-read-backend))

(defun bibtex-actions--completing-read ()
  "Read bibtex-completion entries for completion using 'completing-read'."
  (bibtex-completion-init)
  (when-let ((candidates (bibtex-actions--get-candidates))
             (chosen
              (completing-read
               "BibTeX entries: "
               (lambda (string predicate action)
                 (if (eq action 'metadata)
                     '(metadata
                       ;; (annotation-function . bibtex-completion--annotation)
                       (category . bibtex))
                   (complete-with-action action candidates string predicate))))))
    (cdr (assoc chosen candidates))))

(defun bibtex-actions--get-candidates ()
  "Return all keys from 'bibtex-completion-candidates'."
  (cl-loop
   for candidate in (bibtex-completion-candidates)
   collect
   (cons
    ;; Here use one string for display, and the other for search.
    (propertize
     (car candidate) 'display (bibtex-completion-format-entry candidate (1- (frame-width))))
    (cdr (assoc "=key=" candidate)))))

(defmacro bibtex-actions-define-action (action doc)
  "A macro to create commands from bibtex-completion functions.
It takes the ACTION and the DOC to create another function named
NAME which extracts the keys from the selected candidates and
passes them to ACTION."
  (let* ((old-name (symbol-name action))
         (mid-name (substring old-name 17 (length old-name)))
         (new-name (intern (downcase (concat "bibtex-actions" mid-name)))))
    `(defun ,new-name (cand)
       ,doc
       (interactive (list (bibtex-actions--read)))
       (,action (list cand)))))

(bibtex-actions-define-action
 bibtex-completion-open-any
 "Open the PDFs associated with the BibTeX entry.
If multiple PDFs are found, ask for the one to open using
‘completing-read’. If no PDF is found, try to open a URL or DOI
in the browser instead.")

(bibtex-actions-define-action
 bibtex-completion-open-pdf
 "Open the PDFs associated with the BibTeX entry.
If multiple PDFs are found, ask for the one to open using
‘completing-read’.")

(bibtex-actions-define-action
 bibtex-completion-open-url-or-doi
 "Open the URL or DOI associated with a BibTeX entry in a
 browser.")

(bibtex-actions-define-action
 bibtex-completion-insert-citation
 "Insert citation for BibTeX entry at point.")

(bibtex-actions-define-action
 bibtex-completion-insert-reference
 "Insert reference for BibTeX entry at point.")

(bibtex-actions-define-action
 bibtex-completion-insert-key
 "Insert key for BibTeX entry at point.")

(bibtex-actions-define-action
 bibtex-completion-insert-bibtex
 "Insert entry for BibTeX entry at point.")

(bibtex-actions-define-action
 bibtex-completion-add-PDF-attachment
 "Attach the PDF of a BibTeX entry where available.")

(bibtex-actions-define-action
 bibtex-completion-edit-notes
 "Open the notes associated with a BibTeX entry using
 ‘bibtex-completion-edit-notes-function’.")

(bibtex-actions-define-action
 bibtex-completion-show-entry
 "Show the selected entry in the relevant BibTeX file.")

(bibtex-actions-define-action
 bibtex-completion-add-pdf-to-library
 "Add a PDF to the library for the selected BibTeX entry.
The PDF can be added either from an open buffer, a file, or a
URL.")


(provide 'bibtex-actions)
;;; bibtex-actions.el ends here
