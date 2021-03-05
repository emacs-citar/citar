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
;;  This package turns bibtex-completion functions into completing-read-based
;;  Emacs commands.  When used with selectrum/icomplete-vertical, embark, and
;;  marginalia, it provides similar functionality to helm-bibtex and ivy-bibtex:
;;  quick filtering and selecting of bibliographic entries from the minibuffer,
;;  and the option to run different commands against them.
;;
;;; Code:

(require 'bibtex-completion)

(defvar bibtex-actions-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'bibtex-actions-open)
    (define-key map (kbd "p") 'bibtex-actions-open-pdf)
    (define-key map (kbd "u") 'bibtex-actions-open-link)
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

(defmacro bibtex-actions-define-action (action doc &optional alt-name)
  "A macro to create commands from bibtex-completion functions.
It takes the ACTION and the DOC to create another function named
NAME which extracts the keys from the selected candidates and
passes them to ACTION.  Where ALT-NAME is present, use that instead."
  (let* ((old-name (symbol-name action))
         (mid-name (substring old-name 17 (length old-name)))
         (new-name (intern (concat "bibtex-actions" mid-name)))
         (name (or alt-name new-name)))
    `(defun ,name (cand)
       ,doc
       (interactive (list (bibtex-actions--read)))
       (,action (list cand)))))

(bibtex-actions-define-action
 bibtex-completion-open-any
 "Open PDF, or URL or DOI link.
If multiple PDFs are found, ask for the one to open using
‘completing-read’. If no PDF is found, try to open a URL or DOI
in the browser instead."
 bibtex-actions-open)

(bibtex-actions-define-action
 bibtex-completion-open-pdf
 "Open the PDF.
If multiple PDFs are found, ask for the one to open using
‘completing-read’.")

(bibtex-actions-define-action
 bibtex-completion-open-url-or-doi
 "Open the URL or DOI in a browser."
 bibtex-actions-open-link)

(bibtex-actions-define-action
 bibtex-completion-insert-citation
 "Insert citation.")

(bibtex-actions-define-action
 bibtex-completion-insert-reference
 "Insert reference.")

(bibtex-actions-define-action
 bibtex-completion-insert-key
 "Insert key.")

(bibtex-actions-define-action
 bibtex-completion-insert-bibtex
 "Insert entry.")

(bibtex-actions-define-action
 bibtex-completion-add-PDF-attachment
 "Attach PDF to email."
 bibtex-actions-add-pdf-attachment)

(bibtex-actions-define-action
 bibtex-completion-edit-notes
 "Open notes."
 bibtex-actions-open-notes)

(bibtex-actions-define-action
 bibtex-completion-show-entry
 "Open entry."
 bibtex-actions-open-entry)

(bibtex-actions-define-action
 bibtex-completion-add-pdf-to-library
 "Add a PDF to library.
The PDF can be added either from an open buffer, a file, or a
URL.")


(provide 'bibtex-actions)
;;; bibtex-actions.el ends here
