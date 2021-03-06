;;; bibtex-actions.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus, Maxime Treca
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>, Maxime Treca <https://github.com/mtreca>
;; Created: February 27, 2021
;; Modified: March 3, 2021
;; Version: v0.0
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

;;; Keymap

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

;;; Completion functions

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

;;; Command wrappers for bibtex-completion functions

(defun bibtex-actions-open (keys)
 "Open PDF, or URL or DOI link.
Opens the PDF(s) associated with the KEYS.  If multiple PDFs are
found, ask for the one to open using ‘completing-read’.  If no
PDF is found, try to open a URL or DOI in the browser instead."
  (interactive (list (bibtex-actions--read)))
  (bibtex-completion-open-any (list keys)))

(defun bibtex-actions-open-pdf (keys)
 "Open PDF associated with the KEYS.
If multiple PDFs are found, ask for the one to open using
‘completing-read’."
  (interactive (list (bibtex-actions--read)))
  (bibtex-completion-open-pdf (list keys)))

(defun bibtex-actions-open-link (keys)
 "Open URL or DOI link associated with the KEYS in a browser."
 (interactive (list (bibtex-actions--read)))
 (bibtex-completion-open-url-or-doi (list keys)))

(defun bibtex-actions-insert-citation (keys)
 "Insert citation for the KEYS."
 (interactive (list (bibtex-actions--read)))
 (bibtex-completion-insert-citation (list keys)))

(defun bibtex-actions-insert-reference (keys)
 "Insert formatted reference(s) associated with the KEYS."
  (interactive (list (bibtex-actions--read)))
  (bibtex-completion-insert-reference (list keys)))

(defun bibtex-actions-insert-key (keys)
 "Insert BibTeX KEYS."
 (interactive (list (bibtex-actions--read)))
 (bibtex-completion-insert-key (list keys)))

(defun bibtex-actions-insert-bibtex (keys)
 "Insert BibTeX entry associated with the KEYS."
 (interactive (list (bibtex-actions--read)))
 (bibtex-completion-insert-bibtex (list keys)))

(defun bibtex-actions-add-PDF-attachment (keys)
 "Attach PDF(s) associated with the KEYS to email."
 (interactive (list (bibtex-actions--read)))
 (bibtex-completion-add-PDF-attachment (list keys)))

(defun bibtex-actions-open-notes (keys)
 "Open notes associated with the KEYS."
 (interactive (list (bibtex-actions--read)))
 (bibtex-completion-edit-notes (list keys)))

(defun bibtex-actions-open-entry (keys)
 "Open BibTeX entry associated with the KEYS."
 (interactive (list (bibtex-actions--read)))
 (bibtex-completion-show-entry (list keys)))

(defun bibtex-actions-add-pdf-to-library (keys)
 "Add PDF associated with the KEYS to library.
The PDF can be added either from an open buffer, a file, or a
URL."
  (interactive (list (bibtex-actions--read)))
  (bibtex-completion-add-pdf-to-library (list keys)))

(provide 'bibtex-actions)
;;; bibtex-actions.el ends here
