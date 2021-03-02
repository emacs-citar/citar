;;; bibtex-actions.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus, Maxime Treca
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Author: Maxime Treca <https://github.com/mtreca>
;; Created: February 27, 2021
;; Modified: February 27, 2021
;; Version: 0.0.1
;; Keywords: bibtex, citations, bibliographies, academic
;; Homepage: https://github.com/bdarcus/bibtex-actions
;; Package-Requires: ((emacs "27.1") (bibtex-completion "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'bibtex-completion)

;; one can reset the backend function using fset; maybe there's a
;; more elegant way to do this?
(fset 'bibtex-actions-read-backend `bibtex-actions--completing-read)

(defun bibtex-actions--read ()
  "Select BibTeX entries in completion system."
  ;; define a completion function that defaults to completing-read, but can be overridden
  (list (bibtex-actions-read-backend)))

(defun bibtex-actions--completing-read ()
  "Read bibtex-completion entries for completion using completing-read."
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
  "Return all keys from bibtex-completion-candidates."
  (cl-loop
   for candidate in (bibtex-completion-candidates)
   collect
   (cons
    ;; Here use one string for display, and the other for search.
    (propertize
     (car candidate) 'display (bibtex-completion-format-entry candidate (1- (frame-width))))
    (cdr (assoc "=key=" candidate)))))

(provide 'bibtex-actions)
;;; bibtex-actions.el ends here
