;;; citar-embark.el --- Citar/Embark integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <bdarcus@gmail.com>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: June 22, 2022
;; Modified: June 22, 2022
;; Version: 1.0
;; Keywords: bib extensions
;; Homepage: https://github.com/emacs-citar/citar-embark
;; Package-Requires: ((emacs "27.2") (embark "0.17") (citar "0.9.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'citar)
(require 'embark)

(declare-function 'citar--key-finder "citar")
;;; variables

(defvar citar-embark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'citar-insert-citation)
    (define-key map (kbd "k") #'citar-insert-keys)
    (define-key map (kbd "r") #'citar-copy-reference)
    (define-key map (kbd "R") #'citar-insert-reference)
    (define-key map (kbd "b") #'citar-insert-bibtex)
    (define-key map (kbd "o") #'citar-open)
    (define-key map (kbd "e") #'citar-open-entry)
    (define-key map (kbd "l") #'citar-open-link)
    (define-key map (kbd "n") #'citar-open-notes)
    (define-key map (kbd "f") #'citar-open-library-file)
    (define-key map (kbd "RET") #'citar-run-default-action)
    map)
  "Keymap for Embark minibuffer actions.")

(defvar citar-embark-citation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'citar-insert-edit)
    (define-key map (kbd "o") #'citar-open)
    (define-key map (kbd "e") #'citar-open-entry)
    (define-key map (kbd "l") #'citar-open-link)
    (define-key map (kbd "n") #'citar-open-notes)
    (define-key map (kbd "f") #'citar-open-library-file)
    (define-key map (kbd "r") #'citar-copy-reference)
    (define-key map (kbd "RET") #'citar-run-default-action)
    map)
  "Keymap for Embark citation-key actions.")

;;; functions

(defalias 'citar-embark-key-finder 'citar--key-finder
  "Return key-at-point target for Embark.")

(defun citar-embark--reference-transformer (type target)
  "Look up key for a citar-reference TYPE and TARGET."
  (cons type target))

(defun citar-embark--selected ()
  "Return selected candidates from `citar--select-multiple' for embark."
  ;; REVIEW does this work?
  (when-let (((eq minibuffer-history-variable 'citar-history))
             (metadata (embark--metadata))
             (group-function (completion-metadata-get metadata 'group-function))
             (cands (all-completions
                     "" minibuffer-completion-table
                     (lambda (cand)
                       (and (equal "Selected" (funcall group-function cand nil))
                            (or (not minibuffer-completion-predicate)
                                (funcall minibuffer-completion-predicate cand)))))))
    (cons (completion-metadata-get metadata 'category) cands)))

;;; minor mode

(defun citar-embark-setup ()
  "Setup 'citar-embark-mode'."
  (set-keymap-parent citar-embark-map embark-general-map)
  (dolist (target-finder '(citar-citation-finder citar-embark-key-finder))
    (add-to-list 'embark-target-finders target-finder))

  (add-to-list 'embark-transformer-alist
               '(citar-reference . citar-embark--reference-transformer))
  (add-to-list 'embark-candidate-collectors #'citar-embark--selected)

  (dolist (keymap-cons
           '((citar-reference . citar-embark-map)
             (citar-key . citar-embark-citation-map)
             (citar-citation . citar-embark-citation-map)))
    (add-to-list 'embark-keymap-alist keymap-cons))

  (add-to-list 'embark-target-injection-hooks
               '(citar-insert-edit embark--ignore-target))

  (dolist (command (list #'citar-insert-bibtex #'citar-insert-citation
                         #'citar-insert-reference #'citar-copy-reference
                         #'citar-insert-keys #'citar-run-default-action))
    (add-to-list 'embark-multitarget-actions command)))

(defun citar-embark-reset ()
  "Reset 'citar-embark-mode' to default."
  ;; TODO
  (delete 'citar-embark-key-finder embark-target-finders))

;;;###autoload
(define-minor-mode citar-embark-mode
  "Toggle citar-embark-mode."
  :global t
  :group 'citar
  :lighter " citar-embark"
  (if citar-embark-mode (citar-embark-setup)
    (citar-embark-reset)))

(provide 'citar-embark)
;;; citar-embark.el ends here
