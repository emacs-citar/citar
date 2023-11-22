;;; citar-embark.el --- Citar/Embark integration -*- lexical-binding: t; -*-
;;
;; Author: Bruce D'Arcus <bdarcus@gmail.com>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: June 22, 2022
;; Modified: June 22, 2022
;;
;; SPDX-FileCopyrightText: 2021-2022 Bruce D'Arcus
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Version: 1.0
;; Keywords: bib extensions
;; Homepage: https://github.com/emacs-citar/citar
;; Package-Requires: ((emacs "27.1") (embark "0.17") (citar "0.9.7"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides a minor-mode to integrate Citar and Embark.
;;
;;; Code:

(require 'citar)
(require 'embark)

;;;; Keymaps

(defvar citar-embark-map (make-composed-keymap citar-map embark-general-map)
  "Keymap for Embark actions on Citar references.")

;; TODO should this also inherit from `embark-general-map'?
(defvar citar-embark-citation-map (make-composed-keymap citar-citation-map nil)
  "Keymap for Embark actions on Citar citations and keys.")

;;;; Variables

(defvar citar-embark--target-finders
  (list #'citar-embark--key-finder
        #'citar-embark--citation-finder))

(defvar citar-embark--candidate-collectors
  (list #'citar-embark--selected))

(defvar citar-embark--transformer-alist
  (list (cons 'citar-candidate #'citar-embark--candidate-transformer)))

(defvar citar-embark--keymap-alist
  '((citar-reference . citar-embark-map)                                ; minibuffer candidates
    (citar-key . citar-embark-citation-map)                             ; at-point keys
    (citar-citation . citar-embark-citation-map)))                      ; at-point citations

(defvar citar-embark--multitarget-actions
  (list #'citar-open #'citar-open-files #'citar-attach-files #'citar-open-links
        #'citar-insert-bibtex #'citar-insert-citation #'citar-insert-reference
        #'citar-copy-reference #'citar-insert-keys #'citar-run-default-action
        #'citar-open-notes))

(defvar citar-embark--target-injection-hooks
  (list (list #'citar-insert-edit #'embark--ignore-target)))

;;;; At-point functions for Embark

(defun citar-embark--key-finder ()
  "Return the citation key at point."
  (when-let ((key (and (not (minibufferp)) (citar--key-at-point))))
    (cons 'citar-key key)))

(defun citar-embark--citation-finder ()
  "Return the keys of the citation at point."
  (when-let ((citation (and (not (minibufferp)) (citar--citation-at-point))))
    `(citar-citation ,(citar--stringify-keys (car citation)) . ,(cdr citation))))

(defun citar-embark--candidate-transformer (_type target)
  "Look up key for a citar-reference TYPE and TARGET."
  (or (get-text-property 0 'multi-category target)
      (cons 'citar-reference (citar--extract-candidate-citekey target))))

(defun citar-embark--selected ()
  "Return selected candidates from `citar--select-multiple' for embark."
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

;;;; Enable and disable Citar/Embark integration

(defun citar-embark--enable ()
  "Add Citar-specific functions and keymaps to Embark."
  (mapc (apply-partially #'add-hook 'embark-target-finders)
        (reverse citar-embark--target-finders))
  (mapc (apply-partially #'add-hook 'embark-candidate-collectors)
        (reverse citar-embark--candidate-collectors))
  (pcase-dolist (`(,type . ,transformer) citar-embark--transformer-alist)
    (setf (alist-get type embark-transformer-alist) transformer))
  (pcase-dolist (`(,type . ,keymap) citar-embark--keymap-alist)
    (setf (alist-get type embark-keymap-alist) keymap))
  (cl-callf cl-union embark-multitarget-actions citar-embark--multitarget-actions)
  (pcase-dolist (`(,action . ,hooks) citar-embark--target-injection-hooks)
    (cl-callf cl-union (alist-get action embark-target-injection-hooks) hooks)))

(defun citar-embark--disable ()
  "Undo the effects of `citar-embark--enable'."
  (mapc (apply-partially #'remove-hook 'embark-target-finders)
        citar-embark--target-finders)
  (mapc (apply-partially #'remove-hook 'embark-candidate-collectors)
        citar-embark--candidate-collectors)
  (cl-callf cl-set-difference embark-transformer-alist citar-embark--transformer-alist :test #'equal)
  (cl-callf cl-set-difference embark-keymap-alist citar-embark--keymap-alist :test #'equal)
  (cl-callf cl-set-difference embark-multitarget-actions citar-embark--multitarget-actions)
  (pcase-dolist (`(,action . ,hooks) citar-embark--target-injection-hooks)
    (when-let ((alistentry (assq action embark-target-injection-hooks)))
      (cl-callf cl-set-difference (cdr alistentry) hooks)
      (unless (cdr alistentry)          ; if no other hooks, remove alist entry
        (cl-callf2 remq alistentry embark-target-injection-hooks)))))

;;;###autoload
(define-minor-mode citar-embark-mode
  "Toggle integration between Citar and Embark."
  :group 'citar
  :global t
  :init-value nil
  :lighter " citar-embark"
  (if citar-embark-mode
      (citar-embark--enable)
    (citar-embark--disable)))

(provide 'citar-embark)
;;; citar-embark.el ends here
