;;; citar-menu.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: November 26, 2021
;; Modified: November 26, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bdarcus/citar-menu
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;  Provides interactive functions the wrap citar functions, for use in 'hydra'
;;  and 'transient' menus.
;;
;;; Code:

(require 'citar)

(defun citar-menu-key-finder ()
  "Extract key string from reference at point."
  (cadr (citar-key-finder)))

(defun citar-menu-open-entry ()
  "A hydra interface to 'citar-open-entry'."
  (interactive)
  (citar--open-entry (citar-menu-key-finder)))

(defun citar-menu-open-note ()
    "A hydra interface to 'citar--open-note'."
  (interactive)
  (let* ((key (citar-menu-key-finder))
         (entry (citar--get-entry key)))
    (citar--open-note key entry)))

(defun citar-menu-open-library-file ()
  "A hydra interface to 'citar--library-file-action'."
  (interactive)
  (let* ((key (citar-menu-key-finder))
         (entry (citar--get-entry key)))
    (citar--library-files-action (list (cons key entry)) 'open)))

(defun citar-menu-open ()
    "A hydra interface to 'citar-open'."
  (interactive)
  (let* ((key (citar-menu-key-finder))
         (entry (citar--get-entry key)))
    (citar-open (list (cons key entry)))))

(provide 'citar-menu)
;;; citar-menu.el ends here
