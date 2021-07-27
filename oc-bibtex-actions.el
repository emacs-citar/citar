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
(require 'oc-bibtex-actions-insert)
(require 'org)
(require 'oc)
(require 'oc-basic)
(require 'oc-csl)
(require 'embark)

(declare-function bibtex-actions-at-point "bibtex-actions")
(declare-function org-open-at-point "org")
(declare-function org-cite-make-insert-processor "oc")



;TODO
;(defvar oc-bibtex-actions-open-default

;;; Org-cite processors

;; NOTE I may move some or all of these to a separate project

(defun oc-bibtex-actions-follow (_datum _arg)
  "Follow processor for org-cite."
  (call-interactively bibtex-actions-at-point-function))

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

;; Load this last.

(org-cite-register-processor 'oc-bibtex-actions
  :insert (org-cite-make-insert-processor
           #'oc-bibtex-actions-insert
         ;  #'org-cite-basic--complete-style)
           #'oc-bibtex-actions-select-style)
  :follow #'oc-bibtex-actions-follow)

(provide 'oc-bibtex-actions)
;;; oc-bibtex-actions.el ends here
