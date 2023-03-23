;; -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Setup completion style which is responsible for candidate filtering
(setq completion-styles '(orderless))

;; Enable Selectrum and Marginalia
(vertico-mode)
(marginalia-mode)

;; activate additional packages we need, including bibtex-actions
(require 'embark)
(require 'citar)
(require 'citar-embark)

(citar-embark-mode 1)

;; set binding for Embark context menu
(global-set-key (kbd "M-;") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)

;; replace CRM with consult alernative
;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

(with-eval-after-load "oc"
  (setq org-cite-follow-processor 'citar
        org-cite-insert-processor 'citar
        org-cite-activate-processor 'citar))

;; load the test bib file
(setq citar-bibliography '("../test.bib"))
(setq citar-notes-paths '("../"))

(setq vertico-count 20)

;; theme
(load-theme 'modus-operandi t)
