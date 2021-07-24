;; -*- lexical-binding: t -*-
;; adapted from https://github.com/raxod502/selectrum/tree/master/test
;; Setup completion style which is responsible for candidate filtering
(setq completion-styles '(orderless))

;; Enable Selectrum and Marginalia
(vertico-mode)
(marginalia-mode)

;; activate additional packages we need, including bibtex-actions
(require 'embark)
(require 'bibtex-actions)
(require 'oc-bibtex-actions)
(require 'consult)
;; set binding for Embark context menu
(global-set-key (kbd "M-;") #'embark-act)
(global-set-key (kbd "M-.") #'embark-dwim)

;; replace CRM with consult alernative
;(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;; ensure that embark knows which map to use with bibtex entries
(add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
(add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
(add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))

(with-eval-after-load "oc"
  (setq org-cite-follow-processor 'oc-bibtex-actions)
  (setq org-cite-insert-processor 'oc-bibtex-actions))

;; load the test bib file
(setq bibtex-completion-bibliography "test.bib")
;; set this so basic search functionality works correctly here
(setq bibtex-completion-additional-search-fields '(keywords url doi))

(setq vertico-count 20)

;; theme
(load-theme 'modus-operandi t)
