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
(require 'which-key)
(require 'consult)

;; set binding for Embark context menu
(global-set-key (kbd "M-o") #'embark-act)

;; replace CRM with consult alernative
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;; ensure that embark knows which map to use with bibtex entries
(add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))

;; load the test bib file
(setq bibtex-completion-bibliography "test.bib")

(setq selectrum-fix-vertical-window-height 20)

;; setup embark to use which-key
(setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)

;; sort which-key menu entries, so bibtex-actions commands first
(setq which-key-sort-order 'which-key-description-order)

;; theme
(load-theme 'modus-operandi t)

