;;; citar-vertico.el --- Keybinding remapping for vertico -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus
;;
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Remaps keybindings for using 'citar--multiple-select' with vertico.
;;
;;; Code:

(require 'vertico)

(defvar vertico--input)
(defvar vertico--history-hash)
(defvar vertico--lock-candidate)
(declare-function vertico--exhibit "ext:vertico")
(declare-function vertico--candidate "ext:vertico")

(defun citar-vertico--candidate ()
  "Return current candidate for Consult preview."
  (and vertico--input (vertico--candidate 'highlight)))

(defun citar-vertico--refresh (&optional reset)
  "Refresh completion UI, keep current candidate unless RESET is non-nil."
  (when vertico--input
    (setq vertico--input t)
    (when reset
      (setq vertico--history-hash nil
            vertico--lock-candidate nil))
    (vertico--exhibit)))

(defun citar-vertico--crm-select ()
  "Select/deselect candidate."
  (interactive)
  (when (let ((cand (vertico--candidate)))
          (and (vertico--match-p cand) (not (equal cand ""))))
    (vertico-exit)))

(defun citar-vertico--crm-exit ()
  "Select/deselect candidate and exit."
  (interactive)
  (when (let ((cand (vertico--candidate)))
          (and (vertico--match-p cand) (not (equal cand ""))))
    (run-at-time 0 nil #'exit-minibuffer))
  (vertico-exit))

(defvar citar-vertico--crm-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap vertico-insert] #'citar-vertico--crm-select)
    (define-key map [remap exit-minibuffer] #'citar-vertico--crm-exit)
    map))

(defun citar-vertico--crm-setup ()
  "Setup crm for Vertico."
  (when vertico--input
    (use-local-map (make-composed-keymap (list citar-vertico--crm-map) (current-local-map)))))

(add-hook 'citar--completion-candidate-hook #'citar-vertico--candidate)
(add-hook 'citar--completion-refresh-hook #'citar-vertico--refresh)
(add-hook 'citar--crm-setup-hook #'citar-vertico--crm-setup)

(provide 'citar-vertico)
;;; citar-vertico.el ends here
