;; -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Use a temporary emacs.d directory for testing
(setq citar--tmp-install-dir "/tmp/ba-test-emacs.d/")
(setq user-emacs-directory citar--tmp-install-dir)

;; Setup package archive
(package-initialize)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-refresh-contents)

;; Install packages

(package-install 'load-relative)
(package-install 'parsebib)

;; completion
(package-install 'vertico)

;; completion style
(package-install 'orderless)
(package-install 'consult)
;; supporting packages for contextual commands
(package-install 'embark)
(package-install 'marginalia)

;; Add binding for embark-act
(global-set-key (kbd "C-.") 'embark-act)

;; citar
;; Modify load path so that requires in citar.el are handled
(add-to-list 'load-path "../../")
;; we load this locally, to facilitate development testing on branches
(load-relative "../../citar.el")
(load-relative "../../citar-org.el")
(load-relative "../../citar-embark.el")

;; theme that supports selectrum and vertico
(package-install 'modus-themes)

;; load full-screen, since this package exploits horizontal real estate
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(message "Installed packages to %s" citar--tmp-install-dir)
