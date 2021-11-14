;; -*- lexical-binding: t -*-
;; adapted from https://github.com/raxod502/selectrum/tree/master/test
;; Use a temporary emacs.d directory for testing
(setq user-emacs-directory "/tmp/ba-test-emacs.d/")

;; Setup package archive
(package-initialize)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-refresh-contents)

;; Install packages

(package-install 'load-relative)

;; completion system options
(package-install 'selectrum)
(package-install 'vertico)

;; completion style
(package-install 'orderless)
(package-install 'consult)
;; supporting packages for contextual commands
(package-install 'embark)
(package-install 'marginalia)

;; citar
;; we load this locally, to facilitate development testing on branches
(load-relative "../citar.el")
(load-relative "../citar-org.el")

;; theme that supports selectrum and vertico
(package-install 'modus-themes)

;; load full-screen, since this package exploits horizontal real estate
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(message "Installed packages to /tmp/citar-test-emacs.d/")
