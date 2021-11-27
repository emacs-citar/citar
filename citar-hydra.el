;;; citar-hydra.el --- Pretty hydra menu for citar -*- lexical-binding: t; -*-
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
;;  A 'transient' menu org-cite-follow processor for 'citar'.
;;
;;  Also works in markdown and latex.
;;
;;  Provides an alternative to embark at point.
;;
;;; Code:
(require 'org-element)
(require 'pretty-hydra)
(require 'oc)
(require 'citar-menu)
(require 'citar-org)

(declare-function 'org-cite-register-processor "ext:oc")

(pretty-hydra-define citar-hydra-menu
  (:title "Reference" :color teal :quit-key "q" :idle 0.5)
  ("Open Related"
   (("o" citar-menu-open "files or links")
    ("f" citar-menu-open-library-file "library files")
    ("n" citar-menu-open-note "notes")
    ("e" citar-menu-open-entry "BibTeX entry"))

   "Edit"
   (("i" citar-insert-edit "insert or edit")
    ("d" citar-org-delete-citation "delete" :color red))))

;;;###autoload
(defun citar-hydra-follow (datum _)
  "Org-cite-follow function to use a transient menu for citar.
DATUM is the org element."
  (when (null datum) (setq datum (org-element-context)))
  (if (eq 'citation-reference (org-element-type datum))
      (citar-hydra-menu/body)))

;;;###autoload
(org-cite-register-processor 'citar-hydra
  :follow #'citar-hydra-follow)

(provide 'citar-hydra)
;;; citar-hydra.el ends here
