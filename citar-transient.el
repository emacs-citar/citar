;;; citar-transient.el --- Transient menu for citar -*- lexical-binding: t; -*-
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
(require 'transient)
(require 'oc)
(require 'citar-menu)
(require 'citar-org)

(declare-function 'org-cite-register-processor "ext:oc")

;;;###autoload
(transient-define-prefix citar-transient-menu ()
  ["Reference(s)\n"
   ["Open"
    ("f" "library files" citar-menu-open-library-file)
    ("e" "BibTeX entry" citar-menu-open-entry)
    ("o" "files or links" citar-menu-open)]

   ["Edit"
    ("i" "Insert or edit" citar-insert-edit)
    ("d" "Delete key/citation" citar-org-delete-citation)]])

;;;###autoload
(defun citar-transient-follow (datum _)
  "Org-cite-follow function to use a transient menu for citar.
DATUM is the org element."
  (when (null datum) (setq datum (org-element-context)))
  (if (eq 'citation-reference (org-element-type datum))
      (citar-transient-menu)))

;;;###autoload
(org-cite-register-processor 'citar-transient
  :follow #'citar-transient-follow)

(provide 'citar-transient)
;;; citar-transient.el ends here
