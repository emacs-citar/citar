;;; bibtex-actions-select.el --- bibtex-actions comppletion functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: August 17, 2021
;; Modified: August 17, 2021
;; Version: 0.0.1
;; Homepage: https://github.com/bruce/bibtex-actions
;; Package-Requires: ((emacs "26.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun bibtex-actions-select-files (files)
  "Select file(s) from a list of FILES."
  (completing-read-multiple
   "Files: "
   (lambda (string predicate action)
     (if (eq action 'metadata)
         `(metadata
           (group-function . bibtex-actions-select-group-related-sources)
           (category . file))
       (complete-with-action action files string predicate)))))

(defun bibtex-actions-select-group-related-sources (file transform)
  "Group by FILE by source, TRANSFORM."
    (let ((extension (file-name-extension file)))
      (if transform file
        ;; Transform for grouping and group title display.
        (cond
         ((string= extension (or "org" "md")) "Notes")
          (t "Library Files")))))


(provide 'bibtex-actions-select)
;;; bibtex-actions-select.el ends here
