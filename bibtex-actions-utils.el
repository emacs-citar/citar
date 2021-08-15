;;; bibtex-actions-utils.el --- bibtex-actions convenience functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: August 15, 2021
;; Modified: August 15, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/bruce/bibtex-actions-utils
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Convenience functions for 'bibtex-actions'.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'files)

(defun bibtex-actions--normalize-paths (file-paths)
  "Return a list of FILE-PATHS normalized with truename."
  (if (stringp file-paths)
      ;; If path is a string, return as a list.
      (list (file-truename file-paths))
    (delete-dups (mapcar (lambda (p) (file-truename p)) file-paths))))

(defun bibtex-actions--files-for-key (key dirs extensions)
  "Find files related to KEY in DIRS with extension in EXTENSIONS."
  (cl-flet ((possible-file-names-with-extension
             (extension)
             (seq-map
              (lambda (directory)
                (expand-file-name
                 (concat key "." extension) directory))
              dirs)))
    (seq-filter #'file-exists-p
                (seq-mapcat #'possible-file-names-with-extension
                            extensions))))

(provide 'bibtex-actions-utils)
;;; bibtex-actions-utils.el ends here
