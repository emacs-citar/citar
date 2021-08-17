;;; bibtex-actions-utils.el --- Utility functions for bibtex-actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: August 15, 2021
;; Modified: August 15, 2021
;; Version: 0.1
;; Package-Requires: ((emacs "26.3"))
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
(require 'filenotify)
(require 'subr-x)

(declare-function bibtex-actions-refresh "bibtex-actions")
(declare-function bibtex-actions--local-files-to-cache "bibtex-actions")
(declare-function bibtex-actions--global-files-to-cache "bibtex-actions")
(declare-function reftex-access-scan-info "ext:reftex")
(declare-function file-notify-add-watch "ext:file-notify")
(declare-function file-notify-rm-watch "ext:file-notify")

(defvar bibtex-actions-bibliography)
(defvar bibtex-actions-library-paths)
(defvar bibtex-actions-notes-paths)
(defvar bibtex-actions-filenotify-files)
(defvar bibtex-actions--candidates-cache)
(defvar bibtex-actions--local-candidates-cache)
(defvar bibtex-actions-filenotify-callback)
(defvar bibtex-actions-open-file-function)
(defvar bibtex-actions-file-extensions)

(defun bibtex-actions-utils--stringify-keys (keys)
  "Return a list of KEYS as a crm-string for `embark'."
  (if (listp keys) (string-join keys " & ") keys))

;;; File handling

(defun bibtex-actions-utils--normalize-paths (file-paths)
  "Return a list of FILE-PATHS normalized with truename."
  (if (stringp file-paths)
      ;; If path is a string, return as a list.
      (list (file-truename file-paths))
    (delete-dups (mapcar (lambda (p) (file-truename p)) file-paths))))

(defun bibtex-actions-utils--files-for-key (key dirs extensions)
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

(defun bibtex-actions-utils--files-for-multiple-keys (keys dirs extensions)
  "Find files related to a list of KEYS in DIRS with extension in EXTENSIONS."
  (seq-mapcat
   (lambda (key)
     (bibtex-actions-utils--files-for-key key dirs extensions)) keys))

(cl-defun bibtex-actions-utils-open-files
    (keys dirs &optional &key create-note prompt external)
  "Open files related to KEYS in DIRS.

PROMPT with group-function.

EXTERNAL for external applications.

CREATE-NOTE if note not found."
  (let* ((files
          (bibtex-actions-utils--files-for-multiple-keys
           keys
           dirs
           bibtex-actions-file-extensions))
         (selected (when prompt
                     (completing-read "Files: " files))))
    (if selected
        (cl-loop for file in selected do
                 (funcall bibtex-actions-open-file-function file))
      (if files
          (cl-loop for file in files do
                   (cond (external
                          ;; Adapted from consult-file-externally.
                          (if (and (eq system-type 'windows-nt)
                                   (fboundp 'w32-shell-execute))
                              (w32-shell-execute "open" file)
                            (call-process (pcase system-type
                                            ('darwin "open")
                                            ('cygwin "cygstart")
                                            (_ "xdg-open"))
                                          nil 0 nil
                                          file)))
                         (create-note
                          (message "FIX THIS"))
                         (t (funcall bibtex-actions-open-file-function file))))
        (message "No file(s) found for %s" keys)))))


(provide 'bibtex-actions-utils)
;;; bibtex-actions-utils.el ends here
