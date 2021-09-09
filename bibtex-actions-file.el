;;; bibtex-actions-file.el --- File functions for bibtex-actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: August 17, 2021
;; Version: 0.4
;; Homepage: https://github.com/bdarcus/bibtex-actions
;; Package-Requires: ((emacs "26.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Functions for opening and creating bibliographic files related to a source.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'seq)
(require 'org-id)

(declare-function bibtex-actions-get-entry "bibtex-actions")
(declare-function bibtex-actions-get-value "bibtex-actions")
(declare-function bibtex-actions-get-template "bibtex-actions")
(declare-function bibtex-actions--format-entry-no-widths "bibtex-actions")

;;;; File related variables

(defcustom  bibtex-actions-file-variable "file"
  "The field key to look for in an entry for PDF, etc."
  :group 'bibtex-actions
  :type '(string))

(defcustom bibtex-actions-file-open-function 'find-file
  "Function to open a file."
  :group 'bibtex-actions
  :type '(function))

(defcustom bibtex-actions-file-open-prompt nil
  "Prompt for selection of related files to open."
  :group 'bibtex-actions
  :type '(boolean))

(defcustom bibtex-actions-file-open-note-function
  'bibtex-actions-file-open-notes-default-org
  "Function to open and existing or create a new note.

If you use 'org-roam' and 'org-roam-bibtex', for example, you can
use 'orb-edit-note' for this value."
  :group 'bibtex-actions
  :type '(function))

(defcustom bibtex-actions-file-parser-function
  'bibtex-actions-file-parser-default
  "Function to parse file field."
  :group 'bibtex-actions
  :type '(function))

(defcustom bibtex-actions-file-extensions '("pdf" "org" "md")
  "A list of file extensions to recognize for related files."
  :group 'bibtex-actions
  :type '(repeat string))

(defvar bibtex-actions-notes-paths)

;;;; Convenience functions for files and paths

(defun bibtex-actions-file--normalize-paths (file-paths)
  "Return a list of FILE-PATHS normalized with truename."
  (if (stringp file-paths)
      ;; If path is a string, return as a list.
      (list (file-truename file-paths))
    (delete-dups
     (mapcar
      (lambda (p) (file-truename p)) file-paths))))

(defun bibtex-actions-file-parser-default (dirs file-field)
  "Return a list of files from base directories DIRS and FILE-FIELD."
  (seq-map (lambda (dir) (expand-file-name file-field dir)) dirs))

(defun bibtex-actions-file-parser-zotero (dirs file-field)
  "Return a list of files from DIRS and a Zotero formatted FILE-FIELD."
  (let ((files (split-string file-field ";")))
    (seq-mapcat
     (lambda (dir)
       (mapcar
        (lambda (file)
          (expand-file-name file dir)) files))
     dirs)))

(defun bibtex-actions-file--possible-names (key dirs extensions)
  "Possible names for files correponding to KEY with EXTENSIONS in DIRS."
  (cl-flet ((possible-file-names-with-extension
             (extension)
             (seq-map
              (lambda (directory)
                (expand-file-name
                 (concat key "." extension) directory))
              dirs)))
    (let* ((results-key (seq-mapcat
                         #'possible-file-names-with-extension
                         extensions))
           (entry (bibtex-actions-get-entry key))
           (file-field (bibtex-actions-get-value
                        bibtex-actions-file-variable entry))
           (results-file
            (when file-field (funcall bibtex-actions-file-parser-function dirs file-field))))
      (append results-key results-file))))

(defun bibtex-actions-file--files-for-key (key dirs extensions)
    "Find files related to KEY in DIRS with extension in EXTENSIONS."
    (seq-filter #'file-exists-p
                (bibtex-actions-file--possible-names key dirs extensions)))

(defun bibtex-actions-file--files-to-open-or-create (keys dirs extensions)
  "Find files related to a list of KEYS in DIRS with extension in EXTENSIONS."
  (cl-flet ((files-for-key
             (key)
             (let* ((possible-files
                     (bibtex-actions-file--possible-names key dirs extensions))
                    (existing-files
                     (seq-filter #'file-exists-p possible-files)))
               (if existing-files
                   (seq-map
                    (lambda (file) (cons file 'exists))
                    existing-files)
                 (seq-map
                  (lambda (file) (cons file 'new))
                  possible-files)))))
    (seq-mapcat #'files-for-key keys)))


(defun bibtex-actions-file--files-for-multiple-keys (keys dirs extensions)
  "Find files related to a list of KEYS in DIRS with extension in EXTENSIONS."
  (seq-mapcat
   (lambda (key)
     (bibtex-actions-file--files-for-key key dirs extensions)) keys))

;;;; Opening and creating files functions

(defun bibtex-actions-file-open (file)
  "Open FILE."
  (funcall bibtex-actions-file-open-function file))

(defun bibtex-actions-file-open-external (file)
  "Open FILE with external application."
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

(defun bibtex-actions-file-open-notes-default-org (key)
  "Open a note file from KEY."
  (if-let* ((file
             (caar (bibtex-actions-file--files-to-open-or-create
                    (list key)
                    bibtex-actions-notes-paths '("org"))))
            (file-exists (file-exists-p file)))
      (funcall bibtex-actions-file-open-function file)
    (let* ((uuid (org-id-new))
           (entry (bibtex-actions-get-entry key))
           (note-meta
            (bibtex-actions--format-entry-no-widths
             entry
             (bibtex-actions-get-template 'note)))
           (content
            (concat ":PROPERTIES:\n:ID:  " uuid "\n:END:\n" note-meta "\n")))
      (funcall bibtex-actions-file-open-function file)
      (insert content))))

(provide 'bibtex-actions-file)
;;; bibtex-actions-file.el ends here
