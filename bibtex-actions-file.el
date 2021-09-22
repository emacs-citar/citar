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

A note function must take two arguments:

KEY: a string to represent the citekey
ENTRY: an alist with the structured data (title, author, etc.)

If you use 'org-roam' and 'org-roam-bibtex', you can use
'orb-bibtex-actions-edit-note' for this value."
  :group 'bibtex-actions
  :type '(function))

(defcustom bibtex-actions-file-parser-functions
  '(bibtex-actions-file-parser-default)
  "List of functions to parse file field."
  :group 'bibtex-actions
  :type '(repeat function))

(defcustom bibtex-actions-file-open-function 'find-file
  "Function to use to open files."
  :group 'bibtex-actions
  :type '(function))

(defcustom bibtex-actions-file-extensions '("pdf" "org" "md")
  "List of file extensions to recognize for related files.

These are the extensions the 'bibtex-actions-file-open-function'
will open, via `bibtex-actions-file-open'."
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
  "Return a list of files from DIRS and FILE-FIELD."
  (let ((files (split-string file-field ";")))
    (seq-mapcat
     (lambda (dir)
       (mapcar
        (lambda (file)
          (expand-file-name file dir)) files))
     dirs)))

(defun bibtex-actions-file-parser-triplet (dirs file-field)
  "Return a list of files from DIRS and a FILE-FIELD formatted as a triplet.

Example: ':/path/to/test.pdf:PDF'."
  (let ((parts (split-string file-field "[,;]" 'omit-nulls)))
    (seq-mapcat
     (lambda (part)
       (when (string-match ":\\(.*\\):.*" part)
	 (let ((fn (replace-regexp-in-string "\\\\:" ":" (match-string 1 part))))
           (mapcar (apply-partially #'expand-file-name fn) dirs))))
     parts)))

(defun bibtex-actions-file--possible-names (key dirs extensions &optional entry)
  "Possible names for files correponding to KEY, ENTRY with EXTENSIONS in DIRS."
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
           (file-field (bibtex-actions-get-value
                        bibtex-actions-file-variable entry))
           (results-file
            (when file-field
              (seq-mapcat
               (lambda (func)
                 (funcall
                  func
                  ;; Make sure this arg is non-nil.
                  (or dirs "/")
                  file-field))
               bibtex-actions-file-parser-functions))))
      (append results-key results-file))))

(defun bibtex-actions-file--files-for-entry (key entry dirs extensions)
    "Find files related to KEY, ENTRY in DIRS with extension in EXTENSIONS."
    (seq-filter #'file-exists-p
                (bibtex-actions-file--possible-names key dirs extensions entry)))

(defun bibtex-actions-file--files-for-multiple-entries (keys-entries dirs extensions)
  "Find files related to a list of KEYS-ENTRIES in DIRS with extension in EXTENSIONS."
  (seq-mapcat
   (lambda (key-entry)
     (bibtex-actions-file--files-for-entry
      (car key-entry) (cdr key-entry) dirs extensions)) keys-entries))

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

(defun bibtex-actions-file-open-notes-default-org (key entry)
  "Open a note file from KEY and ENTRY."
  (if-let* ((file
             (caar (bibtex-actions-file--get-note-filename
                    key
                    bibtex-actions-notes-paths '("org"))))
            (file-exists (file-exists-p file)))
      (funcall bibtex-actions-file-open-function file)
    (let* ((uuid (org-id-new))
           (note-meta
            (bibtex-actions--format-entry-no-widths
             entry
             (bibtex-actions-get-template 'note)))
           (content
            (concat ":PROPERTIES:\n:ID:  " uuid "\n:END:\n" note-meta "\n")))
      (funcall bibtex-actions-file-open-function file)
      ;; This just overrides other template insertion.
      (erase-buffer)
      (insert content))))

(defun bibtex-actions-file--get-note-filename (key dirs extensions)
  "Return existing or new filename for KEY in DIRS with extension in EXTENSIONS.

This is for use in a note function where notes are one-per-file,
with citekey as filename.

Returns the filename whether or not the file exists, to support a
function that will open a new file if the note is not present."
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
       possible-files))))

(provide 'bibtex-actions-file)
;;; bibtex-actions-file.el ends here
