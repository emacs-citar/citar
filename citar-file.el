;;; citar-file.el --- File functions for citar -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Functions for opening and creating bibliographic files related to a source.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'seq)

(declare-function citar-get-entry "citar")
(declare-function citar-get-value "citar")
(declare-function citar-get-template "citar")
(declare-function citar--format-entry-no-widths "citar")

;;;; File related variables

(defcustom  citar-file-variable "file"
  "The field key to look for in an entry for PDF, etc."
  :group 'citar
  :type '(string))

(defcustom citar-file-open-function 'find-file
  "Function to open a file."
  :group 'citar
  :type '(function))

(defcustom citar-file-open-prompt t
  "Prompt for selection of related files to open."
  :group 'citar
  :type '(boolean))

(defcustom citar-file-parser-functions
  '(citar-file-parser-default
    citar-file-parser-triplet)
  "List of functions to parse file field."
  :group 'citar
  :type '(repeat function))

(defcustom citar-file-open-function 'find-file
  "Function to use to open files."
  :group 'citar
  :type '(function))

(defcustom citar-file-extensions '("pdf" "org" "md")
  "List of file extensions to recognize for related files.

These are the extensions the 'citar-file-open-function'
will open, via `citar-file-open'."
  :group 'citar
  :type '(repeat string))

(defcustom citar-file-find-additional-files nil
  "Find additional library files starting with reference key.
If t, all files whose base name starts with the reference key and
whose extension is listed in `citar-file-extensions' are located
by the functions `citar-open-library-files' and
`citar-open-notes'.  If nil, only files with the naming scheme
\"<key>.<extension>\" are located.  Otherwise, its value is a
regular expression specifying how the key is separated from the
rest of the filename."
  :group 'citar
  :type '(choice (const :tag "Ignore additional files" nil)
                 (const :tag "Find all files starting with key" t)
                 (const :tag "Find files with space after key" "[[:space:]]")
                 (regexp :tag "Filename separator")))

(defvar citar-notes-paths)

;;;; Convenience functions for files and paths

(defun citar-file--normalize-paths (file-paths)
  "Return a list of FILE-PATHS normalized with truename."
  (if (stringp file-paths)
      ;; If path is a string, return as a list.
      (list (file-truename file-paths))
    (delete-dups
     (mapcar
      (lambda (p) (file-truename p)) file-paths))))

(defun citar-file-parser-default (dirs file-field)
  "Return a list of files from DIRS and FILE-FIELD."
  (let ((files (split-string file-field ";")))
    (delete-dups
     (seq-mapcat
      (lambda (dir)
        (mapcar
         (lambda (file)
           (expand-file-name file dir)) files))
      dirs))))

(defun citar-file-parser-triplet (dirs file-field)
  "Return a list of files from DIRS and a FILE-FIELD formatted as a triplet.

This is file-field format seen in, for example, Calibre and Mendeley.

Example: ':/path/to/test.pdf:PDF'."
  (let ((parts (split-string file-field "[,;]" 'omit-nulls)))
    (seq-mapcat
     (lambda (part)
       (let ((fn (car (split-string part ":" 'omit-nulls))))
         (mapcar (apply-partially #'expand-file-name fn) dirs)))
     parts)))

(defun citar-file--possible-names (key dirs extensions &optional entry find-additional)
  "Possible names for files correponding to KEY, ENTRY with EXTENSIONS in DIRS."
    (let* ((filematch (when find-additional
                        (format "\\`%s\\(?:%s.*\\)?\\.\\(?:%s\\)\\'"
                                (regexp-quote key)
                                (if (eq t find-additional) "" find-additional)
                                (mapconcat #'regexp-quote extensions "\\|"))))
           (results-key (seq-mapcat
                         (lambda (dir)
                           (if filematch
                               (directory-files dir t filematch)
                             (seq-filter
                              #'file-exists-p
                              (seq-map (lambda (ext)
                                         (expand-file-name (concat key "." ext) dir))
                                       extensions))))
                         dirs))
           (file-field (citar-get-value
                        citar-file-variable entry))
           (results-file
            (when file-field
              (seq-mapcat
               (lambda (func)
                 (funcall
                  func
                  ;; Make sure this arg is non-nil.
                  (or dirs "/")
                  file-field))
               citar-file-parser-functions))))
      (delete-dups (append results-key results-file))))

(defun citar-file--files-for-entry (key entry dirs extensions)
    "Find files related to KEY, ENTRY in DIRS with extension in EXTENSIONS."
    (seq-filter #'file-exists-p
                (citar-file--possible-names key dirs extensions entry citar-file-find-additional-files)))

(defun citar-file--files-for-multiple-entries (keys-entries dirs extensions)
  "Find files related to a list of KEYS-ENTRIES in DIRS with extension in EXTENSIONS."
  (seq-mapcat
   (lambda (key-entry)
     (citar-file--files-for-entry
      (car key-entry) (cdr key-entry) dirs extensions)) keys-entries))

;;;; Opening and creating files functions

(defun citar-file-open (file)
  "Open FILE."
  (funcall citar-file-open-function file))

(defun citar-file-open-external (file)
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

(defun citar-file--get-note-filename (key dirs extensions)
  "Return existing or new filename for KEY in DIRS with extension in EXTENSIONS.

This is for use in a note function where notes are one-per-file,
with citekey as filename.

Returns the filename whether or not the file exists, to support a
function that will open a new file if the note is not present."
  (let* ((possible-files
          (citar-file--possible-names key dirs extensions))
         (existing-files
          (seq-filter #'file-exists-p possible-files)))
    (if existing-files
        (seq-map
         (lambda (file) (cons file 'exists))
         existing-files)
      (seq-map
       (lambda (file) (cons file 'new))
       possible-files))))

(provide 'citar-file)
;;; citar-file.el ends here
