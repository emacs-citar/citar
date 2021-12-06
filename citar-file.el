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

(defcustom citar-file-extensions '("pdf")
  "List of file extensions to recognize for related files.

These are the extensions the 'citar-file-open-function'
will open, via `citar-file-open'."
  :group 'citar
  :type '(repeat string))

(defcustom citar-file-note-extensions '("org" "md")
  "List of file extensions to recognize for notes.

These are the extensions the 'citar-open-note-function'
will open, via `citar-open-notes'."
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
rest of the filename.

Note: setting this variable to t or a separator that is present
in citation keys can lead to incorrect \"has:note\" indicators in
the displayed citation key candidates."
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

(defun citar-file--parse-file-field (entry fieldname &optional dirs)
  "Return files listed in FIELDNAME of ENTRY.
File names are expanded relative to the elements of DIRS."
  (unless dirs (setq dirs (list "/")))  ; make sure DIRS is non-nil
  (when-let ((filefield (citar-get-value fieldname entry)))
    (seq-mapcat (lambda (parser)
                  (funcall parser dirs filefield))
                citar-file-parser-functions)))

(defun citar-file--make-filename-regexp (keys extensions &optional find-additional)
  "Regexp matching file names starting with KEYS and ending with EXTENSIONS.
See the documentation of `citar-file--directory-files` for the
meaning of FIND-ADDITIONAL."
  (when (and (null keys) (eq t find-additional))
    (setq find-additional nil))
  (concat
   "\\`\\(?1:"
   (if keys (mapconcat #'regexp-quote keys "\\|") "[^z-a]*?")
   "\\)"
   (when find-additional
     (concat "\\(?:"
             (if (eq t find-additional) "" find-additional)
             "[^z-a]*\\)?"))
   "\\.\\(?:"
   (if extensions (mapconcat #'regexp-quote extensions "\\|") "[^.]*")
   "\\)\\'"))

(defun citar-file--directory-files (dirs &optional keys extensions find-additional)
  "Return files in DIRS starting with KEYS and ending with EXTENSIONS.

Return a hash table mapping keys to lists of file names present
in DIRS.  Each file name is divided into three parts: the key,
optional additional text, and the extension:

- When KEYS is non-nil, each file name must start with an element
of KEYS.  Otherwise file names can start with any key.

- When EXTENSIONS is non-nil, the file extension must match one
of its elements.  Otherwise the files can have any extension.

- When FIND-ADDITIONAL is non-nil, the file name can have
additional text following the key.  When FIND-ADDITIONAL is t,
any text is allowed after the key.  Otherwise, FIND-ADDITIONAL is
a regexp separating the key from the additional text.

When KEYS is nil, each file name is stored in the hash table
under two keys: the base name of the file and, when
FIND-ADDITIONAL is neither nil nor t, the portion of the file
name preceding the first match of FIND-ADDITIONAL.

Note: when KEYS and EXTENSIONS are non-nil and FIND-ADDITIONAL is
nil, this function has an optimized implementation; it checks for
existing files named \"KEY.EXT\" in DIRS, with KEY and EXT being
the elements of KEYS and EXTENSIONS, respectively.  It does not
need to scan the contents of DIRS in this case."
  (let ((files (make-hash-table :test #'equal))
        (filematch (unless (and keys extensions (not find-additional))
                     (citar-file--make-filename-regexp keys extensions find-additional))))
    (prog1 files
      (dolist (dir dirs)
        (when (file-directory-p dir)
          (if filematch
              (dolist (file (directory-files dir nil filematch 'nosort))
                (let ((key (and (string-match filematch file) (match-string 1 file)))
                      (filename (expand-file-name file dir))
                      (basename (file-name-base file)))
                  (push filename (gethash key files))
                  (unless (or keys (string= key basename))
                    (push filename (gethash basename files)))))
            (dolist (key keys)
              (dolist (ext extensions)
                (let ((filename (expand-file-name (concat key "." ext) dir)))
                  (when (file-exists-p filename)
                    (push filename (gethash key files)))))))))
      (maphash (lambda (key filelist)
                 (puthash key (nreverse filelist) files))
               files))))

(defun citar-file--has-file-p (dirs extensions &optional find-additional entry-field)
  "Return predicate testing whether a key and entry have associated files.

Files are found in two ways:

- In DIRS using `citar-file--directory-files`; see its
documentation for the meaning of EXTENSIONS and FIND-ADDITIONAL.

- In the entry field ENTRY-FIELD, when it is non-nil."
  (let ((files (citar-file--directory-files dirs nil extensions find-additional)))
    (lambda (key entry)
      (or (car (gethash key files))
          (and entry-field
               (seq-some #'file-exists-p
                         (citar-file--parse-file-field entry entry-field dirs)))))))

(defun citar-file--files-for-entry (key entry dirs extensions)
  "Find files related to KEY, ENTRY in DIRS with extension in EXTENSIONS."
  (citar-file--files-for-multiple-entries (list (cons key entry)) dirs extensions))

(defun citar-file--files-for-multiple-entries (keys-entries dirs extensions)
  "Find files related to a list of KEYS-ENTRIES in DIRS with extension in EXTENSIONS."
  (let* ((keys (seq-map #'car keys-entries))
         (files (citar-file--directory-files dirs keys extensions
                                             citar-file-find-additional-files)))
    (delete-dups
     (seq-mapcat
      (lambda (key-entry)
        (append
         (gethash (car key-entry) files)
         (seq-filter
          #'file-exists-p
          (citar-file--parse-file-field (cdr key-entry) citar-file-variable dirs))))
      keys-entries))))

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
  (let ((files (citar-file--directory-files dirs (list key) extensions
                                            citar-file-find-additional-files)))
    (or (car (gethash key files))
        (when-let ((dir (car dirs))
                   (ext (car extensions)))
          (expand-file-name (concat key "." ext) dir)))))

(provide 'citar-file)
;;; citar-file.el ends here
