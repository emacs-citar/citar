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
  (require 'cl-lib)
  (require 'subr-x))
(require 'seq)

(declare-function citar--get-entry "citar")
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

(defcustom citar-file-extensions nil
  "List of file extensions to filter for related files.

These are the extensions the 'citar-file-open-function'
will open, via `citar-file-open'.

When nil, the function will not filter the list of files."
  :group 'citar
  :type '(repeat string))

(defcustom citar-file-note-extensions '("org" "md")
  "List of file extensions to filter for notes.

These are the extensions the 'citar-open-note-function'
will open, via `citar-open-notes'."
  :group 'citar
  :type '(repeat string))

(defcustom citar-file-additional-files-separator nil
  "Find additional library files starting with reference key.

If nil, the functions `citar-open-library-files' and
`citar-open-notes' only locate files with the naming scheme
\"<key>.<extension>\".  Otherwise, the value of this variable
should be a regular expression that separates the key from
optional additional text following the key in the file name.
Then files named as \"<key><separator><extra>.<extension>\" are
also located.

Note: when non-nil, the value of this variable should be a
separator that does not otherwise occur in citation keys."
  :group 'citar
  :type '(choice (const :tag "Ignore additional files" nil)
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
  (let ((files (split-string file-field "[:;]")))
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

(defun citar-file--extension-p (filename extensions)
  "Return non-nil if FILENAME extension is among EXTENSIONS."
  (member (file-name-extension filename) extensions))

(defun citar-file--parse-file-field (entry fieldname &optional dirs extensions)
  "Return files listed in FIELDNAME of ENTRY.
File names are expanded relative to the elements of DIRS.

Filter by EXTENSIONS when present."
  (unless dirs (setq dirs (list "/")))  ; make sure DIRS is non-nil
  (let* ((filefield (citar-get-value fieldname entry))
         (files
          (when filefield
            (delete-dups
             (seq-mapcat
              (lambda (parser)
                (funcall parser dirs filefield))
              citar-file-parser-functions)))))
    (if extensions
        (seq-filter
         (lambda (fn)
           (citar-file--extension-p fn extensions)) files)
      files)))

(defun citar-file--make-filename-regexp (keys extensions &optional additional-sep)
  "Regexp matching file names starting with KEYS and ending with EXTENSIONS.
When ADDITIONAL-SEP is non-nil, it should be a regular expression
that separates the key from optional additional text that follows
it in matched file names.  The returned regexp captures the key
as group 1, the extension as group 2, and any additional text
following the key as group 3."
  (let* ((entry (when keys
                  (citar--get-entry (car keys))))
         (xref (citar-get-value "crossref" entry)))
    (unless (or (null xref) (string-empty-p xref))
      (push xref keys))
    (when (and (null keys) (string-empty-p additional-sep))
      (setq additional-sep nil))
    (concat
     "\\`"
     (if keys (regexp-opt keys "\\(?1:") "\\(?1:[^z-a]*?\\)")
     (when additional-sep (concat "\\(?3:" additional-sep "[^z-a]*\\)?"))
     "\\."
     (if extensions (regexp-opt extensions "\\(?2:") "\\(?2:[^.]*\\)")
     "\\'")))

(defun citar-file--directory-files (dirs &optional keys extensions additional-sep)
  "Return files in DIRS starting with KEYS and ending with EXTENSIONS.

Return a hash table mapping keys to lists of file names present
in DIRS.  Each file name is divided into three parts: the key,
optional additional text, and the extension:

- When KEYS is non-nil, each file name must start with an element
of KEYS.  Otherwise file names can start with any key.

- When EXTENSIONS is non-nil, the file extension must match one
of its elements.  Otherwise the files can have any extension.

- When ADDITIONAL-SEP is non-nil, the file name can have
additional text following the key.  ADDITIONAL-SEP is a regexp
separating the key from the additional text.

When KEYS is nil and ADDITIONAL-SEP is non-nil, each file name is
stored in the hash table under two keys: the base name of the
file and the portion of the file name preceding the first match
of ADDITIONAL-SEP.

Note: when KEYS and EXTENSIONS are non-nil and ADDITIONAL-SEP is
nil, this function has an optimized implementation; it checks for
existing files named \"KEY.EXT\" in DIRS, with KEY and EXT being
the elements of KEYS and EXTENSIONS, respectively.  It does not
need to scan the contents of DIRS in this case."
  (let ((files (make-hash-table :test #'equal))
        (filematch (unless (and keys extensions (not additional-sep))
                     (citar-file--make-filename-regexp keys extensions additional-sep))))
    (prog1 files
      (dolist (dir dirs)
        (when (file-directory-p dir)
          (if filematch
              ;; Use regexp to scan directory
              (dolist (file (directory-files dir nil filematch))
                (let ((key (if keys (car keys)
                             (and (string-match filematch file) (match-string 1 file))))
                      (filename (expand-file-name file dir))
                      (basename (file-name-base file)))
                  (push filename (gethash key files))
                  (unless (or keys (string= key basename))
                    (push filename (gethash basename files)))))
            ;; Otherwise, check for files named KEY.EXT
            (dolist (key keys)
              (dolist (ext extensions)
                (let ((filename (expand-file-name (concat key "." ext) dir)))
                  (when (file-exists-p filename)
                    (push filename (gethash key files)))))))))
      ;; Reverse file lists because push adds elements to the front
      (maphash (lambda (key filelist)
                 (puthash key (nreverse filelist) files))
               files))))

(defun citar-file--has-file (dirs extensions &optional entry-field)
  "Return predicate testing whether a key and entry have associated files.

Files are found in two ways:

- By scanning DIRS for files with EXTENSIONS using
  `citar-file--directory-files`, which see.  Its ADDITIONAL-SEP
  argument is taken from `citar-file-additional-files-separator`.

- When ENTRY-FIELD is non-nil, by parsing the entry field it
  names using `citar-file--parse-file-field`; see its
  documentation.  DIRS is used to resolve relative paths and
  non-existent files are ignored.

Note: for performance reasons, this function should be called
once per command; the function it returns can be called
repeatedly."
  (let ((files (citar-file--directory-files dirs nil extensions
                                            citar-file-additional-files-separator)))
    (lambda (key entry)
      (let* ((xref (citar-get-value "crossref" entry))
             (cached (if (and xref
                              (not (eq 'unknown (gethash xref files 'unknown))))
                         (gethash xref files 'unknown)
                       (gethash key files 'unknown))))
        (if (not (eq cached 'unknown))
            cached
          ;; KEY has no files in DIRS, so check the ENTRY-FIELD field of
          ;; ENTRY.  This will run at most once for each KEY; after that, KEY
          ;; in hash table FILES will either contain nil or a file name found
          ;; in ENTRY.
          (puthash key
                   (seq-some
                    #'file-exists-p
                    (citar-file--parse-file-field entry entry-field dirs extensions))
                   files))))))

(defun citar-file--files-for-entry (key entry dirs extensions)
  "Find files related to bibliography item KEY with metadata ENTRY.
See `citar-file--files-for-multiple-entries` for details on DIRS,
EXTENSIONS, and how files are found."
  (citar-file--files-for-multiple-entries (list (cons key entry)) dirs extensions))

(defun citar-file--files-for-multiple-entries (key-entry-alist dirs extensions)
  "Find files related to bibliography items in KEYS-ENTRIES.

KEY-ENTRY-ALIST is a list of (KEY . ENTRY) pairs.  Return a list
of files found in two ways:

- By scanning directories in DIRS for files starting with keys in
  KEYS-ENTRIES and having extensions in EXTENSIONS.  The files
  may also have additional text after the key, separated by the
  value of `citar-file-additional-files-separator`.  The scanning
  is performed by `citar-file--directory-files`, which see.

- By parsing the field named by `citar-file-variable` of the
  entries in KEYS-ENTRIES.  DIRS is used to resolve relative
  paths and non-existent files are ignored; see
  `citar-file--parse-file-field`."
  (let* ((keys (seq-map #'car key-entry-alist))
         (files (citar-file--directory-files dirs keys extensions
                                             citar-file-additional-files-separator)))
    (delete-dups
     (seq-mapcat
      (lambda (key-entry)
        (append
         (gethash (car key-entry) files)
         (seq-filter
          #'file-exists-p
          (citar-file--parse-file-field (cdr key-entry) citar-file-variable dirs extensions))))
      key-entry-alist))))

;;;; Opening and creating files functions

(defun citar-file-open (file)
  "Open FILE."
  (if (equal (file-name-extension file) "html")
      (citar-file-open-external (expand-file-name file))
    (funcall citar-file-open-function (expand-file-name file))))

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
                                            citar-file-additional-files-separator)))
    (or (car (gethash key files))
        (when-let ((dir (car dirs))
                   (ext (car extensions)))
          (expand-file-name (concat key "." ext) dir)))))

(provide 'citar-file)
;;; citar-file.el ends here
