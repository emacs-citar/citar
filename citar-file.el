;;; citar-file.el --- File functions for citar -*- lexical-binding: t; -*-
;;
;; SPDX-FileCopyrightText: 2021-2022 Bruce D'Arcus
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
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
(require 'map)

(declare-function citar-get-value "citar" (field key-or-entry))
(declare-function citar--bibliography-files "citar" (&rest buffers))
(declare-function citar--check-configuration "citar" (&rest variables))
(declare-function citar--get-resources-using-function "citar" (func &optional keys))

;;;; File related variables

(defcustom  citar-file-variable "file"
  "The field key to look for in an entry for PDF, etc."
  :group 'citar
  :type '(string))

(defcustom citar-file-parser-functions
  '(citar-file--parser-default
    citar-file--parser-triplet)
  "List of functions to parse file field."
  :group 'citar
  :type '(repeat function))

(defcustom citar-file-open-functions (list (cons "html" #'citar-file-open-external)
                                           (cons t #'find-file))
  "Functions used by `citar-file-open` to open files.

Should be an alist where each entry is of the form (EXTENSION .
FUNCTION). A file whose name ends with EXTENSION will be opened
using FUNCTION. If no entries are found with a matching
extension, FUNCTION associated with key t will be used as the
default."
  :group 'citar
  :type '(repeat (cons
                  (choice (string :tag "Extension")
                          (symbol :tag "Default" t))
                  (function :tag "Function"))))

;; TODO move this to citar.el for consistency with `citar-library-file-extensions'?
(defcustom citar-file-note-extensions '("org" "md")
  "List of file extensions to filter for notes.

These are the extensions the `citar-open-note-function'
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
(defvar citar-library-paths)
(defvar citar-library-file-extensions)
(defvar citar-note-format-function)

;;;; Convenience functions for files and paths

(defun citar-file--normalize-paths (file-paths)
  "Return a list of FILE-PATHS normalized with truename."
  ;; REVIEW why hassle with this; just require a list?
  (let ((paths (if (stringp file-paths)
                   (list file-paths)
                 file-paths)))
  (citar-file--files-exist-p paths)
  (delete-dups (mapcar #'file-truename paths))))

(defun citar-file--files-exist-p (files)
  "Check each of a list of FILES exists."
  (dolist (file files)
     (unless (file-exists-p file)
       (user-error "Cannot find file: %s" file))))

;;;; Parsing file fields

(defun citar-file--parser-default (file-field)
  "Split FILE-FIELD by `;'.
If resulting filenames might be backslash-escaped, return both
escaped and unescaped versions. Zotero escapes special characters
like backslashes and colons, both of which commonly appear in
Windows filenames."
  (mapcan
   (lambda (filename)
     (let* ((trimmed (string-trim filename))
            (unescaped (replace-regexp-in-string "\\\\\\(.\\)" "\\1" trimmed)))
       (unless (string-empty-p trimmed)
         (if (string= trimmed unescaped)
             (list trimmed)
           (list unescaped trimmed)))))
   (citar-file--split-escaped-string file-field ?\;)))

(defun citar-file--parser-triplet (file-field)
  "Return a list of files from DIRS and a FILE-FIELD formatted as a triplet.

This is file-field format seen in, for example, Calibre and Mendeley.

Example: ':/path/to/test.pdf:PDF'."
  (let (filenames)
    (dolist (sepchar '(?\; ?,))         ; Mendeley and Zotero use ;, Calibre uses ,
      (dolist (substring (citar-file--split-escaped-string file-field sepchar))
        (let* ((triplet (citar-file--split-escaped-string substring ?:))
               (len (length triplet)))
          (when (>= len 3)
            ;; If there are more than three components, we probably split on unescaped : in the filename.
            ;; Take all but the first and last components of TRIPLET and join them with :
            (let* ((escaped (string-join (butlast (cdr triplet)) ":"))
                   (filename (replace-regexp-in-string "\\\\\\(.\\)" "\\1" escaped)))
              ;; Calibre doesn't escape file names in BIB files, so try both
              ;; See https://github.com/kovidgoyal/calibre/blob/master/src/calibre/library/catalogs/bibtex.py
              (push filename filenames)
              (push escaped filenames))))))
    (nreverse filenames)))

(defun citar-file--parse-file-field (fieldvalue dirs &optional citekey)
  "Return files found in file field FIELDVALUE.
Relative file names are expanded from the first directory in DIRS
in which they are found. Omit non-existing absolute file names
and relative file names not found in DIRS. On failure, print a
message explaining the cause; CITEKEY is included in this failure
message."
  (if-let ((files (delete-dups (mapcan (lambda (parser)
                                         (funcall parser fieldvalue))
                                       citar-file-parser-functions))))
      (if-let ((foundfiles (citar-file--find-files-in-dirs files dirs)))
          (if (null citar-library-file-extensions)
              foundfiles
            (or (seq-filter (lambda (file)
                              (member (file-name-extension file) citar-library-file-extensions))
                            foundfiles)
                (ignore
                 (message "No files for `%s' with `citar-library-file-extensions': %S"
                          citekey foundfiles))))
        (ignore
         (message (concat "None of the files for `%s' exist; check `citar-library-paths' and "
                          "`citar-file-parser-functions': %S")
                  citekey files)))
    (ignore
     (if (string-empty-p (string-trim fieldvalue))
         (message "Empty `%s' field: %s" citar-file-variable citekey)
       (message "Could not parse `%s' field of `%s'; check `citar-file-parser-functions': %s"
                citar-file-variable citekey fieldvalue)))))

(defun citar-file--has-file-field ()
  "Return predicate to test if bibliography entry in ENTRIES has a file field.
Note: this function is intended to be used in
`citar-has-files-functions'. Use `citar-has-files' to test
whether entries have associated files."
  (when citar-file-variable
    (lambda (citekey) (and (citar-get-value citar-file-variable citekey) t))))

(defun citar-file--get-from-file-field (&optional keys)
  "Return files for KEYS by parsing the `citar-file-variable' field.

Return a hash table mapping each element of KEYS to a list of
files given in the bibliography entry named by
`citar-file-variable'. If KEYS is nil, return files for all
entries.

Note: this function is intended to be used in
`citar-get-files-functions'. Use `citar-get-files' to get all
files associated with KEYS."
  (when-let ((filefield citar-file-variable))
    (citar--check-configuration 'citar-library-paths 'citar-library-file-extensions
                                'citar-file-parser-functions)
    (let ((dirs (append citar-library-paths
                        (mapcar #'file-name-directory (citar--bibliography-files)))))
      (citar--get-resources-using-function
       (lambda (citekey entry)
         (when-let ((fieldvalue (citar-get-value filefield entry)))
           (citar-file--parse-file-field fieldvalue dirs citekey)))
       keys))))

;;;; Scanning library directories

(defun citar-file--has-library-files ()
  "Return predicate testing whether cite key has library files."
  (let ((files (citar-file--get-library-files)))
    (unless (hash-table-empty-p files)
      (lambda (citekey)
        (and (gethash citekey files) t)))))

(defun citar-file--get-library-files (&optional keys)
  "Return list of files for KEYS in ENTRIES."
  (citar--check-configuration 'citar-library-paths 'citar-library-file-extensions)
  (citar-file--directory-files
   citar-library-paths keys citar-library-file-extensions
   citar-file-additional-files-separator))

(defun citar-file--make-filename-regexp (keys extensions &optional additional-sep)
  "Regexp matching file names starting with KEYS and ending with EXTENSIONS.
When ADDITIONAL-SEP is non-nil, it should be a regular expression
that separates the key from optional additional text that follows
it in matched file names.  The returned regexp captures the key
as group 1, the extension as group 2, and any additional text
following the key as group 3."
  (when (and (null keys) (string-empty-p additional-sep))
    (setq additional-sep nil))
  (concat
   "\\`"
   (if keys (regexp-opt keys "\\(?1:") "\\(?1:[^z-a]*?\\)")
   (when additional-sep (concat "\\(?3:" additional-sep "[^z-a]*\\)?"))
   "\\."
   (if extensions (regexp-opt extensions "\\(?2:") "\\(?2:[^.]*\\)")
   "\\'"))

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
file; and the portion of the file name preceding the first match
of ADDITIONAL-SEP.

When KEYS is nil, if ADDITIONAL-SEP is empty then it is treated
as being nil. In other words, this function can only scan a
directory for file names matching unknown keys if either

1. The key is not followed by any additional text except for the
   file extension.

2. There is a non-empty ADDITIONAL-SEP between the key and any
   following text.

Note: when KEYS and EXTENSIONS are non-nil and ADDITIONAL-SEP is
nil, this function has an optimized implementation; it checks for
existing files named \"KEY.EXT\" in DIRS, with KEY and EXT being
the elements of KEYS and EXTENSIONS, respectively.  It does not
need to scan the contents of DIRS in this case."
  (let ((files (make-hash-table :test 'equal))
        (filematch (unless (and keys extensions (not additional-sep))
                     (citar-file--make-filename-regexp keys extensions additional-sep))))
    (prog1 files
      (dolist (dir dirs)
        (when (file-directory-p dir)
          (if filematch
              ;; Use regexp to scan directory
              (dolist (file (directory-files dir nil filematch))
                (let ((key (and (string-match filematch file) (match-string 1 file)))
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

;;;; Opening and creating files functions

(defun citar-file-open (file)
  "Open FILE."
  (if-let* ((ext (file-name-extension file))
            (func (cdr (or (assoc-string ext citar-file-open-functions 'case-fold)
                           (assq t citar-file-open-functions)))))
      (funcall func (expand-file-name file))
    (user-error "Could not find extension in `citar-file-open-functions': %s" ext)))

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


;;;; Note files

(defun citar-file--get-notes (&optional keys)
  "Return note files associated with KEYS.
Return hash table whose keys are elements of KEYS and values are
lists of note file names found in `citar-notes-paths' having
extensions in `citar-file-note-extensions'."
  (citar--check-configuration 'citar-notes-paths 'citar-file-note-extensions)
  (citar-file--directory-files
   citar-notes-paths keys citar-file-note-extensions
   citar-file-additional-files-separator))

(defun citar-file--has-notes ()
  "Return predicate testing whether cite key has associated notes."
  (let ((notes (citar-file--get-notes)))
    (unless (hash-table-empty-p notes)
      (lambda (citekey) (and (gethash citekey notes) t)))))

(defun citar-file--create-note (key entry)
  "Create a note file from KEY and ENTRY."
  (if-let ((filename (citar-file--get-note-filename key)))
      (prog1 (find-file filename)
        (unless (file-exists-p filename)
          (citar--check-configuration 'citar-note-format-function)
          (funcall citar-note-format-function key entry)))
    (user-error "Make sure `citar-notes-paths' and `citar-file-note-extensions' are non-nil")))

(defun citar-file--get-note-filename (key)
  "Return existing or new note filename for KEY.

This is for use in a note function where notes are one-per-file,
with citekey as filename.

Returns the filename whether or not the file exists, to support a
function that will open a new file if the note is not present."
  (citar--check-configuration 'citar-notes-paths 'citar-file-note-extensions)
  (let* ((dirs citar-notes-paths)
         (exts citar-file-note-extensions)
         (files (citar-file--directory-files dirs (list key) exts citar-file-additional-files-separator)))
    (or (car (gethash key files))
        (when-let ((dir (car dirs))
                   (ext (car exts)))
          (expand-file-name (concat key "." ext) dir)))))

;;;; Utility functions

(defun citar-file--split-escaped-string (string sepchar)
  "Split STRING into substrings at unescaped occurrences of SEPCHAR.
A character is escaped in STRING if it is preceded by `\\'. The
`\\' character can also escape itself. Return a list of
substrings of STRING delimited by unescaped occurrences of
SEPCHAR."
  (let ((skip (format "^\\\\%c" sepchar))
        strings)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (progn (skip-chars-forward skip) (not (eobp)))
        (if (= ?\\ (following-char))
            (ignore-error 'end-of-buffer (forward-char 2))
          (push (delete-and-extract-region (point-min) (point)) strings)
          (delete-char 1)))
      (push (buffer-string) strings))
    (nreverse strings)))

(defun citar-file--find-files-in-dirs (files dirs)
  "Expand file names in FILES in DIRS and keep the ones that exist."
  (let (foundfiles)
    (dolist (file files)
      (if (file-name-absolute-p file)
          (when (file-exists-p file) (push (expand-file-name file) foundfiles))
        (when-let ((filepath (seq-some (lambda (dir)
                                         (let ((filepath (expand-file-name file dir)))
                                           (when (file-exists-p filepath) filepath)))
                                       dirs)))
          (push filepath foundfiles))))
    (nreverse foundfiles)))

(provide 'citar-file)
;;; citar-file.el ends here
