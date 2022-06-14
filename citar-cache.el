;;; citar-cache.el --- Cache functions for citar -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus, Roshan Shariff
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
;;  Functions for caching bibliography files.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'parsebib)
(require 'citar-format)

(declare-function citar--get-template "citar")
(declare-function citar--fields-to-parse "citar")

(defvar citar-ellipsis)

;;; Variables:


(defvar citar-cache--bibliographies (make-hash-table :test 'equal)
  "Cache for parsed bibliography files.
This is an association list following the pattern:
  (FILE-ID . ENTRIES)
FILE-ID is a cons cell (FILE . HASH), with FILE being the absolute file name of
the bibliography file, and HASH a hash of its contents.
ENTRIES is a hash table with citation references as keys and fields alist as
values.")


;;; Bibliography objects


(cl-defstruct (citar-cache--bibliography
               (:constructor citar-cache--make-bibliography (filename))
               (:copier nil))
  "Cached bibliography file."
  (filename
   nil
   :read-only t
   :documentation
   "True filename of a bibliography, as returned by `file-truename`.")
  (hash
   nil
   :documentation
   "Hash of the file's contents, as returned by `buffer-hash`.")
  (buffers
   nil
   :documentation
   "List of buffers that require this bibliography.")
  (entries
   (make-hash-table :test 'equal)
   :documentation
   "Hash table mapping citation keys to bibliography entries,
   as returned by `parsebib-parse`.")
  (preformatted
   (make-hash-table :test 'equal)
   :documentation
   "Pre-formatted strings used to display bibliography entries;
   see `citar--preformatter`.")
  (format-string
   nil
   :documentation
   "Format string used to generate pre-formatted strings."))


(defun citar-cache--get-bibliographies (filenames &optional buffer)
  "Return cached bibliographies for FILENAMES and associate them with BUFFER.
FILENAMES is a list of bibliography file names.  If BUFFER is
nil, use the current buffer.  Otherwise, BUFFER should be a
buffer object or name that requires these bibliographies, or a
symbol like 'global.

Remove any existing associations between BUFFER and cached files
not included in FILENAMES.  Release cached files that are no
longer needed by any other buffer.

Return a list of `citar--bibliography` objects, one for each
element of FILENAMES."
  (citar-cache--release-bibliographies filenames buffer)
  (mapcar
   (lambda (filename) (citar-cache--get-bibliography filename buffer))
   filenames))

(defun citar-cache--entry (key bibs)
  (catch :found
    ;; Iterate through the cached bibliography hashes and find a key.
    (dolist (bib bibs)
      (let* ((entries (citar-cache--bibliography-entries bib))
             (entry (gethash key entries)))
        (when entry (throw :found entry))))
    nil))

(defun citar-cache--entries (bibs)
  (citar-cache--merge-hash-tables
   (mapcar #'citar-cache--bibliography-entries bibs)))

(defun citar-cache--preformatted (bibs)
  (citar-cache--merge-hash-tables
   (mapcar #'citar-cache--bibliography-preformatted bibs)))


;;; Creating and deleting bibliography caches


(defun citar-cache--get-bibliography (filename &optional buffer)
  "Return cached bibliography for FILENAME and associate it with BUFFER.
If FILENAME is not already cached, read and cache it.  If BUFFER
is nil, use the current buffer.  Otherwise, BUFFER should be a
buffer object or name that requires the bibliography FILENAME, or
a symbol like 'global."
  (let* ((cached (gethash filename citar-cache--bibliographies))
         (bib (or cached (citar-cache--make-bibliography filename)))
         (buffer (citar-cache--canonicalize-buffer buffer))
         (fmtstr (citar--get-template 'completion)))
    (unless cached
      (setf (citar-cache--bibliography-format-string bib) fmtstr)
      (citar-cache--update-bibliography bib)
      (puthash filename bib citar-cache--bibliographies))
    ;; Preformat strings if format has changed
    (unless (equal-including-properties
             fmtstr (citar-cache--bibliography-format-string bib))
      (setf (citar-cache--bibliography-format-string bib) fmtstr)
      (citar-cache--preformat-bibliography bib))
    ;; Associate buffer with this bibliography:
    (cl-pushnew buffer (citar-cache--bibliography-buffers bib))
    ;; Release bibliography when buffer is killed or changes major mode:
    (unless (symbolp buffer)
      (with-current-buffer buffer
        (dolist (hook '(change-major-mode-hook kill-buffer-hook))
          (add-hook hook #'citar-cache--release-bibliographies 0 'local))))
    bib))


(defun citar-cache--release-bibliographies (&optional keep-filenames buffer)
  "Dissociate BUFFER from cached bibliographies.
If BUFFER is nil, use the current buffer.  Otherwise, BUFFER
should be a buffer object, buffer name, or a symbol like 'global.
KEEP-FILENAMES is a list of file names that are not dissociated
from BUFFER.

Remove any bibliographies from the cache that are no longer
needed by any other buffer."
  (let ((buffer (citar-cache--canonicalize-buffer buffer)))
    (maphash
     (lambda (filename bib)
       (unless (member filename keep-filenames)
         (cl-callf2 delq buffer (citar-cache--bibliography-buffers bib))
         (unless (citar-cache--bibliography-buffers bib)
           (citar-cache--remove-bibliography filename))))
     citar-cache--bibliographies)))


(defun citar-cache--remove-bibliography (filename)
  "Remove bibliography cache entry for FILENAME."
  ;; TODO Perform other needed  actions, like removing filenotify watches
  (remhash filename citar-cache--bibliographies))


;;; Updating bibliographies


(defun citar-cache--update-bibliography (bib &optional force)
  "Update the bibliography BIB from the original file.

Unless FORCE is non-nil, the file is re-read only if it has been
modified since the last time BIB was updated."
  (let* ((filename (citar-cache--bibliography-filename bib))
         (entries (citar-cache--bibliography-entries bib))
         (newhash (with-temp-buffer
                    (insert-file-contents filename)
                    (buffer-hash))))
    ;; TODO Also check file size and modification time before hashing?
    ;; See `file-has-changed-p` in emacs 29, or `org-file-has-changed-p`
    (when (or force (not (equal newhash (citar-cache--bibliography-hash bib))))
      ;; Update entries
      (clrhash entries)
      (parsebib-parse filename :entries entries :fields (citar--fields-to-parse))
      (setf (citar-cache--bibliography-hash bib) newhash)
      ;; Update preformatted strings
      (citar-cache--preformat-bibliography bib))))


(defun citar-cache--preformat-bibliography (bib)
  "Updated pre-formatted strings in BIB."
  (let* ((entries (citar-cache--bibliography-entries bib))
         (formatstr (citar-cache--bibliography-format-string bib))
         (fieldspecs (citar-format--parse formatstr))
         (preformatted (citar-cache--bibliography-preformatted bib)))
    (clrhash preformatted)
    (maphash
     (lambda (citekey entry)
       (let* ((preformat (citar-format--preformat fieldspecs entry
                                                  t citar-ellipsis))
              ;; CSL-JSOM lets citekey be an arbitrary string. Quote it if...
              (keyquoted (if (or (string-empty-p citekey) ; ... it's empty,
                                 (= ?\" (aref citekey 0)) ; ... starts with ",
                                 (cl-find ?\s citekey))   ; ... or has a space
                             (prin1-to-string citekey)
                           citekey))
              (prefix (propertize (concat keyquoted (when (cdr preformat) " "))
                                  'invisible t)))
         (setcdr preformat (cons (concat prefix (cadr preformat))
                                 (cddr preformat)))
         (puthash citekey preformat preformatted)))
     entries)))


;;; Utility functions:


(defun citar-cache--canonicalize-buffer (buffer)
  "Return buffer object or symbol denoted by BUFFER.
If BUFFER is nil, return the current buffer.  Otherwise, BUFFER
should be a buffer object or name, or a symbol like 'global.  If
it is a buffer object or symbol, it is returned as-is.
Otherwise, return the buffer object whose name is BUFFER."
  (cond ((null buffer) (current-buffer))
        ((symbolp buffer) buffer)
        (t (get-buffer buffer))))


(defun citar-cache--merge-hash-tables (hash-tables)
  "Merge hash tables in HASH-TABLES."
  (when-let ((hash-tables (reverse hash-tables))
             (first (pop hash-tables)))
    (if (null hash-tables)
        first
      (let ((result (copy-hash-table first)))
        (dolist (table hash-tables result)
          (maphash (lambda (key entry) (puthash key entry result)) table))))))


(provide 'citar-cache)
;;; citar-cache.el ends here
