;;; citar-cache.el --- Cache functions for citar -*- lexical-binding: t; -*-
;;
;; SPDX-FileCopyrightText: 2022 Bruce D'Arcus, Roshan Shariff
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;;  Functions for caching bibliography files.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'parsebib)
(require 'citar-format)
(require 'seq)
(require 'map)

(declare-function citar--get-template "citar" (template-name))
(declare-function citar--fields-to-parse "citar" ())
(declare-function citar--prepend-candidate-citekey "citar" (citekey candidate))

(defvar citar-ellipsis)

;;; Variables:


(defvar citar-cache--bibliographies (make-hash-table :test 'equal)
  "Cache for parsed bibliography files.
This is a hash table with keys being file names and the values
being `citar-cache--bibliography' objects.")


;;; Bibliography objects


(cl-defstruct (citar-cache--bibliography
               (:constructor citar-cache--make-bibliography (filename))
               (:copier nil))
  "Cached bibliography file."
  (filename
   nil
   :read-only t
   :documentation
   "True filename of a bibliography, as returned by `file-truename'.")
  (buffers
   nil
   :documentation
   "List of buffers that require this bibliography.")
  (props
   nil
   :documentation
   "Plist with keys :size, :mtime, :hash, and :fields; attributes
   of the cached file and the fields parsed from it.")
  (entries
   (make-hash-table :test 'equal)
   :documentation
   "Hash table mapping citation keys to bibliography entries,
   as returned by `parsebib-parse'.")
  (format-string
   nil
   :documentation
   "Format string used to generate pre-formatted strings.")
  (preformatted
   (make-hash-table :test 'equal)
   :documentation
   "Pre-formatted strings used to display bibliography entries;
   see `citar--preformatter'."))


(defun citar-cache--get-bibliographies (filenames &optional buffer)
  "Return cached bibliographies for FILENAMES and associate them with BUFFER.
FILENAMES is a list of bibliography file names. If BUFFER is nil,
use the current buffer. Otherwise, BUFFER should be a buffer
object or name that requires these bibliographies, or a symbol
like `global'.

Remove any existing associations between BUFFER and cached files
not included in FILENAMES. Release cached files that are no
longer needed by any other buffer.

Return a list of `citar-cache--bibliography' objects, one for each
element of FILENAMES."
  (citar-cache--release-bibliographies filenames buffer)
  (let ((buffer (citar-cache--canonicalize-buffer buffer)))
    (mapcar
     (lambda (filename)
       (let ((bib (citar-cache--get-bibliography filename)))
         (prog1 bib
           ;; Associate buffer with this bibliography:
           (cl-pushnew buffer (citar-cache--bibliography-buffers bib))
           ;; Release bibliography when buffer is killed or changes major mode:
           (when (bufferp buffer)
             (with-current-buffer buffer
               (dolist (hook '(change-major-mode-hook kill-buffer-hook))
                 (add-hook hook #'citar-cache--release-bibliographies 0 'local)))))))
     filenames)))

(defun citar-cache--entry (key bibs)
  "Find the first entry for KEY in the bibliographies BIBS.
BIBS should be a list of `citar-cache--bibliography' objects."
  (catch :found
    (dolist (bib bibs)
      (let* ((entries (citar-cache--bibliography-entries bib))
             (entry (gethash key entries)))
        (when entry (throw :found entry))))))

(defun citar-cache--entries (bibs)
  "Return hash table containing merged entries of BIBS.
BIBS should be a list of `citar-cache--bibliography' objects. If
a key is present in multiple bibliographies in BIBS, keep the
entry that appears first. Return a hash table mapping the keys of
all BIBS to their entries."
  (apply #'map-merge '(hash-table :test equal)
         (nreverse (mapcar #'citar-cache--bibliography-entries bibs))))

(defun citar-cache--preformatted (bibs)
  "Return hash table containing pre-formatted strings from BIBS."
  (apply #'map-merge '(hash-table :test equal)
         (nreverse (mapcar #'citar-cache--bibliography-preformatted bibs))))


;;; Creating and deleting bibliography caches


(defun citar-cache--get-bibliography (filename &optional force-update)
  "Return cached bibliography for FILENAME.

If FILENAME is not already cached, read and cache it. If
FORCE-UPDATE is non-nil, re-read the bibliography even if it is
has not changed.

Note: This function should not be called directly; use
`citar-get-bibliographies' instead. This function adds a
bibliography to the cache without associating it with any buffer,
so it will never be evicted from the cache. Use
`citar-cache--get-bibliographies' to ensure that the cached
bibliographies are removed when the associated buffers no longer
need them."
  (let* ((cached (gethash filename citar-cache--bibliographies))
         (cachedprops (and cached (citar-cache--bibliography-props cached)))
         (cachedfmtstr (and cached (citar-cache--bibliography-format-string cached)))
         (props (citar-cache--get-bibliography-props filename cachedprops))
         (fmtstr (citar--get-template 'completion))
         (bib (or cached (citar-cache--make-bibliography filename))))
    (prog1 bib
      ;; Set the format string so it's correct when updating bibliography
      (setf (citar-cache--bibliography-format-string bib) fmtstr)
      ;; Update bibliography if needed or forced
      (if (or force-update
              (citar-cache--update-bibliography-p cachedprops props))
          (citar-cache--update-bibliography bib props)
        ;; Otherwise, update props anyway in case mtime has changed:
        (setf (citar-cache--bibliography-props bib) props)
        ;; Pre-format if format string has changed even though bibliography hasn't
        (unless (equal-including-properties fmtstr cachedfmtstr)
          (citar-cache--preformat-bibliography bib)))
      ;; Add bibliography to cache:
      (unless cached
        (puthash filename bib citar-cache--bibliographies)))))


(defun citar-cache--release-bibliographies (&optional keep-filenames buffer)
  "Dissociate BUFFER from cached bibliographies.
If BUFFER is nil, use the current buffer. Otherwise, BUFFER
should be a buffer object, buffer name, or a symbol like
`global'. KEEP-FILENAMES is a list of file names that are not
dissociated from BUFFER.

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
  ;; TODO Perform other needed actions, like removing filenotify watches
  (remhash filename citar-cache--bibliographies))


;;; Updating bibliographies

(defun citar-cache--get-bibliography-props (filename &optional oldprops)
  "Return attributes to decide if bibliography FILENAME needs to be updated.
Return a plist with keys :size, :mtime, :hash, and :fields.
OLDPROPS, if given, should be a plist with the same keys. If
FILENAME has the same size and modification time as in OLDPROPS,
then assume that the hash value is also the same without
re-hashing the file contents."
  (let* ((remote-file-name-inhibit-cache t)
         (attr (file-attributes filename 'integer))
         (size (file-attribute-size attr))
         (mtime (file-attribute-modification-time attr))
         (fields (citar--fields-to-parse))
         (oldhash (plist-get oldprops :hash))
         (hash (if (and (stringp oldhash)
                        (equal size (plist-get oldprops :size))
                        (equal mtime (plist-get oldprops :mtime)))
                   oldhash              ; if size and mtime are unchanged, assume hash is the same
                 (with-temp-buffer
                   (insert-file-contents filename)
                   (buffer-hash)))))
    `(:size ,size :mtime ,mtime :hash ,hash :fields ,fields)))

(defun citar-cache--update-bibliography-p (oldprops newprops)
  "Return whether bibliography needs to be updated.
Compare NEWPROPS with OLDPROPS and decide whether the file
contents have changed or the list of bibliography fields to be
parsed is different."
  (not (and (equal (plist-get oldprops :size) (plist-get newprops :size))
            (equal (plist-get oldprops :hash) (plist-get newprops :hash))
            (equal (plist-get oldprops :fields) (plist-get newprops :fields)))))

(defun citar-cache--update-bibliography (bib &optional props)
  "Update the bibliography BIB from the original file.

PROPS should be a plist returned by
`citar-cache--get-bibliography-props'; if PROPS is unspecified;
use the value returned by that function. This argument is
provided in case that function has already been called so that
its return value can be reused.

After updating, the `props' slot of BIB is set to PROPS."
  (let* ((filename (citar-cache--bibliography-filename bib))
         (props (or props (citar-cache--get-bibliography-props filename)))
         (entries (citar-cache--bibliography-entries bib))
         (messagestr (format "Updating bibliography %s" (abbreviate-file-name filename)))
         (starttime (current-time)))
    (message "%s..." messagestr)
    (redisplay)                         ; Make sure message is displayed before Emacs gets busy parsing
    (clrhash entries)
    (parsebib-parse filename :entries entries)
    (setf (citar-cache--bibliography-props bib) props)
    (citar-cache--preformat-bibliography bib)
    (message "%s...done (%.3f seconds)" messagestr (float-time (time-since starttime)))))


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
              (withkey (citar--prepend-candidate-citekey citekey (cadr preformat))))
         (setcdr preformat (cons withkey (cddr preformat)))
         (puthash citekey preformat preformatted)))
     entries)))


;;; Utility functions:


(defun citar-cache--canonicalize-buffer (buffer)
  "Return buffer object or symbol denoted by BUFFER.
If BUFFER is nil, return the current buffer. Otherwise, BUFFER
should be a buffer object or name, or a symbol like `global'. If
it is a buffer object or symbol, it is returned as-is. Otherwise,
return the buffer object whose name is BUFFER."
  (cond ((null buffer) (current-buffer))
        ((symbolp buffer) buffer)
        (t (get-buffer buffer))))


(provide 'citar-cache)
;;; citar-cache.el ends here
