;;; citarn.el --- Stripped down test of alt citar -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <bdarcus@gmail.com>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: June 06, 2022
;; Modified: June 06, 2022
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A minimal experiment in an alternative structure for citar caching and the
;;  API.
;;
;;  Code is commented.
;;
;;; Code:

(require 'parsebib)
(require 'citar)

;; Internal variables

;; Most of this design is adapted from org-mode 'oc-basic',
;; written by Nicolas Goaziou.

(defvar citarn--bibliography-cache nil
  "Cache for parsed bibliography files.
This is an association list following the pattern:
  (FILE-ID . ENTRIES)
FILE-ID is a cons cell (FILE . HASH), with FILE being the absolute file name of
the bibliography file, and HASH a hash of its contents.
ENTRIES is a hash table with citation references as keys and fields alist as
values.")

(defvar citarn--completion-cache (make-hash-table :test #'equal)
  "Hash with key as completion string, value as citekey.")

;; Internal functions

(defun citarn--all-keys ()
  "List all keys available in current bibliography."
  (seq-mapcat (pcase-lambda (`(,_ . ,entries))
                (map-keys entries))
              (citarn--parse-bibliography)))

(defun citarn--ref-completion-table ()
  "Return completion table for cite keys, as a hash table.
In this hash table, keys are a strings with author, date, and
title of the reference.  Values are the cite keys.
Return nil if there are no bibliography files or no entries."
  ;; Populate bibliography cache.
  (let ((entries (citarn--parse-bibliography)))
    (cond
     ((null entries) nil)  ; no bibliography files
     ((gethash entries citarn--completion-cache)
      citarn--completion-cache) ; REVIEW ?
     (t
      (clrhash citarn--completion-cache)
      (dolist (key (citarn--all-keys))
        (let ((completion (citarn--get-value key "title"))) ; TODO hook up string formatting
          (puthash completion key citarn--completion-cache)))
      (unless (map-empty-p citarn--completion-cache) ; no key
        (puthash entries t citarn--completion-cache) ; REVIEW ?
        citarn--completion-cache)))))

;; adapted from 'org-cite-basic--parse-bibliography'
(defvar citarn--file-id-cache nil
   "Hash table linking files to their hash.")

(defun citarn--parse-bibliography ()
  "List all entries available in the buffer.
Each association follows the pattern
  (FILE . ENTRIES)
where FILE is the absolute file name of the bibliography file,
and ENTRIES is a hash table where keys are references and values
are association lists between fields, as symbols, and values as
strings or nil."
  (unless (hash-table-p citarn--file-id-cache)
    (setq citarn--file-id-cache (make-hash-table :test #'equal)))
  (let ((results nil))
    ;; FIX the files to parse needs to be a function that returns the right
    ;; local and/or global bibliography files for the current buffer.
    (dolist (file citar-bibliography)
      (when (file-readable-p file)
        (with-temp-buffer
          (when (or (file-has-changed-p file)
                    (not (gethash file citarn--file-id-cache)))
            (insert-file-contents file)
            (puthash file (md5 (current-buffer)) citarn--file-id-cache))
	  (let* ((file-id (cons file (gethash file citarn--file-id-cache)))
                 (entries
                  (or (cdr (assoc file-id citarn--bibliography-cache))
                      (let ((table (parsebib-parse file)))
                        (push (cons file-id table) citarn--bibliography-cache)
                        table))))
            (push (cons file entries) results)))))
    results))

(defun citarn--get-entry (key)
  "Return entry for KEY, as an association list."
  (catch :found
    ;; Iterate through the cached bibliography hashes and find a key.
    (pcase-dolist (`(,_ . ,entries) (citarn--parse-bibliography))
      (let ((entry (gethash key entries)))
        (when entry (throw :found entry))))
    nil))

(defun citarn--get-value (key-or-entry field)
  "Return FIELD value for KEY-OR-ENTRY."
  (let ((entry (if (stringp key-or-entry)
                   (citarn--get-entry key-or-entry)
                 key-or-entry)))
    (cdr (assoc-string field entry))))

(defun citarn-select-ref ()
  "Select reference, return citekey."
  (let* ((table
          (or (citarn--ref-completion-table)
              (user-error "No bibliography set")))
         (choice (completing-read "Ref: " table)))
    (gethash choice table)))

;; Interactive commands

(defun citarn-example ()
  "Return title as message."
  (interactive)
  (let* ((choice (citarn-select-ref))
         (title (citarn--get-value choice "title")))
  (message title)))

(citarn-example)

(provide 'citarn)
;;; citarn.el ends here
