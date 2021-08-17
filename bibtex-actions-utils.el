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

(cl-defun bibtex-actions-open-files
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

;;; File watching

(defvar-local bibtex-actions--local-watches 'uninitialized)
(defvar bibtex-actions--global-watches nil)

(defun bibtex-actions--invalidate-cache (&optional scope)
  "Invalidate local or global caches according to SCOPE.
If it is other than 'global or 'local invalidate both"
  (unless (eq 'local scope)
    (setq bibtex-actions--candidates-cache 'uninitialized))
  (unless (eq 'glocal scope)
    (setq bibtex-actions--local-candidates-cache 'uninitialized)))

(defun bibtex-actions--make-default-callback (func scope &optional change)
  "The callback FUNC by SCOPE used to update cache for default options.

CHANGE refers to the notify argument."
  (cl-case (cadr change)
    ((nil changed) (funcall func scope))
    ((created deleted renamed) (if (member (nth 2 change)
                                           (seq-concatenate 'list
                                                            bibtex-actions-bibliography
                                                            (bibtex-actions--local-files-to-cache)))
                                   (bibtex-actions-filenotify-refresh scope)
                                 (funcall func scope)))))

(defun bibtex-actions--filenotify-callback (scope &optional change)
  "A by SCOPE callback according to `bibtex-actions-filenotify-callback'.

This callback can be passed to the `file-notify-add-watch'.

CHANGE refers to the filenotify argument."
  (cl-case bibtex-actions-filenotify-callback
    (invalidate-cache (bibtex-actions--make-default-callback
                       #'bibtex-actions--invalidate-cache scope change))
    (refresh-cache (bibtex-actions--make-default-callback
                    (lambda (x) (bibtex-actions-refresh nil x)) scope change))
    (t (funcall bibtex-actions-filenotify-callback scope change))))

(defun bibtex-actions--add-local-watches ()
  "Add watches for the files that contribute to the local cache."
  (let ((buffer (buffer-name)))
    (setq bibtex-actions--local-watches
          (seq-map
           (lambda (bibfile)
             (file-notify-add-watch
              bibfile '(change)
              (lambda (x)
                (with-current-buffer buffer
                  (bibtex-actions--filenotify-callback 'local x)))))
           (bibtex-actions--local-files-to-cache)))))

(defun bibtex-actions-filenotify-local-watches ()
  "Hook to add and remove watches on local bib files.

The watches are added only if `bibtex-actions--local-watches' has the
default value `uninitialized'. This is to ensure that duplicate
watches aren't added. This means a mode hook containing this
function can run several times without adding duplicate watches."
  (when (eq 'uninitialized bibtex-actions--local-watches)
    (bibtex-actions--add-local-watches))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (mapc #'file-notify-rm-watch bibtex-actions--local-watches)
              (setq bibtex-actions--local-watches 'uninitialized))
            nil t))

(defun bibtex-actions--filenotify-files ()
  "Get the list of files to watch from `bibtex-actions-filenotify-files'."
  (seq-mapcat (lambda (x)
                (bibtex-actions-utils--normalize-paths
                 (cl-case x
                   (bibliography bibtex-actions-bibliography)
                   (library bibtex-actions-library-paths)
                   (notes  bibtex-actions-notes-paths)
                   (t x))))
              bibtex-actions-filenotify-files))

(defun bibtex-actions-filenotify-global-watches ()
  "Add watches on the global files in `bibtex-actions-filenotify-files'.

Unlike `bibtex-actions-filenotify-local-watches' these
watches have to be removed manually. To remove them call
`bibtex-actions-rm-global-watches'"
  (setq bibtex-actions--global-watches
        (seq-map
         (lambda (bibfile)
           (file-notify-add-watch
            bibfile '(change)
            (lambda (x)
              (bibtex-actions--filenotify-callback 'global x))))
         (bibtex-actions--filenotify-files))))

(defun bibtex-actions-rm-global-watches ()
  "Remove the watches on global bib files."
  (interactive)
  (mapc #'file-notify-rm-watch bibtex-actions--global-watches)
  (setq bibtex-actions--global-watches nil))

(defun bibtex-actions-filenotify-refresh (&optional scope)
  "Refresh the watches by SCOPE on the bib files.

This function only needs to be called if a bib file has been added or removed."
  (interactive)
  (unless (eq 'global scope)
    (seq-map #'file-notify-rm-watch bibtex-actions--local-watches)
    (reftex-access-scan-info t)
    (bibtex-actions--add-local-watches)
    (bibtex-actions--filenotify-callback 'local))
  (unless (eq 'local scope)
    (bibtex-actions-rm-global-watches)
    (bibtex-actions-filenotify-global-watches)
    (bibtex-actions--filenotify-callback 'global)))

(provide 'bibtex-actions-utils)
;;; bibtex-actions-utils.el ends here
