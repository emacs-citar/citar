;;; bibtex-actions-filenotify.el --- filenotify functions for bibtex-actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: August 17, 2021
;; Modified: August 17, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bruce/bibtex-actions-filenotify
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'filenotify)
(require 'files)
(require 'bibtex-actions)

(declare-function bibtex-actions-refresh "bibtex-actions")
(declare-function bibtex-actions--local-files-to-cache "bibtex-actions")
(declare-function bibtex-actions--global-files-to-cache "bibtex-actions")
(declare-function bibtex-actions-utils--normalize-paths "bibtex-actions-utils")
(declare-function reftex-access-scan-info "ext:reftex")
(declare-function file-notify-add-watch "ext:file-notify")
(declare-function file-notify-rm-watch "ext:file-notify")

(defvar bibtex-actions-bibliography)
(defvar bibtex-actions-library-paths)
(defvar bibtex-actions-notes-paths)
(defvar bibtex-actions-create-note-function)
(defvar bibtex-actions-filenotify-files)
(defvar bibtex-actions--candidates-cache)
(defvar bibtex-actions--local-candidates-cache)
(defvar bibtex-actions-filenotify-callback)
(defvar bibtex-actions-open-file-function)
(defvar bibtex-actions-file-extensions)

;;;; Variables


(defcustom bibtex-actions-filenotify-callback 'invalidate-cache
  "The callback that is run when the bibliography related files change.
Its value can be either 'invalidate-cache, 'refresh-cache or else a function.
The function takes two arguments. The first is the scope, which is `global' when
the changed file is in `bibtex-actions-filenotify-files' and `local' otherwise.
The second is the change that occured. This is the argument that the callback of
`file-notify-add-watch' accepts. This argument must be optional. The callback is
called without it when `bibtex-actions-filenotify-refresh' is run"
  :group 'bibtex-actions
  :type '(choice (const invalidate-cache)
                 (const refresh-cache)
                 function))

(defcustom bibtex-actions-filenotify-files '(bibliography)
  "The files to watch using filenotify."
  :group 'bibtex-actions
  :type '(repeat (choice (const bibliogrpahy)
                         (const library)
                         (const notes)
                         string)))


(defvar-local bibtex-actions-filenotify--local-watches 'uninitialized)
(defvar bibtex-actions-filenotify--global-watches nil)

;;; Filenotify functions

(defun bibtex-actions-filenotify--invalidate-cache (&optional scope)
  "Invalidate local or global caches according to SCOPE.
If it is other than 'global or 'local invalidate both"
  (unless (eq 'local scope)
    (setq bibtex-actions--candidates-cache 'uninitialized))
  (unless (eq 'glocal scope)
    (setq bibtex-actions--local-candidates-cache 'uninitialized)))

(defun bibtex-actions-filenotify--make-default-callback (func scope &optional change)
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

(defun bibtex-actions-filenotify--callback (scope &optional change)
  "A by SCOPE callback according to `bibtex-actions-filenotify-callback'.

This callback can be passed to the `file-notify-add-watch'.

CHANGE refers to the filenotify argument."
  (cl-case bibtex-actions-filenotify-callback
    (invalidate-cache (bibtex-actions-filenotify--make-default-callback
                       #'bibtex-actions-filenotify--invalidate-cache scope change))
    (refresh-cache (bibtex-actions-filenotify--make-default-callback
                    (lambda (x) (bibtex-actions-refresh nil x)) scope change))
    (t (funcall bibtex-actions-filenotify-callback scope change))))

(defun bibtex-actions-filenotify--add-local-watches ()
  "Add watches for the files that contribute to the local cache."
  (let ((buffer (buffer-name)))
    (setq bibtex-actions-filenotify--local-watches
          (seq-map
           (lambda (bibfile)
             (file-notify-add-watch
              bibfile '(change)
              (lambda (x)
                (with-current-buffer buffer
                  (bibtex-actions-filenotify--callback 'local x)))))
           (bibtex-actions--local-files-to-cache)))))

(defun bibtex-actions-filenotify-local-watches ()
  "Hook to add and remove watches on local bib files.

The watches are added only if `bibtex-actions--local-watches' has the
default value `uninitialized'. This is to ensure that duplicate
watches aren't added. This means a mode hook containing this
function can run several times without adding duplicate watches."
  (when (eq 'uninitialized bibtex-actions-filenotify--local-watches)
    (bibtex-actions-filenotify--add-local-watches))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (mapc #'file-notify-rm-watch bibtex-actions-filenotify--local-watches)
              (setq bibtex-actions-filenotify--local-watches 'uninitialized))
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
  (setq bibtex-actions-filenotify--global-watches
        (seq-map
         (lambda (bibfile)
           (file-notify-add-watch
            bibfile '(change)
            (lambda (x)
              (bibtex-actions-filenotify--callback 'global x))))
         (bibtex-actions--filenotify-files))))

(defun bibtex-actions-filenotify-rm-global-watches ()
  "Remove the watches on global bib files."
  (interactive)
  (mapc #'file-notify-rm-watch bibtex-actions-filenotify--global-watches)
  (setq bibtex-actions-filenotify--global-watches nil))

(defun bibtex-actions-filenotify-refresh (&optional scope)
  "Refresh the watches by SCOPE on the bib files.

This function only needs to be called if a bib file has been added or removed."
  (interactive)
  (unless (eq 'global scope)
    (seq-map #'file-notify-rm-watch bibtex-actions-filenotify--local-watches)
    (reftex-access-scan-info t)
    (bibtex-actions-filenotify--add-local-watches)
    (bibtex-actions-filenotify--callback 'local))
  (unless (eq 'local scope)
    (bibtex-actions-filenotify-rm-global-watches)
    (bibtex-actions-filenotify-global-watches)
    (bibtex-actions-filenotify--callback 'global)))

;;;; Interactive filenoify commands

;;;###autoload
(defun bibtex-actions-filenotify-setup (mode-hooks)
  "Setup filenotify watches for local and global bibliography related files.

This functions adds watches to the files in
`bibtex-actions-filenotify-files' and adds a hook to the
'major-mode' hooks in 'MODE-HOOKS' which adds watches for the
local bib files. These local watches are removed when the buffer
closes."
  (bibtex-actions-filenotify-global-watches)
  (mapc (lambda (mode)
          (add-hook mode #'bibtex-actions-filenotify-local-watches)) mode-hooks))

(provide 'bibtex-actions-filenotify)
;;; bibtex-actions-filenotify.el ends here
