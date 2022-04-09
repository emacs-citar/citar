;;; citar-filenotify.el --- Filenotify functions for citar -*- lexical-binding: t; -*-
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
;; A companion to the citar for auto-invalidation and auto-refreshing
;; of cache when a bib file or a related file such notes directory or library
;; changes.  Uses filenotify api to acheive this.
;;
;;; Code:

(require 'filenotify)
(require 'files)
(require 'citar)

(declare-function citar-refresh "citar")
(declare-function citar--local-files-to-cache "citar")
(declare-function citar-file--normalize-paths "citar-file")
(declare-function reftex-access-scan-info "ext:reftex")
(declare-function file-notify-add-watch "ext:file-notify")
(declare-function file-notify-rm-watch "ext:file-notify")

;;;; Variables

(defcustom citar-filenotify-callback 'invalidate-cache
  "The callback that is run when the bibliography related files change.

Its value can be either 'invalidate-cache, 'refresh-cache or else a function.

The function takes two arguments.

The first is the scope, which is `global' when the changed file
is in `citar-filenotify-files' and `local' otherwise.

The second is the change that occured.  This is the argument that
the callback of `file-notify-add-watch' accepts, and is optional.
The callback is called without it when
`citar-filenotify-refresh' is run"
  :group 'citar
  :type '(choice (const invalidate-cache)
                 (const refresh-cache)
                 function))

(defcustom citar-filenotify-files '(bibliography)
  "The files to watch using filenotify."
  :group 'citar
  :type '(repeat (choice (const bibliogrpahy)
                         (const library)
                         (const notes)
                         string)))


(defvar-local citar-filenotify--local-watches 'uninitialized)
(defvar citar-filenotify--global-watches nil)

;;; Filenotify functions

(defun citar-filenotify--invalidate-cache (&optional scope)
  "Invalidate local or global caches according to SCOPE.
If it is other than 'global or 'local invalidate both"
  (unless (eq 'local scope)
    (setq citar--candidates-cache 'uninitialized))
  (unless (eq 'global scope)
    (setq citar--local-candidates-cache 'uninitialized)))

(defun citar-filenotify--make-default-callback (func scope &optional change)
  "The callback FUNC by SCOPE used to update cache for default options.

CHANGE refers to the notify argument."
  (pcase (cadr change)
    ((or 'nil 'changed) (funcall func scope))
    ((or 'created 'deleted 'renamed)
     (if (member
          (nth 2 change)
          (seq-concatenate 'list
                           (citar-file--normalize-paths citar-bibliography)
                           (citar--local-files-to-cache)))
         (citar-filenotify-refresh scope)
       (funcall func scope)))))

(defun citar-filenotify--callback (scope &optional change)
  "A by SCOPE callback according to `citar-filenotify-callback'.

This callback can be passed to the `file-notify-add-watch'.

CHANGE refers to the filenotify argument."
  (pcase citar-filenotify-callback
    ('invalidate-cache (citar-filenotify--make-default-callback
                        #'citar-filenotify--invalidate-cache scope change))
    ('refresh-cache (citar-filenotify--make-default-callback
                     (lambda (x) (citar-refresh nil x)) scope change))
    (_ (funcall citar-filenotify-callback scope change))))

(defun citar-filenotify--add-local-watches ()
  "Add watches for the files that contribute to the local cache."
  (let ((buffer (buffer-name)))
    (setq citar-filenotify--local-watches
          (seq-map
           (lambda (bibfile)
             (file-notify-add-watch
              bibfile '(change)
              (lambda (x)
                (with-current-buffer buffer
                  (citar-filenotify--callback 'local x)))))
           (citar--local-files-to-cache)))))

(defun  citar-filenotify-rm-local-watches ()
  "Delete the filenotify watches for the local bib files."
  (mapc #'file-notify-rm-watch citar-filenotify--local-watches)
  (setq citar-filenotify--local-watches 'uninitialized))

(defun citar-filenotify-local-watches ()
  "Hook to add and remove watches on local bib files.

The watches are added only if `citar--local-watches' has the
default value `uninitialized'.  This is to ensure that duplicate
watches aren't added.  This means a mode hook containing this
function can run several times without adding duplicate watches."
  (when (eq 'uninitialized citar-filenotify--local-watches)
    (citar-filenotify--add-local-watches))
  (add-hook 'kill-buffer-hook #'citar-filenotify-rm-local-watches nil t))

(defun citar-filenotify--files ()
  "Get the list of files to watch from `citar-filenotify-files'."
  (seq-mapcat (lambda (x)
                (citar-file--normalize-paths
                 (pcase x
                   ('bibliography citar-bibliography)
                   ('library citar-library-paths)
                   ('notes  citar-notes-paths)
                   (_ x))))
              citar-filenotify-files))

(defun citar-filenotify-global-watches ()
  "Add watches on the global files in `citar-filenotify-files'.

Unlike `citar-filenotify-local-watches' these
watches have to be removed manually.  To remove them call
`citar-rm-global-watches'"
  (setq citar-filenotify--global-watches
        (seq-map
         (lambda (bibfile)
           (file-notify-add-watch
            bibfile '(change)
            (lambda (x)
              (citar-filenotify--callback 'global x))))
         (citar-filenotify--files))))

(defun citar-filenotify-rm-global-watches ()
  "Remove the watches on global bib files."
  (interactive)
  (mapc #'file-notify-rm-watch citar-filenotify--global-watches)
  (setq citar-filenotify--global-watches nil))

(defun citar-filenotify-refresh (&optional scope)
  "Refresh the watches by SCOPE on the bib files.

This function only needs to be called if a bib file has been added or removed."
  (interactive)
  (unless (eq 'global scope)
    (seq-map #'file-notify-rm-watch citar-filenotify--local-watches)
    (reftex-access-scan-info t)
    (citar-filenotify--add-local-watches)
    (citar-filenotify--callback 'local))
  (unless (eq 'local scope)
    (citar-filenotify-rm-global-watches)
    (citar-filenotify-global-watches)
    (citar-filenotify--callback 'global)))

;;;; Interactive filenoify commands

;;;###autoload
(defun citar-filenotify-setup (mode-hooks)
  "Setup filenotify watches for local and global bibliography related files.

This functions adds watches to the files in
`citar-filenotify-files' and adds a hook to the
'major-mode' hooks in 'MODE-HOOKS' which adds watches for the
local bib files.  These local watches are removed when the buffer
closes."
  (citar-filenotify-global-watches)
  (dolist (mode mode-hooks)
    (add-hook mode #'citar-filenotify-local-watches)))

(provide 'citar-filenotify)
;;; citar-filenotify.el ends here
