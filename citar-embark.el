;;; citar-embark.el --- Integrate citar with embark    -*- lexical-binding: t; -*-
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
;;  Add embark functionality to citar.
;;
;;; Code:

(require 'citar)
(require 'embark)

;;;; Keymaps

(defvar citar-embark-map (make-composed-keymap citar-map embark-general-map)
  "Keymap for Embark actions on Citar references.")

;; TODO should this also inherit from `embark-general-map'?
(defvar citar-embark-citation-map (make-composed-keymap citar-citation-map nil)
  "Keymap for Embark actions on Citar citations and keys.")

;;; At-point functions for Embark

(defun citar-embark--key-finder ()
  "Return the citation key at point."
  (when-let (key (and (not (minibufferp))
                      (citar--major-mode-function 'key-at-point #'ignore)))
    (cons 'citar-key key)))

(defun citar-embark--citation-finder ()
  "Return the keys of the citation at point."
  (when-let (citation (and (not (minibufferp))
                           (citar--major-mode-function 'citation-at-point #'ignore)))
    `(citar-citation ,(citar--stringify-keys (car citation)) . ,(cdr citation))))

(defun citar-embark--candidate-transformer (_type target)
  "Look up key for a citar-reference TYPE and TARGET."
  (cons 'citar-reference (citar--extract-candidate-citekey target)))

(defun citar-embark--selected ()
  "Return selected candidates from `citar--select-multiple' for embark."
  (when-let (((eq minibuffer-history-variable 'citar-history))
             (metadata (embark--metadata))
             (group-function (completion-metadata-get metadata 'group-function))
             (cands (all-completions
                     "" minibuffer-completion-table
                     (lambda (cand)
                       (and (equal "Selected" (funcall group-function cand nil))
                            (or (not minibuffer-completion-predicate)
                                (funcall minibuffer-completion-predicate cand)))))))
    (cons (completion-metadata-get metadata 'category) cands)))

;;;###autoload
(define-minor-mode citar-embark-mode
  "Toggle Citar target finders for Embark."
  :group 'citar
  :global t
  :init-value nil
  :lighter nil
  (let ((targetfinders (list #'citar-embark--key-finder #'citar-embark--citation-finder))
        (collectors (list #'citar-embark--selected))
        (transformers (list (cons 'citar-candidate #'citar-embark--candidate-transformer)))
        (keymaps '((citar-reference . citar-embark-map)            ; minibuffer candidates
                   (citar-key . citar-embark-citation-map)         ; at-point keys
                   (citar-citation . citar-embark-citation-map)))
        (multitarget (list #'citar-insert-bibtex #'citar-insert-citation
                           #'citar-insert-reference #'citar-copy-reference
                           #'citar-insert-keys #'citar-run-default-action))
        (ignoretarget (list #'citar-insert-edit))) ; at-point citations
    (if citar-embark-mode
        (progn
          ;; Add target finders for `embark-act'
          (dolist (targetfinder (reverse targetfinders))
            (add-hook 'embark-target-finders targetfinder))

          ;; Add collectors for `embark-collect', `embark-act-all', etc.
          (dolist (collector (reverse collectors))
            (add-hook 'embark-candidate-collectors collector))

          ;; Add target transformers
          (dolist (transformer transformers)
            (setf (alist-get (car transformer) embark-transformer-alist) (cdr transformer)))

          ;; Add Embark keymaps
          (dolist (keymap keymaps)
            (setf (alist-get (car keymap) embark-keymap-alist) (cdr keymap)))

          ;; Mark commands as multitarget actions
          (dolist (command multitarget)
            (cl-pushnew command embark-multitarget-actions))

          ;; Mark commands as ignoring target
          (dolist (command ignoretarget)
            (cl-pushnew #'embark--ignore-target
                        (alist-get command (if (boundp 'embark-setup-action-hooks)
                                               ;; TODO Remove backward compatibility for Embark < 0.15?
                                               embark-setup-action-hooks
                                             embark-target-injection-hooks)))))
      ;; Disable citar-embark-mode:

      ;; Remove target finders
      (dolist (targetfinder targetfinders)
        (remove-hook 'embark-target-finders targetfinder))

      ;; Remove target collectors
      (dolist (collector collectors)
        (remove-hook 'embark-candidate-collectors collector))

      ;; Remove target transformers
      (dolist (transformer transformers)
        (cl-callf2 assq-delete-all (car transformer) embark-transformer-alist))

      ;; Remove Embark keymaps
      (dolist (keymap keymaps)
        (cl-callf2 assq-delete-all (car keymap) embark-transformer-alist))

      ;; Remove commands from embark-multitarget-actions
      (cl-callf cl-set-difference embark-multitarget-actions multitarget)

      ;; Remove #'embark--ignore-target setup hook
      (dolist (command ignoretarget)
        ;; TODO simplfy this when we drop compatibility with Embark < 0.15
        (cl-callf (lambda (hookalist)
                    (when-let ((alistentry (assq command hookalist)))
                      (cl-callf2 remq #'embark--ignore-target (cdr alistentry))
                      (unless (cdr alistentry) ; if no other hooks, remove alist entry
                        (cl-callf2 remq alistentry hookalist)))
                    hookalist)
            (if (boundp 'embark-setup-action-hooks)
                embark-setup-action-hooks
              embark-target-injection-hooks))))))

;;;###autoload
(with-eval-after-load 'citar
  (with-eval-after-load 'embark
    (citar-embark-mode)))

(provide 'citar-embark)
;;; citar-embark.el ends here
