;;; citar-embark.el --- Embark support for citar -*- lexical-binding: t; -*-
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

;;; Commentary:

;;  Provides functions for integrating Embark and Citar.
;;
;;; Code:
(require 'embark)
(require 'citar)

;;;###autoload
(defun citar--embark-selected ()
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

(provide 'citar-embark)
;;; citar-embark.el ends here
