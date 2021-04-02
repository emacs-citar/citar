;;; bibtex-actions.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: February 27, 2021
;; License: GPL-3.0-or-later
;; Version: 0.4
;; Homepage: https://github.com/bdarcus/bibtex-actions
;; Package-Requires: ((emacs "26.3") (bibtex-completion "1.0"))
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
;;  This package turns bibtex-completion functions into completing-read-based
;;  Emacs commands.  When used with selectrum/icomplete-vertical, embark, and
;;  marginalia, it provides similar functionality to helm-bibtex and ivy-bibtex:
;;  quick filtering and selecting of bibliographic entries from the minibuffer,
;;  and the option to run different commands against them.
;;
;;; Code:

(require 'bibtex-completion)

;;; Variables

;; REVIEW: is this the correct way to ensure we use the custom separator in
;;         'bibtex-actions--completing-read'?
(defvar crm-separator)

(defface bibtex-actions-suffix
  '((t :inherit completions-annotations))
  "Face used to highlight suffixes in `bibtex-actions' candidates."
  :group 'bibtex-actions)

(defcustom bibtex-actions-display-template
  '((t . "${author:20}   ${title:48}   ${year:4}"))
  "Configures display formatting for the BibTeX entry."
    :group 'bibtex-actions
    :type  '(alist :key-type symbol :value-type function))

(defcustom bibtex-actions-display-template-suffix
  '((t . "          ${=key=:15}    ${=type=:12}    ${tags:*}"))
  "Configures display formatting for the BibTeX entry suffix."
    :group 'bibtex-actions
    :type  '(alist :key-type symbol :value-type function))

(defcustom bibtex-actions-link-symbol "🔗"
  "Symbol to indicate a DOI or URL link is available for a publication.
This should be a single character."
  :group 'bibtex-actions
  :type 'string)

(defcustom bibtex-actions-symbols
  `((pdf  .     (,bibtex-completion-pdf-symbol . " "))
    (note .     (,bibtex-completion-notes-symbol . " "))
    (link .     (,bibtex-actions-link-symbol . " ")))
  "Configuration alist specifying which symbol or icon to pick for a bib entry.
This leaves room for configurations where the absense of an item
may be indicated with the same icon but a different face."
  :group 'bibtex-actions
  :type '(alist :key-type string
                :value-type (choice (string :tag "Symbol"))))

(defcustom bibtex-actions-symbol-separator " "
  "The padding between prefix symbols."
  :group 'bibtex-actions
  :type 'string)

;;; Keymap

(defvar bibtex-actions-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'bibtex-actions-open)
    (define-key map (kbd "p") 'bibtex-actions-open-pdf)
    (define-key map (kbd "l") 'bibtex-actions-open-link)
    (define-key map (kbd "c") 'bibtex-actions-insert-citation)
    (define-key map (kbd "r") 'bibtex-actions-insert-reference)
    (define-key map (kbd "k") 'bibtex-actions-insert-key)
    (define-key map (kbd "b") 'bibtex-actions-insert-bibtex)
    (define-key map (kbd "t") 'bibtex-actions-add-pdf-attachment)
    (define-key map (kbd "n") 'bibtex-actions-open-notes)
    (define-key map (kbd "e") 'bibtex-actions-open-entry)
    (define-key map (kbd "a") 'bibtex-actions-add-pdf-to-library)
    map)
  "Keymap for 'bibtex-actions'.")

;;; Completion functions
(defun bibtex-actions-read ()
  "Read bibtex-completion entries for completion using 'completing-read-multiple'."
  (bibtex-completion-init)
  (when-let ((crm-separator "\\s-*&\\s-*")
             (candidates (bibtex-actions--get-candidates))
             (chosen
              (completing-read-multiple
               "BibTeX entries: "
               (lambda (string predicate action)
                 (if (eq action 'metadata)
                     `(metadata
                       (affixation-function . bibtex-actions--affixation)
                       (category . bibtex))
                   (complete-with-action action candidates string predicate))))))
    (cl-loop for choice in chosen
             ;; Collect citation keys of selected candidate(s).
             collect (cdr (assoc choice candidates)))))

(defun bibtex-actions--get-candidates ()
  "Transform candidates from 'bibtex-completion-candidates'.
This both propertizes the candidates for display, and grabs the
key associated with each one."
  (cl-loop
   for candidate in (bibtex-completion-candidates)
   collect
   (let* ((pdf (if (assoc "=has-pdf=" (cdr candidate)) " has:pdf"))
          (note (if (assoc "=has-note=" (cdr candidate)) "has:note"))
          (link (if (assoc "doi" (cdr candidate)) "has:link"))
          (citekey (bibtex-completion-get-value "=key=" candidate))
          (candidate-main
           (bibtex-actions--format-entry
            candidate
            (1- (frame-width))
            bibtex-actions-display-template))
          (candidate-suffix
           (bibtex-actions--format-entry
            candidate
            (1- (frame-width))
            bibtex-actions-display-template-suffix))
          ;; We display this content already using symbols; here we add back
          ;; text to allow it to be searched.
          (candidate-hidden (s-trim-right (s-join " " (list pdf note link)))))
   (cons
    ;; If we don't trim the trailing whitespace, 'completing-read-multiple' will
    ;; get confused when there are multiple selected candidates.
    (s-trim-right
     (concat
      ;; We need all of these searchable:
      ;;   1. the 'candidate-main' variable to be displayed
      ;;   2. the 'candidate-suffix' variable to be displayed with a different face
      ;;   3. the 'candidate-hidden' variable to be hidden
      (propertize candidate-main) " "
      (propertize candidate-suffix 'face 'bibtex-actions-suffix) " "
      (propertize candidate-hidden 'invisible t)))
    citekey))))

(defun bibtex-actions--affixation (cands)
  "Add affixation prefix to CANDS."
  (cl-loop
   for candidate in cands
   collect
   (let ((pdf (if (string-match "has:pdf" candidate)
                  (cadr (assoc 'pdf bibtex-actions-symbols))
                (cddr (assoc 'pdf bibtex-actions-symbols))))
         ;(link (if (string-match "has:link" candidate)
         ;         (cadr (assoc 'link bibtex-actions-symbols))
         ;       (cddr (assoc 'link bibtex-actions-symbols))))
         (note
          (if (string-match "has:note" candidate)
                  (cadr (assoc 'note bibtex-actions-symbols))
                (cddr (assoc 'note bibtex-actions-symbols))))
         (suffix ""))
   (list candidate (concat
                    (s-join bibtex-actions-symbol-separator
                            (list pdf note))"	") suffix))))

;;; Formatting functions
;;  NOTE this section will be removed, or dramatically simplified, if and
;;  when this PR is merged:
;;    https://github.com/tmalsburg/helm-bibtex/pull/367

(defun bibtex-actions--process-display-formats (formats)
  "Pre-calculate minimal widths needed by the FORMATS strings for various entry types."
  ;; Adapted from bibtex-completion.
  (cl-loop
   for format in formats
   collect
   (let* ((format-string (cdr format))
          (fields-width 0)
          (string-width
           (string-width
            (s-format format-string
                      (lambda (field)
                        (setq fields-width
                              (+ fields-width
                                 (string-to-number
                                  (or (cadr (split-string field ":"))
                                      ""))))
                        "")))))
     (-cons* (car format) format-string (+ fields-width string-width)))))

(defun bibtex-actions--format-entry (entry width template)
  "Formats a BibTeX ENTRY for display in results list.
WIDTH is the width of the results list, and the display format is governed by
TEMPLATE."
  ;; Adapted from bibtex-completion.
  (let* ((processed-template
          (bibtex-actions--process-display-formats template))
         (format
          (or
           ;; If there's a template specific to the type, use that.
           (assoc-string
            (bibtex-completion-get-value "=type=" entry) processed-template 'case-fold)
           ;; Otherwise, use the generic template.
           (assoc t processed-template)))
         (format-string (cadr format)))
    (s-format
     format-string
     (lambda (field)
       (let* ((field (split-string field ":"))
              (field-name (car field))
              (field-width (cadr field))
              (field-value (bibtex-completion-get-value field-name entry)))
         (when (and (string= field-name "author")
                    (not field-value))
           (setq field-value (bibtex-completion-get-value "editor" entry)))
         (when (and (string= field-name "year")
                    (not field-value))
           (setq field-value (car (split-string (bibtex-completion-get-value "date" entry "") "-"))))
         (setq field-value (bibtex-completion-clean-string (or field-value " ")))
         (when (member field-name '("author" "editor"))
           (setq field-value (bibtex-completion-shorten-authors field-value)))
         (if (not field-width)
             field-value
           (setq field-width (string-to-number field-width))
           (truncate-string-to-width
            field-value
            (if (> field-width 0)
                field-width
              (- width (cddr format)))
            0 ?\s)))))))

;;; Command wrappers for bibtex-completion functions

(defun bibtex-actions-open (keys)
 "Open PDF, or URL or DOI link.
Opens the PDF(s) associated with the KEYS.  If multiple PDFs are
found, ask for the one to open using ‘completing-read’.  If no
PDF is found, try to open a URL or DOI in the browser instead."
  (interactive (list (bibtex-actions-read)))
  (bibtex-completion-open-any keys))

(defun bibtex-actions-open-pdf (keys)
 "Open PDF associated with the KEYS.
If multiple PDFs are found, ask for the one to open using
‘completing-read’."
  (interactive (list (bibtex-actions-read)))
  (bibtex-completion-open-pdf keys))

(defun bibtex-actions-open-link (keys)
 "Open URL or DOI link associated with the KEYS in a browser."
 (interactive (list (bibtex-actions-read)))
 (bibtex-completion-open-url-or-doi keys))

(defun bibtex-actions-insert-citation (keys)
 "Insert citation for the KEYS."
 (interactive (list (bibtex-actions-read)))
 (bibtex-completion-insert-citation keys))

(defun bibtex-actions-insert-reference (keys)
 "Insert formatted reference(s) associated with the KEYS."
  (interactive (list (bibtex-actions-read)))
  (bibtex-completion-insert-reference keys))

(defun bibtex-actions-insert-key (keys)
 "Insert BibTeX KEYS."
 (interactive (list (bibtex-actions-read)))
 (bibtex-completion-insert-key keys))

(defun bibtex-actions-insert-bibtex (keys)
 "Insert BibTeX entry associated with the KEYS."
 (interactive (list (bibtex-actions-read)))
 (bibtex-completion-insert-bibtex keys))

(defun bibtex-actions-add-pdf-attachment (keys)
 "Attach PDF(s) associated with the KEYS to email."
 (interactive (list (bibtex-actions-read)))
 (bibtex-completion-add-PDF-attachment keys))

(defun bibtex-actions-open-notes (keys)
 "Open notes associated with the KEYS."
 (interactive (list (bibtex-actions-read)))
 (bibtex-completion-edit-notes keys))

(defun bibtex-actions-open-entry (keys)
 "Open BibTeX entry associated with the KEYS."
 (interactive (list (bibtex-actions-read)))
 (bibtex-completion-show-entry keys))

(defun bibtex-actions-add-pdf-to-library (keys)
 "Add PDF associated with the KEYS to library.
The PDF can be added either from an open buffer, a file, or a
URL."
  (interactive (list (bibtex-actions-read)))
  (bibtex-completion-add-pdf-to-library keys))

(provide 'bibtex-actions)
;;; bibtex-actions.el ends here
