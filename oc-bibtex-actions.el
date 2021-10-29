;;; oc-bibtex-actions.el --- Org-cite support for bibtex-actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: July 11, 2021
;; License: GPL-3.0-or-later
;; Version: 0.4
;; Homepage: https://github.com/bdarcus/bibtex-actions
;; Package-Requires: ((emacs "26.3")(org "9.5"))
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
;;  This is a small package that intergrates bibtex-actions and org-cite.  It
;;  provides a simple org-cite processor with "follow" and "insert" capabilties.
;;
;;  Simply load this file and it will configure them for 'org-cite.'
;;
;;; Code:

(require 'bibtex-actions)
(require 'org)
(require 'oc)
(require 'oc-basic)
(require 'oc-csl)
(require 'embark)

(declare-function bibtex-actions-at-point "bibtex-actions")
(declare-function org-open-at-point "org")
(declare-function org-element-property "org")
(declare-function org-element-type "org")
(declare-function org-cite-make-insert-processor "oc")
(declare-function org-cite-get-references "oc")

(defface oc-bibtex-actions-style-preview
  ;; Not sure if this is the best parent face.
    '((t :inherit bibtex-actions))
  "Face for org-cite previews."
  :group 'oc-bibtex-actions)

(defcustom oc-bibtex-actions-styles-format 'long
  "Style format; whether to use full style names or shortcuts."
  :group 'oc-bibtex-actions
  :type '(choice
          (const long)
          (const short)))

(defcustom oc-bibtex-actions-style-targets nil
  "Export processor targets to include in styles list.

If nil, use 'org-cite-supported-styles'."
  :group 'oc-bibtex-actions
  :type '(repeat :tag "org-cite export processor" symbol))

(defcustom oc-bibtex-actions-activation-functions
  '(org-cite-basic-activate
    oc-bibtex-actions-activate-keymap)
  "List of activation functions for a citation.
Each function takes one argument, a citation."
  :group 'oc-bibtex-actions
  :type '(repeat function))

;;; Keymaps

(defcustom oc-bibtex-actions-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") '("open source (file or link)" . bibtex-actions-open))
    (define-key map (kbd "e") '("open bibtex entry" . bibtex-actions-open-entry))
    (define-key map (kbd "f") '("open source file" . bibtex-actions-open-library-files))
    (define-key map (kbd "l") '("open source link" . bibtex-actions-open-link))
    (define-key map (kbd "n") '("open notes" . bibtex-actions-open-notes))
    (define-key map (kbd "r") '("refresh" . bibtex-actions-refresh))
    map)
  "Keymap for org-cite Embark minibuffer functionality."
  :group 'oc-bibtex-actions
  :type '(restricted-sexp :match-alternatives (keymapp)))

(defcustom oc-bibtex-actions-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") '("open source (file or link)" . bibtex-actions-open))
    (define-key map (kbd "e") '("open bibtex entry" . bibtex-actions-open-entry))
    (define-key map (kbd "f") '("open source file" . bibtex-actions-open-library-files))
    (define-key map (kbd "l") '("open source link" . bibtex-actions-open-link))
    (define-key map (kbd "n") '("open notes" . bibtex-actions-open-notes))
    (define-key map (kbd "r") '("refresh" . bibtex-actions-refresh))
    map)
  "Keymap for org-cite Embark at-point functionality."
  :group 'oc-bibtex-actions
  :type '(restricted-sexp :match-alternatives (keymapp)))

(defcustom oc-bibtex-actions-citation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") '("default action" . bibtex-actions-dwim))
    (define-key map (kbd "<mouse-3>") '("embark act" . embark-act))
    (define-key map (kbd "C-d") '("delete citation" . oc-bibtex-actions-delete-citation))
    (define-key map (kbd "C-k") '("kill citation" . oc-bibtex-actions-kill-citation))
    (define-key map (kbd "S-<left>") '("shift left" . oc-bibtex-actions-shift-reference-left))
    (define-key map (kbd "S-<right>") '("shift right" . oc-bibtex-actions-shift-reference-right))
    (define-key map (kbd "C-p") '("update prefix/suffix" . oc-bibtex-actions-update-pre-suffix))
    map)
  "Keymap for interacting with org citations at point."
  :group 'oc-bibtex-actions
  :type '(restricted-sexp :match-alternatives (keymapp)))

;; Add keymaps to list.
(defvar bibtex-actions-keymaps)

(dolist (km '(oc-bibtex-actions-map oc-bibtex-actions-citation-map))
  (push km bibtex-actions-keymaps))

;; TODO maybe connvert to defcustoms. But this is not really the right approach;
;; better to just run the export processors to get the previews. But we need
;; citation context for that, or some other solution to have a citation to
;; process.

(defvar oc-bibtex-actions-style-preview-alist
  '(("/" . "(de Villiers et al, 2019)")
    ("/b" . "de Villiers et al, 2019")
    ("/c" . "(De Villiers et al, 2019)")
    ("/bc" . "de Villiers et al, 2019")
    ;; "text" style.
    ("text" . "de Villiers et al (2019)")
    ("text/c" . "De Villiers et al (2019)")
    ("text/f" . "de Villiers, Smith, Doa, and Jones (2019)")
    ("text/cf" . "De Villiers, Smith, Doa, and Jones (2019)")
    ;; "author" style.
    ("author" . "de Villiers et al")
    ("author/c" . "De Villiers et al")
    ("author/f" . "de Villiers, Smith, Doa, and Jones")
    ("author/cf" . "De Villiers, Smith, Doa, and Jones")
    ;; "locators" style.
    ("locators" . "(p23)")
    ("locators" . "p23")
    ;; "noauthor" style.
    ("noauthor" . "(2019)")
    ("noauthor/b" . "2019")))

(defun oc-bibtex-actions--style-candidates (&optional proc)
  "Return a flat list of supported styles.

Convert 'org-cite-supported-styles' to a flat list for use as
completion candidates.

With PROC list, limit to specific processor(s)."
  (let ((styles (list)))
    (dolist (style-variants (org-cite-supported-styles proc))
      (seq-let (style &rest variants) style-variants
        (let ((style-name (if (string= "nil" (car style)) "" (car style))))
          (push (propertize
                 (if (string= "" style-name) "/" style-name) 'face 'bibtex-actions-highlight) styles)
          (dolist (variant variants)
            (let ((fstyle
                   (concat style-name "/" (cadr variant))))
              (push (propertize fstyle 'face 'bibtex-actions) styles))))))
      styles))

;;; Org-cite processors

;; NOTE I may move some or all of these to a separate project

;;;###autoload
(defun oc-bibtex-actions-insert (&optional multiple)
  "Return a list of keys when MULTIPLE, or else a key string."
  (let ((references (bibtex-actions--extract-keys
                     (bibtex-actions-select-refs))))
    (if multiple
        references
      (car references))))

;;;###autoload
(defun oc-bibtex-actions-follow (_datum _arg)
  "Follow processor for org-cite."
  (call-interactively bibtex-actions-at-point-function))

;;;###autoload
(defun oc-bibtex-actions-select-style ()
  "Complete a citation style for org-cite with preview."
  (let* ((oc-styles
          ;; Sort the list upfront, but let completion UI handle beyond that.
          (sort (oc-bibtex-actions--style-candidates) 'string-lessp))
         (style
          (completing-read
           "Styles: "
           (lambda (str pred action)
             (if (eq action 'metadata)
                 `(metadata
                   (annotation-function . oc-bibtex-actions--style-preview-annote)
                   (group-function . oc-bibtex-actions--styles-group-fn))
               (complete-with-action action oc-styles str pred)))))
         (style-final (string-trim style)))
    (if (string= style-final "/") "" style-final)))

(defun oc-bibtex-actions--styles-group-fn (style transform)
  "Return group title of STYLE or TRANSFORM the candidate.
This is a group-function that groups org-cite style/variant
strings by style."
    (let* ((style-str (string-trim style))
           (short-style
            (if (string-match "^/[bcf]*" style-str) "default"
              (car (split-string style-str "/")))))
    (if transform
        ;; Use the candidate string as is, but add back whitespace alignment.
        (concat "  " (truncate-string-to-width style-str 20 nil 32))
      ;; Transform for grouping and display.
      (pcase short-style
       ("author" "Author-Only")
       ("locators" "Locators-Only")
       ("text" "Textual/Narrative")
       ("nocite" "No Cite")
       ("year" "Year-Only")
       ("noauthor" "Suppress Author")
       (_ (upcase-initials short-style))))))

(defun oc-bibtex-actions--style-preview-annote (style &optional _citation)
  "Annotate STYLE with CITATION preview."
  ;; TODO rather than use the alist, run the export processors on the citation..
  (let* ((preview (or (cdr (assoc style oc-bibtex-actions-style-preview-alist)) ""))
         ;; TODO look at how define-face does this.
         (formatted-preview (truncate-string-to-width preview 50 nil 32)))
    (propertize formatted-preview 'face 'oc-bibtex-actions-style-preview)))

;;; Embark target finder

(defun oc-bibtex-actions-citation-finder ()
  "Return org-cite citation keys at point as a list for `embark'."
  (when-let ((keys (bibtex-actions-get-key-org-cite)))
    (cons 'oc-citation (bibtex-actions--stringify-keys keys))))

;;; Functions for editing/modifying citations

;; most of this section is adapted from org-ref-cite

(defun oc-bibtex-actions-activate-keymap (citation)
  "Activation function for CITATION to add keymap and tooltip."
  (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
    ;; Put the keymap on a citation
    (put-text-property beg end 'keymap oc-bibtex-actions-citation-map)))

(defun oc-bibtex-actions--get-ref-index (refs ref)
  "Return index of citation-reference REF within REFS."
  (seq-position refs ref
                (lambda (r1 r2)
                  (and (string= (org-element-property :key r1)
                                (org-element-property :key r2))))))

(defun oc-bibtex-actions-delete-citation ()
  "Delete the citation or citation-reference at point."
  (interactive)
  (org-cite-delete-citation (org-element-context)))

(defun oc-bibtex-actions-kill-citation ()
  "Kill (copy) the citation or citation-reference at point."
  (interactive)
  (let* ((datum (org-element-context)))
    (kill-region (org-element-property :begin datum) (org-element-property :end datum))))

(defun oc-bibtex-actions-cite-swap (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)

(defun oc-bibtex-actions--shift-reference (datum direction)
  "When point is on a citation-reference DATUM, shift it in DIRECTION."
  (let*  ((current-citation (if (eq 'citation (org-element-type datum)) datum
                             (org-element-property :parent datum)))
          (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
          (refs (org-cite-get-references current-citation))
          (index
           (oc-bibtex-actions--get-ref-index refs current-ref)))

    (when (= 1 (length refs))
      (error "You only have one reference; you cannot shift this"))
    (when (null index)
      (error "Nothing to shift here"))
    (setf (buffer-substring (org-element-property :contents-begin current-citation)
                            (org-element-property :contents-end current-citation))
          (org-element-interpret-data
           (oc-bibtex-actions-cite-swap
            index
            (if (eq 'left direction) (- index 1) (+ index 1)) refs)))
    ;; Now get on the original ref.
    (let* ((newrefs (org-cite-get-references current-citation))
           (index
            (oc-bibtex-actions--get-ref-index newrefs current-ref)))

      (goto-char (org-element-property :begin (nth index newrefs))))))

(defun oc-bibtex-actions-shift-reference-left ()
  "When point is on a citation-reference, shift it left."
  (interactive)
  (let ((datum (org-element-context)))
    (oc-bibtex-actions--shift-reference datum 'left)))

(defun oc-bibtex-actions-shift-reference-right ()
  "When point is on a citation-reference, shift it right."
  (interactive)
  (let ((datum (org-element-context)))
    (oc-bibtex-actions--shift-reference datum 'right)))

(defun oc-bibtex-actions-update-pre-suffix ()
  "Change the pre/suffix text of the reference at point."
  ;; TODO I want this to also work for global affixes on the citation,
  ;;      but haven't figured that out yet.
  (interactive)
  (let* ((datum (org-element-context))
         (datum-type (org-element-type datum))
         (ref (if (eq datum-type 'citation-reference) datum
                (error "Not on a citation reference")))
         (key (org-element-property :key ref))
         ;; TODO handle space delimiter elegantly.
         (pre (read-string "Prefix text: " (org-element-property :prefix ref)))
         (post (read-string "Suffix text: " (org-element-property :suffix ref))))
    (setf (buffer-substring (org-element-property :begin ref)
                            (org-element-property :end ref))
          (org-element-interpret-data
           `(citation-reference
             (:key ,key :prefix ,pre :suffix ,post))))))

;; Embark configuration for org-cite

(add-to-list 'embark-target-finders 'oc-bibtex-actions-citation-finder)
(add-to-list 'embark-keymap-alist '(bib-reference . oc-bibtex-actions-map))
(add-to-list 'embark-keymap-alist '(oc-citation . oc-bibtex-actions-buffer-map))
(when (boundp 'embark-pre-action-hooks)
  ;; Ensure that Embark ignores the target for 'org-cite-insert'.
  (add-to-list 'embark-pre-action-hooks '(org-cite-insert embark--ignore-target)))

;; Load this last.

;;;###autoload
(defun oc-bibtex-actions-activate (citation)
  "Run all the activation functions in `oc-bibtex-actions-activation-functions'.
Argument CITATION is an org-element holding the references."
  (dolist (activate-func oc-bibtex-actions-activation-functions)
    (funcall activate-func citation)))

(org-cite-register-processor 'oc-bibtex-actions
  :insert (org-cite-make-insert-processor
           #'oc-bibtex-actions-insert
           #'oc-bibtex-actions-select-style)
  :follow #'oc-bibtex-actions-follow
  :activate #'oc-bibtex-actions-activate)

(provide 'oc-bibtex-actions)
;;; oc-bibtex-actions.el ends here
