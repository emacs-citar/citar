;;; bibtex-actions.el --- Biblographic commands based on completing-read -*- lexical-binding: t; -*-
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
;;  A completing-read front-end to bibtex-completion.
;;
;;  This package turns bibtex-completion functions into completing-read-based
;;  Emacs commands.  When used with selectrum/icomplete-vertical, embark, and
;;  marginalia, it provides similar functionality to helm-bibtex and ivy-bibtex:
;;  quick filtering and selecting of bibliographic entries from the minibuffer,
;;  and the option to run different commands against them.
;;
;;; Code:

(require 'bibtex-completion)

(declare-function org-element-context "org-element")
(declare-function org-element-property "org-element")
(declare-function org-element-type "org-element")
(declare-function org-cite-get-references "org-cite")
(declare-function org-cite-register-processor "org-cite")
(declare-function org-cite-make-insert-processor "org-cite")
(declare-function org-cite-basic--complete-style "org-cite")
(declare-function embark-act "embark")

;;; Declare variables for byte compiler

(defvar crm-separator)
(defvar embark-keymap-alist)
(defvar embark-target-finders)
(defvar embark-general-map)
(defvar embark-meta-map)

;;; Variables

(defface bibtex-actions-suffix
  '((t :inherit completions-annotations))
  "Face used to highlight suffixes in `bibtex-actions' candidates."
  :group 'bibtex-actions)

(defcustom bibtex-actions-template
  '((t . "${author:20}   ${title:48}   ${year:4}"))
  "Configures formatting for the BibTeX entry.
When combined with the suffix, the same string is used for
display and for search."
    :group 'bibtex-actions
    :type  '(alist :key-type symbol :value-type function))

(defcustom bibtex-actions-template-suffix
  '((t . "          ${=key=:15}    ${=type=:12}    ${tags:*}"))
  "Configures formatting for the BibTeX entry suffix.
When combined with the main template, the same string is used for
display and for search."
    :group 'bibtex-actions
    :type  '(alist :key-type symbol :value-type function))

(defcustom bibtex-actions-org-cite-styles
  '("text" "noauthor" "title" "author" "locators" "nocite")
  "Org citation styles."
  :group 'bibtex-actions
  :type '(repeat string))

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

(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-actions--format-citation-org)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (python-mode   . bibtex-completion-format-citation-sphinxcontrib-bibtex)
    (rst-mode      . bibtex-completion-format-citation-sphinxcontrib-bibtex)
    (default       . bibtex-completion-format-citation-pandoc-citeproc)))

(defcustom bibtex-actions-force-refresh-hook nil
  "Hook run when user forces a (re-) building of the candidates cache.
This hook is only called when the user explicitly requests the
cache to be rebuilt.  It is intended for 'heavy' operations which
recreate entire bibliography files using an external reference
manager like Zotero or JabRef."
  :group 'bibtex-actions
  :type '(repeat function))

(defcustom bibtex-actions-initial-inputs
  '((pdf    . "has:pdf")
    (note   . nil)
    (link   . "has:link")
    (source . "has:link\\|has:pdf"))
  "Alist defining the initial input for some bibtex open actions.
Given a flexible completion style, this will restrict the list of
available candidates to those matching the initial input.

The association key can be one of the symbols `pdf', `note',
`link' or `source' and defines the input for the function
`bibtex-action-open-pdf', `bibtex-action-open-link', etc.  The
associated value must be nil, meaning that there will be no
initial input, or a string.

To match entries with certain properties (e.g. files attached),
you can use the following initial inputs: \"has:pdf\",
\"has:link\" and \"has:note\"."
  :group 'bibtex-actions
  :type '(alist :key-type symbol
                :value-type (choice string
                                    (const :tag "No initial input" nil))))

(defcustom bibtex-actions-default-action 'bibtex-actions-open
  "The default action for the `bibtex-actions-at-point' command."
  :group 'bibtex-actions
  :type 'function)

(defcustom bibtex-actions-at-point-fallback 'prompt
  "Fallback action for `bibtex-actions-at-point'.
The action is used when no citation key is found at point.
`prompt' means choosing entries via `bibtex-actions-read' and nil
means no action."
  :group 'bibtex-actions
  :type '(choice (const :tag "Prompt" 'prompt)
                 (const :tag "Ignore" nil)))

(defcustom bibtex-actions-embark-dwim t
  "Whether to run the default action on citation keys at point.
If non-nil, call the default embark action through `embark-dwim'.
If nil, prompt the user for an action through `embark-act'."
  :group 'bibtex-actions
  :type 'boolean)

;;; History, including future history list.

(defvar bibtex-actions-history nil
  "Search history for `bibtex-actions'.")

(defcustom bibtex-actions-presets nil
  "List of predefined searches."
  :group 'bibtex-actions
  :type '(repeat string))


;;; Keymap

(defvar bibtex-actions-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'bibtex-actions-add-pdf-attachment)
    (define-key map (kbd "a") 'bibtex-actions-add-pdf-to-library)
    (define-key map (kbd "b") 'bibtex-actions-insert-bibtex)
    (define-key map (kbd "c") 'bibtex-actions-insert-citation)
    (define-key map (kbd "k") 'bibtex-actions-insert-key)
    (define-key map (kbd "f") 'bibtex-actions-insert-reference)
    (define-key map (kbd "o") 'bibtex-actions-open)
    (define-key map (kbd "e") 'bibtex-actions-open-entry)
    (define-key map (kbd "l") 'bibtex-actions-open-link)
    (define-key map (kbd "n") 'bibtex-actions-open-notes)
    (define-key map (kbd "p") 'bibtex-actions-open-pdf)
    (define-key map (kbd "r") 'bibtex-actions-refresh)
    map)
  "Keymap for 'bibtex-actions'.")

;;; Org-cite citation function

(defun bibtex-actions--format-citation-org (keys)
  "Format org-cite citations for the entries in KEYS.

The full citation syntax is:

  [cite/style:common prefix ;prefix @key suffix; ... ; common suffix]

Everything is optional, except the brackets, 'cite' and the colon.
Also the citation must contain at least a key.
So its minimal form is:

  [cite:@key]

Note that the prefix here is for the citation as a whole; if you
need to add one specific to an individual citation item, you
would need to add that after inserting.

  [cite:see ;item prefix @key]

The same is true for suffixes like page numbers, which are
specific to the item, rather than the citation as a whole.

  [cite: see ;@key pp23-24]"
  (let* ((prefix  (if bibtex-completion-cite-prompt-for-optional-arguments (read-from-minibuffer "Prefix: ") ""))
         (styles bibtex-actions-org-cite-styles)
         (style  (if bibtex-completion-cite-prompt-for-optional-arguments
                     (ido-completing-read "Style: " styles nil nil nil nil "default")))
         (prefix  (if (string= "" prefix)  "" (concat prefix  " ;")))
         (style  (if (string-equal style "default") "" (concat "/" style))))
    ;; this is derived from the pandoc syntax function, but this has two-levels of affixes
    (format "[cite%s:%s%s]" style prefix (s-join ";" (--map (concat "@" it) keys)))))

;;; Completion functions
(cl-defun bibtex-actions-read (&optional &key initial rebuild-cache)
  "Read bibtex-completion entries.

This provides a wrapper around 'completing-read-multiple', with
the following optional arguments:

If 'INITIAL' matches one of the keys defined in
`bibtex-actions-initial-inputs', use the associated initial
input.

'REBUILD-CACHE' if t, forces rebuilding the cache before
offering the selection candidates"
  (let* ((crm-separator "\\s-*&\\s-*")
	 (candidates (bibtex-actions--get-candidates rebuild-cache))
         (initial-input (assoc-default initial bibtex-actions-initial-inputs))
         (chosen
          (completing-read-multiple
           "References: "
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   (affixation-function . bibtex-actions--affixation)
                   (category . bibtex))
               (complete-with-action action candidates string predicate)))
           nil
           nil
           (and initial-input
                (stringp initial-input)
                (concat initial-input " "))
           'bibtex-actions-history bibtex-actions-presets nil)))
    (cl-loop for choice in chosen
             ;; Collect citation keys of selected candidate(s).
             collect (cdr (or (assoc choice candidates)
                              (rassoc choice candidates))))))

(defun bibtex-actions--format-candidates ()
  "Transform candidates from 'bibtex-completion-candidates'.
This both propertizes the candidates for display, and grabs the
key associated with each one."
  (let* ((main-template
         (bibtex-actions--process-display-formats
          bibtex-actions-template))
         (suffix-template
          (bibtex-actions--process-display-formats
           bibtex-actions-template-suffix))
         (main-width (truncate (* (frame-width) 0.65)))
         (suffix-width (truncate (* (frame-width) 0.34))))
    (cl-loop
     for candidate in (bibtex-completion-candidates)
     collect
     (let* ((pdf (if (assoc "=has-pdf=" (cdr candidate)) " has:pdf"))
            (note (if (assoc "=has-note=" (cdr candidate)) "has:note"))
            (link (if (or (assoc "doi" (cdr candidate))
                          (assoc "url" (cdr candidate))) "has:link"))
            (citekey (bibtex-completion-get-value "=key=" candidate))
            (candidate-main
             (bibtex-actions--format-entry
              candidate
              main-width
              main-template))
            (candidate-suffix
             (bibtex-actions--format-entry
              candidate
              suffix-width
              suffix-template))
            ;; We display this content already using symbols; here we add back
            ;; text to allow it to be searched, and citekey to ensure uniqueness
            ;; of the candidate.
            (candidate-hidden (s-join " " (list pdf note link citekey))))
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
        citekey)))))

(defun bibtex-actions--affixation (cands)
  "Add affixation prefix to CANDS."
  (cl-loop
   for candidate in cands
   collect
   (let ((pdf (if (string-match "has:pdf" candidate)
                  (cadr (assoc 'pdf bibtex-actions-symbols))
                (cddr (assoc 'pdf bibtex-actions-symbols))))
         (link (if (string-match "has:link" candidate)
                   (cadr (assoc 'link bibtex-actions-symbols))
                 (cddr (assoc 'link bibtex-actions-symbols))))
         (note
          (if (string-match "has:note" candidate)
                  (cadr (assoc 'note bibtex-actions-symbols))
                (cddr (assoc 'note bibtex-actions-symbols))))
         (suffix ""))
   (list candidate (concat
                    (s-join bibtex-actions-symbol-separator
                            (list pdf note link))"	") suffix))))

(defvar bibtex-actions--candidates-cache nil
  "Store the candidates list.")

(defun bibtex-actions--get-candidates (&optional force-rebuild-cache)
  "Get the cached candidates.
If the cache is nil, this will load the cache.
If FORCE-REBUILD-CACHE is t, force reloading the cache."
  (when (or force-rebuild-cache
            (not bibtex-actions--candidates-cache))
    (bibtex-actions-refresh force-rebuild-cache))
  bibtex-actions--candidates-cache)

(defun bibtex-actions-complete-key-at-point ()
    "Complete org-cite or pandoc citation key at point."
    ; FIX
    (when (and (or (eq major-mode 'org-mode)
                   (eq major-mode 'markdown-mode))
               (eq ?@ (char-before)))
      (let* ((candidates (bibtex-actions--get-candidates))
             ;; set both begin and end to point so we use capf UI to narrow and
             ;; select
             (begin (point))
             (end (point)))
        (list begin end candidates
              :exit-function
              (lambda (str _status)
                ;; take completion str and replace with key
                (delete-char (- (length str)))
                (insert (cdr (assoc str candidates))))))))

;;;###autoload
(defun bibtex-actions-refresh (&optional force-rebuild-cache)
  "Reload the candidates cache.
If called interactively with a prefix or if FORCE-REBUILD-CACHE
is non-nil, also run the `bibtex-actions-before-refresh-hook' hook."
  (interactive "P")
  (when force-rebuild-cache
    (run-hooks 'bibtex-actions-force-refresh-hook))
  (setq bibtex-actions--candidates-cache
        (bibtex-actions--format-candidates)))

;;;###autoload
(defun bibtex-actions-insert-preset ()
  "Prompt for and insert a predefined search."
  (interactive)
  (unless (minibufferp)
    (user-error "Command can only be used in minibuffer"))
  (when-let ((enable-recursive-minibuffers t)
             (search (completing-read "Preset: " bibtex-actions-presets)))
    (insert search)))

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
            (s-format
             format-string
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
  (let* ((format
          (or
           ;; If there's a template specific to the type, use that.
           (assoc-string
            (bibtex-completion-get-value "=type=" entry) template 'case-fold)
           ;; Otherwise, use the generic template.
           (assoc t template)))
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
             (let ((width (if (> field-width 0)
                              ;; If user specifies field width of "*", use
                              ;; WIDTH; else use the explicit 'field-width'.
                              field-width
                            width)))
               (truncate-string-to-width field-value width 0 ?\s))))))))

;;; At-point functions

;;; Org-cite

;; This function will likely be removed if and when bibtex-completions adds
;; something equivalent.
(defun bibtex-actions-get-key-org-cite ()
  "Return key at point for org-cite citation-reference."
  (when-let (((eq major-mode 'org-mode))
             (elt (org-element-context)))
    (pcase (org-element-type elt)
      ('citation-reference
       (org-element-property :key elt))
      ('citation
       (org-cite-get-references elt t)))))

;;; Embark

(defun bibtex-actions-citation-key-at-point ()
  "Return citation keys at point as a list for `embark'."
  (when-let ((key (or (bibtex-actions-get-key-org-cite)
                      (bibtex-completion-key-at-point))))
    (cons 'citation-key (if (listp key) (string-join key " & ") key))))

(defvar bibtex-actions-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'bibtex-actions-add-pdf-attachment)
    (define-key map (kbd "a") 'bibtex-actions-add-pdf-to-library)
    (define-key map (kbd "o") 'bibtex-actions-open)
    (define-key map (kbd "e") 'bibtex-actions-open-entry)
    (define-key map (kbd "l") 'bibtex-actions-open-link)
    (define-key map (kbd "n") 'bibtex-actions-open-notes)
    (define-key map (kbd "p") 'bibtex-actions-open-pdf)
    (define-key map (kbd "RET") 'bibtex-actions-run-default-action)
    map)
  "Keymap for Embark citation-key actions.")

;;; Command wrappers for bibtex-completion functions

;;;###autoload
(defun bibtex-actions-open (keys)
 "Open PDF, or URL or DOI link.
Opens the PDF(s) associated with the KEYS.  If multiple PDFs are
found, ask for the one to open using ‘completing-read’.  If no
PDF is found, try to open a URL or DOI in the browser instead.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :initial 'source
					  :rebuild-cache current-prefix-arg)))
  (bibtex-completion-open-any keys))

;;;###autoload
(defun bibtex-actions-open-pdf (keys)
 "Open PDF associated with the KEYS.
If multiple PDFs are found, ask for the one to open using
‘completing-read’.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :initial 'pdf
					  :rebuild-cache current-prefix-arg)))
  (bibtex-completion-open-pdf keys))

;;;###autoload
(defun bibtex-actions-open-link (keys)
  "Open URL or DOI link associated with the KEYS in a browser.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :initial 'link
					  :rebuild-cache current-prefix-arg)))
 (bibtex-completion-open-url-or-doi keys))

;;;###autoload
(defun bibtex-actions-insert-citation (keys)
  "Insert citation for the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :rebuild-cache current-prefix-arg)))
 (bibtex-completion-insert-citation keys))

;;;###autoload
(defun bibtex-actions-insert-reference (keys)
  "Insert formatted reference(s) associated with the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :rebuild-cache current-prefix-arg)))
  (bibtex-completion-insert-reference keys))

;;;###autoload
(defun bibtex-actions-insert-key (keys)
  "Insert BibTeX KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :rebuild-cache current-prefix-arg)))
 (bibtex-completion-insert-key keys))

;;;###autoload
(defun bibtex-actions-insert-bibtex (keys)
  "Insert BibTeX entry associated with the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :rebuild-cache current-prefix-arg)))
 (bibtex-completion-insert-bibtex keys))

;;;###autoload
(defun bibtex-actions-add-pdf-attachment (keys)
  "Attach PDF(s) associated with the KEYS to email.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :rebuild-cache current-prefix-arg)))
 (bibtex-completion-add-PDF-attachment keys))

;;;###autoload
(defun bibtex-actions-open-notes (keys)
  "Open notes associated with the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :initial 'note
					  :rebuild-cache current-prefix-arg)))
 (bibtex-completion-edit-notes keys))

;;;###autoload
(defun bibtex-actions-open-entry (keys)
  "Open BibTeX entry associated with the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :rebuild-cache current-prefix-arg)))
 (bibtex-completion-show-entry keys))

;;;###autoload
(defun bibtex-actions-add-pdf-to-library (keys)
 "Add PDF associated with the KEYS to library.
The PDF can be added either from an open buffer, a file, or a
URL.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-read :rebuild-cache current-prefix-arg)))
  (bibtex-completion-add-pdf-to-library keys))

(defun bibtex-actions-run-default-action (keys)
  "Run the default action `bibtex-actions-default-action' on KEYS."
  (funcall bibtex-actions-default-action
           (if (stringp keys) (split-string keys " & ") keys)))

;;;###autoload
(defun bibtex-actions-at-point (&optional arg)
  "Run the default action on citation keys found at point.
If no citation key is found, target entries can be chosen
interactively when `bibtex-actions-at-point-fallback' is non-nil.
With prefix ARG, rebuild the cache before offering candidates."
  (interactive "P")
  (if (fboundp 'embark-dwim)
      (condition-case err
          (if bibtex-actions-embark-dwim
              (embark-dwim)
            (embark-act))
        (user-error
         (when (and (string-equal (error-message-string err) "No target found")
                    bibtex-actions-at-point-fallback)
           (bibtex-actions-run-default-action
            (bibtex-actions-read :rebuild-cache arg)))))
    (if-let ((keys (bibtex-actions-citation-key-at-point)))
        (funcall bibtex-actions-default-action keys)
      (when bibtex-actions-at-point-fallback
        (bibtex-actions-run-default-action
         (bibtex-actions-read :rebuild-cache arg))))))

(provide 'bibtex-actions)
;;; bibtex-actions.el ends here
