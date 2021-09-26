;;; citar.el --- Bibliographic commands based on completing-read -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: February 27, 2021
;; License: GPL-3.0-or-later
;; Version: 0.4
;; Homepage: https://github.com/bdarcus/citar
;; Package-Requires: ((emacs "26.3") (bibtex-completion "1.0") (parsebib "3.0"))
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
;;  A completing-read front-end for browsing and acting on bibliographic data.
;;
;;  When used with vertico/selectrum/icomplete-vertical, embark, and marginalia,
;;  it provides similar functionality to helm-bibtex and ivy-bibtex: quick
;;  filtering and selecting of bibliographic entries from the minibuffer, and
;;  the option to run different commands against them.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'seq)
(require 'citar-file)
(require 'bibtex-completion)
(require 'parsebib)
(require 's)
;; Not ideal, find a better FIX
(require 'oc)

(declare-function org-element-context "org-element")
(declare-function org-element-property "org-element")
(declare-function org-element-type "org-element")
(declare-function org-cite-get-references "org-cite")
(declare-function org-cite-register-processor "org-cite")
(declare-function org-cite-make-insert-processor "org-cite")
(declare-function org-cite-basic--complete-style "org-cite")
(declare-function embark-act "ext:embark")

(declare-function reftex-access-scan-info "reftex")
(declare-function reftex-get-bibfile-list "reftex")
(declare-function TeX-current-macro "tex")

;;; Declare variables for byte compiler

(defvar crm-separator)
(defvar embark-keymap-alist)
(defvar embark-target-finders)
(defvar embark-general-map)
(defvar embark-meta-map)
(defvar citar-file-open-note-function)
(defvar citar-file-extensions)
(defvar citar-file-open-prompt)
(defvar citar-file-variable)

;;; Variables

(defface citar
  '((t :inherit font-lock-doc-face))
  "Default Face for `citar' candidates."
  :group 'citar)

(defface citar-highlight
  '((t :weight bold))
  "Face used to highlight content in `citar' candidates."
  :group 'citar)

(defcustom citar-bibliography nil
  "A list of bibliography files."
  :group 'citar
  :type '(repeat file))

(defcustom citar-library-paths nil
  "A list of files paths for related PDFs, etc."
  :group 'citar
  :type '(repeat path))

(defcustom citar-notes-paths nil
  "A list of file paths for bibliographic notes."
  :group 'citar
  :type '(repeat path))

(defcustom citar-templates
  '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
    (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
    (note . "#+title: Notes on ${author editor}, ${title}"))
  "Configures formatting for the bibliographic entry.

The main and suffix templates are for candidate display, and note
for the title field for new notes."
    :group 'citar
    :type  '(alist :key-type string))

(defcustom citar-display-transform-functions
  ;; TODO change this name, as it might be confusing?
  '((t  . citar-clean-string)
    (("author" "editor") . citar-shorten-names))
  "Configure transformation of field display values from raw values.

All functions that match a particular field are run in order."
  :group 'citar
  :type '(alist :key-type   (choice (const t) (repeat string))
                :value-type function))

(defcustom citar-symbols
  `((file  .  ("F" . " "))
    (note .   ("N" . " "))
    (link .   ("L" . " ")))
  "Configuration alist specifying which symbol or icon to pick for a bib entry.
This leaves room for configurations where the absense of an item
may be indicated with the same icon but a different face.

To avoid alignment issues make sure that both the car and cdr of a symbol have
the same width."
  :group 'citar
  :type '(alist :key-type string
                :value-type (choice (string :tag "Symbol"))))

(defcustom citar-symbol-separator " "
  "The padding between prefix symbols."
  :group 'citar
  :type 'string)

(defcustom citar-force-refresh-hook nil
  "Hook run when user forces a (re-) building of the candidates cache.
This hook is only called when the user explicitly requests the
cache to be rebuilt.  It is intended for 'heavy' operations which
recreate entire bibliography files using an external reference
manager like Zotero or JabRef."
  :group 'citar
  :type '(repeat function))

(defcustom citar-default-action 'citar-open
  "The default action for the `citar-at-point' command."
  :group 'citar
  :type 'function)

(defcustom citar-at-point-fallback 'prompt
  "Fallback action for `citar-at-point'.
The action is used when no citation key is found at point.
`prompt' means choosing entries via `citar-select-keys'
and nil means no action."
  :group 'citar
  :type '(choice (const :tag "Prompt" 'prompt)
                 (const :tag "Ignore" nil)))

(defcustom citar-open-note-function
  'citar-org-open-notes-default
  "Function to open and existing or create a new note.

A note function must take two arguments:

KEY: a string to represent the citekey
ENTRY: an alist with the structured data (title, author, etc.)

If you use 'org-roam' and 'org-roam-bibtex', you can use
'orb-bibtex-actions-edit-note' for this value."
  :group 'citar
  :type '(function))


(defcustom citar-at-point-function 'citar-dwim
  "The function to run for 'citar-at-point'."
  :group 'citar
  :type 'function)

(defcustom citar-major-mode-functions
  '(((latex-mode) .
     ((local-bib-files . citar-latex--local-bib-files)
      (keys-at-point . citar-latex--keys-at-point)))
    ((markdown-mode) .
     ((local-bib-files . citar-markdown--local-bib-files)
      (insert-keys . citar-markdown--insert-keys))))
  "The variable determining the major mode specifc functionality.

It is alist with keys being a list of major modes.

The value is an alist with values being functions to be used for
these modes while the keys are symbols used to lookup them up.
The keys are:

local-bib-files: the corresponding functions should return the list of
local bibliography files.

insert-keys: the corresponding function should insert the list of keys given
to as the argument at point in the buffer.

insert-citation: the corresponding function should insert a
complete citation from a list of keys at point.

keys-at-point: the corresponding function should return the list of keys at
point."
  :group 'citar
  :type '(alist :key-type (repeat string :tag "Major modes")
                :value-type (set (cons (const local-bib-files) function)
                                 (cons (const insert-keys) function)
                                 (cons (const insert-citation) function)
                                 (cons (const keys-at-pont) function))))

;;; History, including future history list.

(defvar citar-history nil
  "Search history for `citar'.")

(defcustom citar-presets nil
  "List of predefined searches."
  :group 'citar
  :type '(repeat string))

;;; Keymaps

;;;###autoload
(defcustom citar-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") '("add pdf attachment" . citar-add-pdf-attachment))
    (define-key map (kbd "a") '("add pdf to library" . citar-add-pdf-to-library))
    (define-key map (kbd "b") '("insert bibtex" . citar-insert-bibtex))
    (define-key map (kbd "c") '("insert citation" . citar-insert-citation))
    (define-key map (kbd "k") '("insert key" . citar-insert-key))
    (define-key map (kbd "fr") '("insert formatted reference" . citar-insert-reference))
    (define-key map (kbd "o") '("open source document" . citar-open))
    (define-key map (kbd "e") '("open bibtex entry" . citar-open-entry))
    (define-key map (kbd "l") '("open source URL or DOI" . citar-open-link))
    (define-key map (kbd "n") '("open notes" . citar-open-notes))
    (define-key map (kbd "f") '("open library files" . citar-open-library-files))
    (define-key map (kbd "r") '("refresh" . citar-refresh))
    ;; Embark doesn't currently use the menu description.
    ;; https://github.com/oantolin/embark/issues/251
    (define-key map (kbd "RET") '("default action" . citar-run-default-action))
    map)
  "Keymap for Embark minibuffer actions."
  :group 'oc-citar
  :type '(restricted-sexp :match-alternatives (keymapp)))

;;;###autoload
(defcustom citar-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") '("open source document" . citar-open))
    (define-key map (kbd "e") '("open bibtex entry" . citar-open-entry))
    (define-key map (kbd "l") '("open source URL or DOI" . citar-open-link))
    (define-key map (kbd "n") '("open notes" . citar-open-notes))
    (define-key map (kbd "f") '("open library files" . citar-open-library-files))
    (define-key map (kbd "r") '("refresh library" . citar-refresh))
    ;; Embark doesn't currently use the menu description.
    ;; https://github.com/oantolin/embark/issues/251
    (define-key map (kbd "RET") '("default action" . citar-run-default-action))
    map)
  "Keymap for Embark citation-key actions."
  :group 'citar
  :type '(restricted-sexp :match-alternatives (keymapp)))

;;; Completion functions

(cl-defun citar-select-refs (&optional &key rebuild-cache)
  "Select bibliographic references.

Provides a wrapper around 'completing-read-multiple, and returns
an alist of key-entry, where the entry is a field-value alist.

Therefore, for each returned candidate, 'car' is the citekey, and
'cdr' is an alist of structured data.

Includes the following optional argument:

'REBUILD-CACHE' if t, forces rebuilding the cache before
offering the selection candidates."
  (let* ((crm-separator "\\s-*&\\s-*")
         (candidates (citar--get-candidates rebuild-cache))
         (chosen
          (completing-read-multiple
           "References: "
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   (affixation-function . citar--affixation)
                   (category . bib-reference))
               (complete-with-action action candidates string predicate)))
           nil nil nil
           'citar-history citar-presets nil)))
    (seq-map
     (lambda (choice)
       ;; Collect citation key-entry of selected candidate(s).
       (or (cdr (assoc choice candidates))
           ;; When calling embark at-point, use keys to look up and return the
           ;; selected candidates.
           ;; See https://github.com/bdarcus/citar/issues/233#issuecomment-901536901
           (cdr (seq-find (lambda (cand) (equal choice (cadr cand))) candidates))))
     chosen)))

(defun citar-select-files (files)
  "Select file(s) from a list of FILES."
  ;; TODO add links to candidates
  (completing-read-multiple
   "Open related file(s): "
   (lambda (string predicate action)
     (if (eq action 'metadata)
         `(metadata
        ; (group-function . citar-select-group-related-sources)
           (category . file))
       (complete-with-action action files string predicate)))))

(defun citar-select-group-related-sources (file transform)
  "Group by FILE by source, TRANSFORM."
    (let ((extension (file-name-extension file)))
      (when transform file
        ;; Transform for grouping and group title display.
        (cond
         ((string= extension (or "org" "md")) "Notes")
          (t "Library Files")))))

(defun citar--major-mode-function (key &rest args)
  "Function for the major mode corresponding to KEY applied to ARGS."
  (apply (alist-get key (cdr (seq-find (lambda (x) (memq major-mode (car x)))
                                citar-major-mode-functions)))
         args))

(defun citar--local-files-to-cache ()
  "The local bibliographic files not included in the global bibliography."
  ;; We cache these locally to the buffer.
  (seq-difference (citar-file--normalize-paths
                   (citar--major-mode-function 'local-bib-files))
                  (citar-file--normalize-paths
                   citar-bibliography)))

(defun citar-get-value (field item)
  "Return the FIELD value for ITEM."
  (cdr (assoc-string field item 'case-fold)))

(defun citar-has-a-value (fields item)
  "Return the first field that has a value in ITEM among FIELDS ."
  (seq-find (lambda (field) (citar-get-value field item)) fields))

(defun citar-display-value (fields item)
  "Return the first non nil value for ITEM among FIELDS .

The value is transformed using `citar-display-transform-functions'"
  (let ((field (citar-has-a-value fields item)))
    (seq-reduce (lambda (string fun)
                  (if (or (eq t (car fun))
                          (member field (car fun)))
                      (funcall (cdr fun) string)
                    string))
                citar-display-transform-functions
            ;; Make sure we always return a string, even if empty.
                (or (citar-get-value field item) ""))))

;; Lifted from bibtex-completion
(defun citar-clean-string (s)
  "Remove quoting brackets and superfluous whitespace from string S."
  (replace-regexp-in-string "[\n\t ]+" " "
         (replace-regexp-in-string "[\"{}]+" "" s)))

(defun citar-shorten-names (names)
  "Return a list of family names from a list of full NAMES.

To better accomomodate corporate names, this will only shorten
personal names of the form 'family, given'."
  (when (stringp names)
    (mapconcat
     (lambda (name)
       (if (eq 1 (length name))
           (cdr (split-string name " "))
         (car (split-string name ", "))))
     (split-string names " and ") ", ")))

(defun citar--fields-for-format (template)
  "Return list of fields for TEMPLATE."
  ;; REVIEW I don't really like this code, but it works correctly.
  ;;        Would be good to at least refactor to remove s dependency.
  (let* ((fields-rx "${\\([^}]+\\)}")
         (raw-fields (seq-mapcat #'cdr (s-match-strings-all fields-rx template))))
    (seq-map
     (lambda (field)
       (car (split-string field ":")))
     (seq-mapcat (lambda (raw-field) (split-string raw-field " ")) raw-fields))))

(defun citar--fields-in-formats ()
  "Find the fields to mentioned in the templates."
  (seq-mapcat #'citar--fields-for-format
              (list (citar-get-template 'main)
                    (citar-get-template 'suffix)
                    (citar-get-template 'note))))

(defun citar--fields-to-parse ()
  "Determine the fields to parse from the template."
  (seq-concatenate
   'list
   (citar--fields-in-formats)
   (list "doi" "url" citar-file-variable)))

(defun citar--format-candidates (bib-files &optional context)
  "Format candidates from BIB-FILES, with optional hidden CONTEXT metadata.
This both propertizes the candidates for display, and grabs the
key associated with each one."
  (let* ((candidates ())
         (raw-candidates
          (parsebib-parse bib-files :fields (citar--fields-to-parse)))
         (main-width (citar--format-width (citar-get-template 'main)))
         (suffix-width (citar--format-width (citar-get-template 'suffix)))
         (symbols-width (string-width (citar--symbols-string t t t)))
         (star-width (- (frame-width) (+ 2 symbols-width main-width suffix-width))))
    (maphash
     (lambda (citekey entry)
       (let* ((files
               (when (citar-file--files-for-entry
                      citekey
                      entry
                      citar-library-paths
                      citar-file-extensions)
                 " has:files"))
              (notes
               (when (citar-file--files-for-entry
                      citekey
                      entry
                      citar-notes-paths
                      citar-file-extensions)
                 " has:notes"))
              (link
               (when (citar-has-a-value '("doi" "url") entry)
                 "has:link"))
              (candidate-main
               (citar--format-entry
                entry
                star-width
                (citar-get-template 'main)))
              (candidate-suffix
               (citar--format-entry
                entry
                star-width
                (citar-get-template 'suffix)))
              ;; We display this content already using symbols; here we add back
              ;; text to allow it to be searched, and citekey to ensure uniqueness
              ;; of the candidate.
              (candidate-hidden (string-join (list files notes link context citekey) " ")))
         (push
          (cons
           ;; If we don't trim the trailing whitespace,
           ;; 'completing-read-multiple' will get confused when there are
           ;; multiple selected candidates.
           (string-trim-right
            (concat
             ;; We need all of these searchable:
             ;;   1. the 'candidate-main' variable to be displayed
             ;;   2. the 'candidate-suffix' variable to be displayed with a different face
             ;;   3. the 'candidate-hidden' variable to be hidden
             (propertize candidate-main 'face 'citar-highlight) " "
             (propertize candidate-suffix 'face 'citar) " "
             (propertize candidate-hidden 'invisible t)))
           (cons citekey entry))
          candidates)))
       raw-candidates)
    candidates))

  (defun citar--affixation (cands)
    "Add affixation prefix to CANDS."
    (seq-map
     (lambda (candidate)
       (let ((candidate-symbols (citar--symbols-string
                                 (string-match "has:files" candidate)
                                 (string-match "has:note" candidate)
                                 (string-match "has:link" candidate))))
         (list candidate candidate-symbols "")))
     cands))

(defun citar--symbols-string (has-files has-note has-link)
  "String for display from booleans HAS-FILES HAS-LINK HAS-NOTE."
  (cl-flet ((thing-string (has-thing thing-symbol)
                          (if has-thing
                              (cadr (assoc thing-symbol citar-symbols))
                            (cddr (assoc thing-symbol citar-symbols)))))
    (seq-reduce (lambda (constructed newpart)
                  (let* ((str (concat constructed newpart
                                      citar-symbol-separator))
                         (pos (length str)))
                    (put-text-property (- pos 1) pos 'display
                                       (cons 'space (list :align-to (string-width str)))
                                       str)
                    str))
                (list (thing-string has-files 'file)
                      (thing-string has-note 'note)
                      (thing-string has-link 'link)
                      "")
                "")))

(defvar citar--candidates-cache 'uninitialized
  "Store the global candidates list.

Default value of 'uninitialized is used to indicate that cache
has not yet been created")

(defvar-local citar--local-candidates-cache 'uninitialized
  ;; We use defvar-local so can maintain per-buffer candidate caches.
  "Store the local (per-buffer) candidates list.")

;;;###autoload
(defun citar-refresh (&optional force-rebuild-cache scope)
  "Reload the candidates cache.

If called interactively with a prefix or if FORCE-REBUILD-CACHE
is non-nil, also run the `citar-before-refresh-hook' hook.

If SCOPE is `global' only global cache is refreshed, if it is
`local' only local cache is refreshed.  With any other value both
are refreshed."
  (interactive (list current-prefix-arg nil))
  (when force-rebuild-cache
    (run-hooks 'citar-force-refresh-hook))
  (unless (eq 'local scope)
    (setq citar--candidates-cache
      (citar--format-candidates
        (citar-file--normalize-paths citar-bibliography))))
  (unless (eq 'global scope)
    (setq citar--local-candidates-cache
          (citar--format-candidates
           (citar--local-files-to-cache) "is:local"))))

(defun citar-get-template (template-name)
  "Return template string for TEMPLATE-NAME."
  (cdr (assoc template-name citar-templates)))

(defun citar--get-candidates (&optional force-rebuild-cache)
  "Get the cached candidates.

If the cache is unintialized, this will load the cache.

If FORCE-REBUILD-CACHE is t, force reload the cache."
  (when force-rebuild-cache
    (citar-refresh force-rebuild-cache))
  (when (eq 'uninitialized citar--candidates-cache)
    (citar-refresh nil 'global))
  (when (eq 'uninitialized citar--local-candidates-cache)
    (citar-refresh nil 'local))
  (seq-concatenate 'list
                   citar--local-candidates-cache
                   citar--candidates-cache))

(defun citar--get-entry (key)
  "Return the cached entry for KEY."
    (cddr (seq-find
           (lambda (entry)
             (string-equal key (cadr entry)))
           (citar--get-candidates))))

(defun citar-get-link (entry)
  "Return a link for an ENTRY."
  (let* ((field (citar-has-a-value '(doi pmid pmcid url) entry))
         (base-url (pcase field
                     ('doi "https://doi.org/")
                     ('pmid "https://www.ncbi.nlm.nih.gov/pubmed/")
                     ('pmcid "https://www.ncbi.nlm.nih.gov/pmc/articles/"))))
    (when field
      (concat base-url (citar-get-value field entry)))))

(defun citar--extract-keys (keys-entries)
  "Extract list of keys from KEYS-ENTRIES alist."
  (seq-map #'car keys-entries))

;;;###autoload
(defun citar-insert-preset ()
  "Prompt for and insert a predefined search."
  (interactive)
  (unless (minibufferp)
    (user-error "Command can only be used in minibuffer"))
  (when-let ((enable-recursive-minibuffers t)
             (search (completing-read "Preset: " citar-presets)))
    (insert search)))

;;; Formatting functions

(defun citar--format-width (format-string)
  "Calculate minimal width needed by the FORMAT-STRING."
  (let ((content-width (apply #'+
                              (seq-map #'string-to-number
                                       (split-string format-string ":"))))
        (whitespace-width (string-width (s-format format-string
                                                  (lambda (_) "")))))
    (+ content-width whitespace-width)))

(defun citar--fit-to-width (value width)
  "Propertize the string VALUE so that only the WIDTH columns are visible."
  (let* ((truncated-value (truncate-string-to-width value width))
         (display-value (truncate-string-to-width truncated-value width 0 ?\s)))
    (if (> (string-width value) width)
        (concat display-value (propertize (substring value (length truncated-value))
                                          'invisible t))
      display-value)))

(defun citar--format-entry (entry width format-string)
  "Formats a BibTeX ENTRY for display in results list.
WIDTH is the width for the * field, and the display format is governed by
FORMAT-STRING."
  (s-format
   format-string
   (lambda (raw-field)
     (let* ((field (split-string raw-field ":"))
            (field-names (split-string (car field) "[ ]+"))
            (field-width (string-to-number (cadr field)))
            (display-width (if (> field-width 0)
                               ;; If user specifies field width of "*", use
                               ;; WIDTH; else use the explicit 'field-width'.
                               field-width
                             width))
            ;; Make sure we always return a string, even if empty.
            (display-value (citar-display-value field-names entry)))
       (citar--fit-to-width display-value display-width)))))

(defun citar--format-entry-no-widths (entry format-string)
  "Format ENTRY for display per FORMAT-STRING."
  (s-format
   format-string
   (lambda (raw-field)
     (let ((field-names (split-string raw-field "[ ]+")))
       (citar-display-value field-names entry)))))

;;; At-point functions

;;; Org-cite

(defun citar-get-key-org-cite ()
  "Return key at point for org-cite citation-reference."
  (when-let (((eq major-mode 'org-mode))
             (elt (org-element-context)))
    (pcase (org-element-type elt)
      ('citation-reference
       (org-element-property :key elt))
      ('citation
       (org-cite-get-references elt t)))))

(defun citar--insert-keys-org-cite (keys)
  "Insert KEYS in org-cite format."
  (string-join (seq-map (lambda (key) (concat "@" key)) keys) ":"))

;;; Embark

;;;###autoload
(defun citar-citation-key-at-point ()
  "Return citation keys at point as a list for `embark'."
  (when-let ((keys (or (citar-get-key-org-cite)
                      (bibtex-completion-key-at-point))))
    (cons 'citation-key (citar--stringify-keys keys))))

(defun citar--stringify-keys (keys)
  "Return a list of KEYS as a crm-string for `embark'."
  (if (listp keys) (string-join keys " & ") keys))

;;;###autoload
(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders 'citar-citation-key-at-point)
  (add-to-list 'embark-keymap-alist '(bib-reference . citar-map))
  (add-to-list 'embark-keymap-alist '(citation-key . citar-buffer-map)))

;;; Commands

;;;###autoload
(defun citar-open (keys-entries)
  "Open related resource (link or file) for KEYS-ENTRIES."
  ;; TODO add links
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (let* ((files
         (citar-file--files-for-multiple-entries
          keys-entries
          (append citar-library-paths citar-notes-paths)
          citar-file-extensions))
         (links
          (seq-map
           (lambda (key-entry)
             (citar-get-link (cdr key-entry)))
           keys-entries))
         (resource-candidates (delete-dups (append files (remq nil links))))
         (resources
          (when resource-candidates
            (completing-read-multiple "Related resources: " resource-candidates))))
    (if resource-candidates
        (dolist (resource resources)
          (cond ((string-match "http" resource 0)
                 (browse-url resource))
                (t (citar-file-open resource))))
      (message "No associated resources"))))

;;;###autoload
(defun citar-open-library-files (keys-entries)
 "Open library files associated with the KEYS-ENTRIES.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (let ((files
         (citar-file--files-for-multiple-entries
          keys-entries
          citar-library-paths
          citar-file-extensions)))
    (if citar-file-open-prompt
        (let ((selected-files
               (citar-select-files files)))
          (dolist (file selected-files)
            (citar-file-open file))))
      (dolist (file files)
        (citar-file-open file))))

(make-obsolete 'citar-open-pdf
               'citar-open-library-files "1.0")

;;;###autoload
(defun citar-open-notes (keys-entries)
  "Open notes associated with the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (when (and (null citar-notes-paths)
             (equal citar-file-open-note-function
                    'citar-file-open-notes-default-org))
    (message "You must set 'citar-notes-paths' to open notes with default notes function"))
  (dolist (key-entry keys-entries)
    (funcall citar-file-open-note-function
             (car key-entry) (cdr key-entry))))

;;;###autoload
(defun citar-open-entry (keys-entries)
  "Open bibliographic entry associated with the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
 (let ((bibtex-files (seq-concatenate 'list
                                      citar-bibliography
                                      (citar--local-files-to-cache))))
  (mapc (lambda (key) (bibtex-find-entry key t nil t)) (citar--extract-keys keys-entries))))

;;;###autoload
(defun citar-open-link (keys-entries)
  "Open URL or DOI link associated with the KEYS-ENTRIES in a browser.

With prefix, rebuild the cache before offering candidates."
  ;;      (browse-url-default-browser "https://google.com")
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (dolist (key-entry keys-entries)
    (let ((link (citar-get-link (cdr key-entry))))
      (if link
          (browse-url-default-browser link)
        (message "No link found for %s" key-entry)))))

;;;###autoload
(defun citar-insert-citation (keys-entries)
  "Insert citation for the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  ;; TODO
  (citar--major-mode-function 'insert-citation
   (citar--extract-keys
    keys-entries)))

;;;###autoload
(defun citar-insert-reference (keys-entries)
  "Insert formatted reference(s) associated with the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (bibtex-completion-insert-reference
   (citar--extract-keys
    keys-entries)))

;;;###autoload
(defun citar-insert-keys (keys-entries)
  "Insert KEYS-ENTRIES citekeys.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
 (citar--major-mode-function 'insert-keys
  (citar--extract-keys
   keys-entries)))

;;;###autoload
(defun citar-insert-bibtex (keys-entries)
  "Insert bibliographic entry associated with the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
 (bibtex-completion-insert-bibtex
  (citar--extract-keys
   keys-entries)))

;;;###autoload
(defun citar-add-pdf-attachment (keys-entries)
  "Attach PDF(s) associated with the KEYS-ENTRIES to email.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
 (bibtex-completion-add-PDF-attachment
  (citar--extract-keys
   keys-entries)))

;;;###autoload
(defun citar-add-pdf-to-library (keys-entries)
 "Add PDF associated with the KEYS-ENTRIES to library.
The PDF can be added either from an open buffer, a file, or a
URL.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (bibtex-completion-add-pdf-to-library
   (citar--extract-keys
    keys-entries)))

;;;###autoload
(defun citar-run-default-action (keys)
  "Run the default action `citar-default-action' on KEYS."
  (let* ((keys-parsed
          (if (stringp keys)
              (split-string keys " & ")
            (split-string (cdr keys) " & ")))
         (keys-entries
          (seq-map
           (lambda (key)
             (cons key (citar--get-entry key))) keys-parsed)))
    (funcall citar-default-action keys-entries)))

;;;###autoload
(defun citar-dwim ()
  "Run the default action on citation keys found at point."
  (interactive)
  (if-let ((keys (cdr (citar-citation-key-at-point))))
      ;; FIX how?
      (citar-run-default-action keys)))

(provide 'citar)
;;; citar.el ends here
