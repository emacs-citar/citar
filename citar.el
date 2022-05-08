;;; citar.el --- Citation-related commands for org, latex, markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Bruce D'Arcus

;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: February 27, 2021
;; License: GPL-3.0-or-later
;; Version: 0.9
;; Homepage: https://github.com/bdarcus/citar
;; Package-Requires: ((emacs "27.1") (parsebib "3.0") (org "9.5") (citeproc "0.9"))

;; This file is not part of GNU Emacs.

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

;;  A completing-read front-end to browse, filter and act on BibTeX, BibLaTeX,
;;  and CSL JSON bibliographic data, including LaTeX, markdown, and org-cite
;;  citation editing support.
;;
;;  With embark, it also provides access to contextual actions, both in the
;;  minibuffer, and in the buffer at-point.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'seq)
(require 'browse-url)
(require 'citar-file)
(require 'parsebib)
(require 'crm)

;;; Declare variables and functions for byte compiler

(defvar embark-keymap-alist)
(defvar embark-target-finders)
(defvar embark-pre-action-hooks)
(defvar embark-general-map)
(defvar embark-meta-map)
(defvar embark-transformer-alist)
(defvar embark-multitarget-actions)
(defvar embark-default-action-overrides)

(declare-function 'embark--target-buffer "ext:embark")

;;; Variables

(defgroup citar nil
  "Citations and bibliography management."
  :group 'editing)

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
  :type '(repeat directory))

(defcustom citar-notes-paths nil
  "A list of file paths for bibliographic notes."
  :group 'citar
  :type '(repeat directory))

(defcustom citar-additional-fields '("doi" "url" "crossref")
  "A list of fields to add to parsed data.

By default, citar filters parsed data based on the fields
specified in 'citar-templates'. This specifies additional fields
to include."
  :group 'citar
  :type '(repeat string))

(defcustom citar-templates
  '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
    (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
    (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
    (note . "Notes on ${author editor}, ${title}"))
  "Configures formatting for the bibliographic entry.

The main and suffix templates are for candidate display, and note
for the title field for new notes."
    :group 'citar
    :type  '(alist :key-type symbol
                   :value-type string
                   :options (main suffix preview note)))

(defcustom citar-format-reference-function
  #'citar-format-reference
  "Function used to render formatted references.

This function is called by 'citar-insert-reference' and
'citar-copy-reference'. The default value,
'citar-format-reference', formats references using the 'preview'
template set in 'citar-template'. To use 'citeproc-el' to format
references according to CSL styles, set the value to
'citar-citeproc-format-reference'. Alternatively, set to a custom
function that takes a list of (KEY . ENTRY) and returns formatted
references as a string."
  :group 'citar
  :type '(choice (function-item :tag "Use 'citar-template'" citar-format-reference)
                 (function-item :tag "Use 'citeproc-el'" citar-citeproc-format-reference)
                 (function :tag "Other")))

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
  :type '(alist :key-type symbol
                :value-type (cons (string :tag "Present")
                                  (string :tag "Absent"))
                :options (file note link)))

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
  :type 'hook)

(defcustom citar-default-action #'citar-open
  "The default action for the `citar-at-point' command.
Should be a function that takes one argument, a list with each
entry being either a citation KEY or a (KEY . ENTRY) pair."
  :group 'citar
  :type 'function)

(defcustom citar-at-point-fallback 'prompt
  "Fallback action for `citar-at-point'.
The action is used when no citation key is found at point.
`prompt' means choosing entries via `citar-select-keys'
and nil means no action."
  :group 'citar
  :type '(radio (const :tag "Prompt" prompt)
                (const :tag "Ignore" nil)))

(defcustom citar-open-prompt t
  "Always prompt for selection files with 'citar-open'.
If nil, single resources will open without prompting."
  :group 'citar
  :type '(boolean))

(defcustom citar-open-note-function
  'citar--open-note
  "Function to open a new or existing note.

A note function must take two arguments:

KEY: a string to represent the citekey
ENTRY: an alist with the structured data (title, author, etc.)"
  :group 'citar
  :type 'function)

(defcustom citar-format-note-function
  'citar-org-format-note-default
  "Function to format a new note.

A note function must take three arguments:

KEY: a string to represent the citekey
ENTRY: an alist with the structured data (title, author, etc.)
FILEPATH: the file name."
  :group 'citar
  :type 'function)

(defcustom citar-at-point-function #'citar-dwim
  "The function to run for 'citar-at-point'."
  :group 'citar
  :type 'function)

(defcustom citar-major-mode-functions
  '(((org-mode) .
     ((local-bib-files . citar-org-local-bib-files)
      (insert-citation . citar-org-insert-citation)
      (insert-edit . citar-org-insert-edit)
      (key-at-point . citar-org-key-at-point)
      (citation-at-point . citar-org-citation-at-point)
      (list-keys . citar-org-list-keys)))
    ((latex-mode) .
     ((local-bib-files . citar-latex-local-bib-files)
      (insert-citation . citar-latex-insert-citation)
      (insert-edit . citar-latex-insert-edit)
      (key-at-point . citar-latex-key-at-point)
      (citation-at-point . citar-latex-citation-at-point)
      (list-keys . reftex-all-used-citation-keys)))
    ((markdown-mode) .
     ((insert-keys . citar-markdown-insert-keys)
      (insert-citation . citar-markdown-insert-citation)
      (insert-edit . citar-markdown-insert-edit)
      (key-at-point . citar-markdown-key-at-point)
      (citation-at-point . citar-markdown-citation-at-point)
      (list-keys . citar-markdown-list-keys)))
    (t .
       ((insert-keys . citar--insert-keys-comma-separated))))
  "The variable determining the major mode specific functionality.

It is alist with keys being a list of major modes.

The value is an alist with values being functions to be used for
these modes while the keys are symbols used to lookup them up.
The keys are:

local-bib-files: the corresponding functions should return the list of
local bibliography files.

insert-keys: the corresponding function should insert the list of keys given
to as the argument at point in the buffer.

insert-citation: the corresponding function should insert a
complete citation from a list of keys at point.  If the point is
in a citation, new keys should be added to the citation.

insert-edit: the corresponding function should accept an optional
prefix argument and interactively edit the citation or key at
point.

key-at-point: the corresponding function should return the
citation key at point or nil if there is none.  The return value
should be (KEY . BOUNDS), where KEY is a string and BOUNDS is a
pair of buffer positions indicating the start and end of the key.

citation-at-point: the corresponding function should return the
keys of the citation at point, or nil if there is none.  The
return value should be (KEYS . BOUNDS), where KEYS is a list of
strings and BOUNDS is pair of buffer positions indicating the
start and end of the citation.

list-keys: the corresponding function should return the keys
of all citations in the current buffer."
  :group 'citar
  :type 'alist)

;;; History, including future history list.

(defvar citar-history nil
  "Search history for `citar'.")

(defcustom citar-presets nil
  "List of predefined searches."
  :group 'citar
  :type '(repeat string))

;;; Keymaps

(defvar citar-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'citar-insert-citation)
    (define-key map (kbd "k") #'citar-insert-keys)
    (define-key map (kbd "r") #'citar-copy-reference)
    (define-key map (kbd "R") #'citar-insert-reference)
    (define-key map (kbd "b") #'citar-insert-bibtex)
    (define-key map (kbd "o") #'citar-open)
    (define-key map (kbd "e") #'citar-open-entry)
    (define-key map (kbd "l") #'citar-open-link)
    (define-key map (kbd "n") #'citar-open-notes)
    (define-key map (kbd "f") #'citar-open-library-file)
    (define-key map (kbd "RET") #'citar-run-default-action)
    map)
  "Keymap for Embark minibuffer actions.")

(defvar citar-citation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'citar-insert-edit)
    (define-key map (kbd "o") #'citar-open)
    (define-key map (kbd "e") #'citar-open-entry)
    (define-key map (kbd "l") #'citar-open-link)
    (define-key map (kbd "n") #'citar-open-notes)
    (define-key map (kbd "f") #'citar-open-library-file)
    (define-key map (kbd "r") #'citar-copy-reference)
    (define-key map (kbd "RET") #'citar-run-default-action)
    map)
  "Keymap for Embark citation-key actions.")

;;; Completion functions

(defcustom citar-select-multiple t
  "Use `completing-read-multiple' for selecting citation keys.
When nil, all citar commands will use `completing-read`."
  :type 'boolean
  :group 'citar)

(defun citar--completion-table (candidates &optional filter)
  "Return a completion table for CANDIDATES.

CANDIDATES is an alist with entries (CAND KEY . ENTRY), where
  CAND is a display string for the bibliography item given
  by (KEY . ENTRY).

FILTER, if non-nil, should be a predicate function taking
  arguments KEY and ENTRY.  Only candidates for which this
  function returns non-nil will be offered for completion.

The returned completion table can be used with `completing-read`
and other completion functions."
  (let ((metadata `(metadata (category . citar-reference)
                             (affixation-function . ,#'citar--affixation))))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          metadata
        (let ((predicate
               (when (or filter predicate)
                 (lambda (cand-key-entry)
                   (pcase-let ((`(,cand ,key . ,entry) cand-key-entry))
                     (and (or (null filter) (funcall filter key entry))
                          (or (null predicate) (funcall predicate cand))))))))
          (complete-with-action action candidates string predicate))))))

(cl-defun citar-select-ref (&optional &key rebuild-cache multiple filter)
  "Select bibliographic references.

A wrapper around 'completing-read' that returns (KEY . ENTRY),
where ENTRY is a field-value alist.  Therefore 'car' of the
return value is the cite key, and 'cdr' is an alist of structured
data.

Takes the following optional keyword arguments:

REBUILD-CACHE: if t, forces rebuilding the cache before offering
  the selection candidates.

MULTIPLE: if t, calls `completing-read-multiple` and returns an
  alist of (KEY . ENTRY) pairs.

FILTER: if non-nil, should be a predicate function taking
  arguments KEY and ENTRY.  Only candidates for which this
  function returns non-nil will be offered for completion.  For
  example:

  (citar-select-ref :filter (citar-has-file))

  (citar-select-ref :filter (citar-has-note))

  (citar-select-ref
   :filter (lambda (_key entry)
             (when-let ((keywords (assoc-default \"keywords\" entry)))
               (string-match-p \"foo\" keywords))))"
  (let* ((candidates (citar--get-candidates rebuild-cache))
         (completions (citar--completion-table candidates filter))
         (crm-separator "\\s-*&\\s-*")
         (chosen (if (and multiple citar-select-multiple)
                     (completing-read-multiple "References: " completions nil nil nil
                                               'citar-history citar-presets nil)
                   (completing-read "Reference: " completions nil nil nil
                                    'citar-history citar-presets nil)))
         (notfound nil)
         (keyentries
          (seq-mapcat
           ;; Find citation key-entry of selected candidate.
           ;; CHOICE is either the formatted candidate string, or the citation
           ;; key when called through `embark-act`.  To handle both cases, test
           ;; CHOICE against the first two elements of the entries of
           ;; CANDIDATES.  See
           ;; https://github.com/bdarcus/citar/issues/233#issuecomment-901536901
           (lambda (choice)
             (if-let ((cand (seq-find
                             (lambda (cand) (member choice (seq-take cand 2)))
                             candidates)))
                 (list (cdr cand))
               ;; If not found, add CHOICE to NOTFOUND and return nil
               (push choice notfound)
               nil))
           (if (listp chosen) chosen (list chosen)))))
    (when notfound
      (message "Keys not found: %s" (mapconcat #'identity notfound "; ")))
    (if multiple keyentries (car keyentries))))

(cl-defun citar-select-refs (&optional &key rebuild-cache filter)
  "Select bibliographic references.

Call 'citar-select-ref' with argument :multiple; see its
documentation for the return value and the meaning of
REBUILD-CACHE and FILTER."
  (citar-select-ref :rebuild-cache rebuild-cache :multiple t :filter filter))

(defun citar-select-resource (files &optional links)
  "Select resource from a list of FILES, and optionally LINKS."
  (let* ((files (mapcar
                 (lambda (cand)
                   (abbreviate-file-name cand))
                 files))
         (resources (append files (remq nil links))))
    (dolist (item resources)
      (cond ((string-match "http" item 0)
             (push (propertize item 'multi-category `(url . ,item)) resources))
            (t
             (push (propertize item 'multi-category `(file . ,item)) resources))))
    (completing-read
     "Select resource: "
     (lambda (string predicate action)
       (if (eq action 'metadata)
           `(metadata
             (group-function . citar-select-group-related-resources)
             (category . multi-category))
         (complete-with-action action (delete-dups resources) string predicate))))))

(defun citar-select-group-related-resources (resource transform)
  "Group RESOURCE by type or TRANSFORM."
    (let ((extension (file-name-extension resource)))
      (if transform
          resource
        (cond
         ((member extension citar-file-note-extensions) "Notes")
         ((string-match "http" resource 0) "Links")
         (t "Library Files")))))

(defun citar--get-major-mode-function (key &optional default)
  "Return  function associated with KEY in 'major-mode-functions'.
If no function is found matching KEY for the current major mode,
return DEFAULT."
  (alist-get
   key
   (cdr (seq-find
         (lambda (modefns)
           (let ((modes (car modefns)))
             (or (eq t modes)
                 (apply #'derived-mode-p (if (listp modes) modes (list modes))))))
         citar-major-mode-functions))
   default))

(defun citar--major-mode-function (key default &rest args)
  "Function for the major mode corresponding to KEY applied to ARGS.
If no function is found, the DEFAULT function is called."
  (apply (citar--get-major-mode-function key default) args))

(defun citar--local-files-to-cache ()
  "The local bibliographic files not included in the global bibliography."
  ;; We cache these locally to the buffer.
  (seq-difference (citar-file--normalize-paths
                   (citar--major-mode-function 'local-bib-files #'ignore))
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
  (let* ((regexp "\\(?:\\`\\|}\\|:\\)[^{]*\\(?:\\${\\|\\'\\)\\|[[:space:]]+"))
    ;; The readable version of regexp is:
    ;; (rx (or (seq (or bos "}" ":") (0+ (not "{")) (or "${" eos)) (1+ space)))
    (split-string template regexp t)))

(defun citar--fields-in-formats ()
  "Find the fields to mentioned in the templates."
  (seq-mapcat #'citar--fields-for-format
              (list (citar-get-template 'main)
                    (citar-get-template 'suffix)
                    (citar-get-template 'preview)
                    (citar-get-template 'note))))

(defun citar--fields-to-parse ()
  "Determine the fields to parse from the template."
  (seq-concatenate
   'list
   (citar--fields-in-formats)
   (list citar-file-variable)
   citar-additional-fields))

(defun citar-has-file ()
  "Return predicate testing whether entry has associated files.

Return a function that takes arguments KEY and ENTRY and returns
non-nil when the entry has associated files, either in
`citar-library-paths` or the field named in
`citar-file-variable`.

Note: for performance reasons, this function should be called
once per command; the function it returns can be called
repeatedly."
  (citar-file--has-file citar-library-paths
                        citar-file-extensions
                        citar-file-variable))

(defun citar-has-note ()
  "Return predicate testing whether entry has associated notes.

Return a function that takes arguments KEY and ENTRY and returns
non-nil when the entry has associated notes in `citar-notes-paths`.

Note: for performance reasons, this function should be called
once per command; the function it returns can be called
repeatedly."
  (citar-file--has-file citar-notes-paths
                        citar-file-note-extensions))

(defun citar--format-candidates (bib-files &optional context)
  "Format candidates from BIB-FILES, with optional hidden CONTEXT metadata.
This both propertizes the candidates for display, and grabs the
key associated with each one."
  (let* ((candidates nil)
         (raw-candidates
          (parsebib-parse bib-files :fields (citar--fields-to-parse)))
         (hasfilep (citar-has-file))
         (hasnotep (citar-has-note))
         (main-width (citar--format-width (citar-get-template 'main)))
         (suffix-width (citar--format-width (citar-get-template 'suffix)))
         (symbols-width (string-width (citar--symbols-string t t t)))
         (star-width (- (frame-width) (+ 2 symbols-width main-width suffix-width))))
    (maphash
     (lambda (citekey entry)
       (let* ((files (when (funcall hasfilep citekey entry) " has:files"))
              (notes (when (funcall hasnotep citekey entry) " has:notes"))
              (link (when (citar-has-a-value '("doi" "url") entry) "has:link"))
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
                                 (string-match "has:notes" candidate)
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
  (let ((template
         (cdr (assoc template-name citar-templates))))
    (unless template
      (error "No template for \"%s\" - check variable 'citar-templates'" template-name))
    template))

(defun citar--get-candidates (&optional force-rebuild-cache)
  "Get the cached candidates.

If the cache is unintialized, this will load the cache.

If FORCE-REBUILD-CACHE is t, force reload the cache."
  (unless (or citar-bibliography (citar--local-files-to-cache))
    (error "Make sure to set citar-bibliography and related paths"))
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
  "Extract list of keys from KEYS-ENTRIES.

Each element of KEYS-ENTRIES should be either a (KEY . ENTRY)
pair or a string KEYS.

- If it is a (KEY . ENTRY) pair, it is replaced by KEY in the
  returned list.

- Otherwise, it should be a string KEYS consisting of multiple
  keys separated by \" & \".  The string is split and the
  separated keys are included in the returned list.

Return a list containing only KEY strings."
  (seq-mapcat
   (lambda (key-entry)
     (if (consp key-entry)
         (list (car key-entry))
       (split-string key-entry " & ")))
   keys-entries))

(defun citar--ensure-entries (keys-entries)
  "Return copy of KEYS-ENTRIES with every element a (KEY . ENTRY) pair.

Each element of KEYS-ENTRIES should be either a (KEY . ENTRY)
pair or a string KEYS.

- If it is a (KEY . ENTRY) pair, it is included in the returned
  list.

- Otherwise, it should be a string KEYS consisting of multiple
  keys separated by \" & \".  Look up the corresponding ENTRY for
  each KEY and, if found, include the (KEY . ENTRY) pairs in the
  returned list.

Return a list containing only (KEY . ENTRY) pairs."
  (if (seq-every-p #'consp keys-entries)
      keys-entries
    ;; Get candidates only if some key has a missing entry, to avoid nasty
    ;; recursion issues like https://github.com/bdarcus/citar/issues/286. Also
    ;; avoids lots of memory allocation in the common case when all entries are
    ;; present.
    (let ((candidates (citar--get-candidates)))
      (seq-mapcat
       (lambda (key-entry)
         (if (consp key-entry)
             (list key-entry)
           (seq-remove    ; remove keys not found in CANDIDATES
            #'null
            (seq-map
             (lambda (key)
               (cdr (seq-find (lambda (cand-key-entry)
                                (string= key (cadr cand-key-entry)))
                              candidates)))
             (split-string key-entry " & ")))))
       keys-entries))))

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
        (whitespace-width (string-width (citar--format format-string
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

(defun citar--format (template replacer)
  "Format TEMPLATE with the function REPLACER.
The templates are of form ${foo} for variable foo.
REPLACER takes an argument of the format variable.
Adapted from `org-roam-format-template'."
  (replace-regexp-in-string
   "\\${\\([^}]+\\)}"
   (lambda (md)
     (save-match-data
       (if-let ((text (funcall replacer (match-string 1 md))))
           text
         (signal 'citar-format-resolve md))))
   template
   ;; Need literal to make sure it works
   t t))

(defun citar--format-entry (entry width format-string)
  "Formats a BibTeX ENTRY for display in results list.
WIDTH is the width for the * field, and the display format is governed by
FORMAT-STRING."
  (citar--format
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
  (citar--format
   format-string
   (lambda (raw-field)
     (let ((field-names (split-string raw-field "[ ]+")))
       (citar-display-value field-names entry)))))

;;; At-point functions for Embark

;;;###autoload
(defun citar-key-finder ()
  "Return the citation key at point."
  (when-let (key (and (not (minibufferp))
                      (citar--major-mode-function 'key-at-point #'ignore)))
    (cons 'citar-key key)))

;;;###autoload
(defun citar-citation-finder ()
  "Return the keys of the citation at point."
  (when-let (citation (and (not (minibufferp))
                           (citar--major-mode-function 'citation-at-point #'ignore)))
    `(citar-citation ,(citar--stringify-keys (car citation)) . ,(cdr citation))))

(defun citar--stringify-keys (keys)
  "Return a list of KEYS as a crm-string for `embark'."
  (if (listp keys) (string-join keys " & ") keys))

(defun citar--reference-transformer (type target)
  "Look up key for a citar-reference TYPE and TARGET."
  (cons type (or (cadr (assoc target
                              (with-current-buffer (embark--target-buffer)
                                (citar--get-candidates)))))))

;;;###autoload
(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders 'citar-citation-finder)
  (add-to-list 'embark-transformer-alist
             '(citar-reference . citar--reference-transformer))
  (add-to-list 'embark-target-finders 'citar-key-finder))

(with-eval-after-load 'embark
  (set-keymap-parent citar-map embark-general-map)
  (add-to-list 'embark-keymap-alist '(citar-reference . citar-map))
  (add-to-list 'embark-keymap-alist '(citar-key . citar-citation-map))
  (add-to-list 'embark-keymap-alist '(citar-citation . citar-citation-map))
  (add-to-list (if (boundp 'embark-allow-edit-actions)
                   'embark-pre-action-hooks
                 'embark-target-injection-hooks)
               '(citar-insert-edit embark--ignore-target))
  (when (boundp 'embark-multitarget-actions)
    (dolist (command (list #'citar-insert-bibtex #'citar-insert-citation
                           #'citar-insert-reference #'citar-copy-reference
                           #'citar-insert-keys #'citar-run-default-action))
      (add-to-list 'embark-multitarget-actions command))))

;;; Commands

;;;###autoload
(defun citar-open (keys-entries)
  "Open related resources (links or files) for KEYS-ENTRIES."
  (interactive (list
                (list (citar-select-ref
                       :rebuild-cache current-prefix-arg))))
  (when (and citar-library-paths
             (stringp citar-library-paths))
    (message "Make sure 'citar-library-paths' is a list of paths"))
  (let* ((embark-default-action-overrides
          '((multi-category . citar-open-multi)
              (file . citar-file-open)
              (url . browse-url)))
         (key-entry-alist (citar--ensure-entries keys-entries))
         (files
          (citar-file--files-for-multiple-entries
           key-entry-alist
           (append citar-library-paths citar-notes-paths)
           ;; find files with any extension:
           nil))
         (links
          (seq-map
           (lambda (key-entry)
             (citar-get-link (cdr key-entry)))
           key-entry-alist))
         (resource-candidates (delete-dups (append files (remq nil links)))))
    (cond
     ((eq nil resource-candidates)
      (error "No associated resources"))
     ((unless citar-open-prompt
        (eq 1 (length resource-candidates)))
      (citar-open-multi (car resource-candidates)))
     (t (citar-open-multi
         (citar-select-resource files links))))))

(defun citar-open-multi (selection)
  "Act appropriately on SELECTION when type is 'multi-category'.
For use with 'embark-act-all'."
  (cond ((string-match "http" selection 0)
         (browse-url selection))
        ((member t (mapcar (lambda (x)
                             (file-in-directory-p selection x))
                           citar-notes-paths))
         (find-file selection))
        (t (citar-file-open selection))))

(defun citar--library-file-action (key-entry action)
  "Run ACTION on file associated with KEY-ENTRY."
  (let* ((fn (pcase action
               ('open 'citar-file-open)
               ('attach 'mml-attach-file)))
         (ke (citar--ensure-entries key-entry))
         (key (caar ke))
         (entry (cdar ke))
         (files
          (citar-file--files-for-entry
           key
           entry
           citar-library-paths
           citar-file-extensions))
         (file
          (pcase (length files)
            (1 (car files))
            ((guard (> 1))
             (citar-select-resource files)))))
    (if file
        (funcall fn file)
      (message "No associated file"))))

;;;###autoload
(defun citar-open-library-file (key-entry)
 "Open library file associated with the KEY-ENTRY.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref
                      :rebuild-cache current-prefix-arg)))
  (let ((embark-default-action-overrides '((file . citar-file-open))))
    (when (and citar-library-paths
               (stringp citar-library-paths))
      (error "Make sure 'citar-library-paths' is a list of paths"))
    (citar--library-file-action key-entry 'open)))

;;;###autoload
(defun citar-open-notes (key-entry)
  "Open notes associated with the KEY-ENTRY.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref
                      :rebuild-cache current-prefix-arg)))
  (let ((embark-default-action-overrides '((file . find-file))))
    (when (and (null citar-notes-paths)
               (equal citar-format-note-function
                      'citar-org-format-note-default))
      (error "You must set 'citar-notes-paths' to open notes with default notes function"))
    (funcall citar-open-note-function (car key-entry) (cdr key-entry))))

(defun citar--open-note (key entry)
  "Open a note file from KEY and ENTRY."
  (if-let* ((file (citar-file--get-note-filename key
                                                 citar-notes-paths
                                                 citar-file-note-extensions))
            (file-exists (file-exists-p file)))
      (find-file file)
    (funcall citar-format-note-function key entry file)))

;;;###autoload
(defun citar-open-entry (key-entry)
  "Open bibliographic entry associated with the KEY-ENTRY.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref
                      :rebuild-cache current-prefix-arg)))
  (when-let* ((key (car key-entry))
              (bibtex-files
               (seq-concatenate
                'list
                citar-bibliography
                (citar--local-files-to-cache))))
    (bibtex-search-entry key t nil t)))

;;;###autoload
(defun citar-insert-bibtex (keys-entries)
  "Insert bibliographic entry associated with the KEYS-ENTRIES.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (dolist (key (citar--extract-keys keys-entries))
    (citar--insert-bibtex key)))

(defun citar--insert-bibtex (key)
  "Insert the bibtex entry for KEY at point."
  (let* ((bibtex-files
          (seq-concatenate 'list citar-bibliography (citar--local-files-to-cache)))
         (entry
          (with-temp-buffer
            (bibtex-set-dialect)
            (dolist (bib-file bibtex-files)
              (insert-file-contents bib-file))
            (bibtex-search-entry key)
            (let ((beg (bibtex-beginning-of-entry))
                  (end (bibtex-end-of-entry)))
              (buffer-substring-no-properties beg end)))))
    (unless (equal entry "")
      (insert entry "\n\n"))))

;;;###autoload
(defun citar-export-local-bib-file ()
  "Create a new bibliography file from citations in current buffer.

The file is titled \"local-bib\", given the same extention as
the first entry in 'citar-bibliography', and created in the same
directory as current buffer."
  (interactive)
  (let* ((keys (citar--major-mode-function 'list-keys #'ignore))
        (ext (file-name-extension (car citar-bibliography)))
        (file (format "%slocal-bib.%s" (file-name-directory buffer-file-name) ext)))
    (with-temp-file file
      (dolist (key keys)
          (citar--insert-bibtex key)))))

;;;###autoload
(defun citar-open-link (key-entry)
  "Open URL or DOI link associated with the KEY-ENTRY in a browser.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref
                      :rebuild-cache current-prefix-arg)))
  (let ((link (citar-get-link (cdr key-entry))))
    (if link
        (browse-url link)
      (message "No link found for %s" (car key-entry)))))

;;;###autoload
(defun citar-insert-citation (keys-entries &optional arg)
  "Insert citation for the KEYS-ENTRIES.

Prefix ARG is passed to the mode-specific insertion function. It
should invert the default behaviour for that mode with respect to
citation styles. See specific functions for more detail."
  (interactive
   (if (member major-mode (mapcar
                           'caar
                           (butlast citar-major-mode-functions)))
       (list (citar-select-refs)  ; key-entries
	     current-prefix-arg) ; arg
     (error "Citation insertion is not supported for %s" major-mode)))
  (citar--major-mode-function
   'insert-citation
   #'ignore
   (citar--extract-keys keys-entries)
   arg))

(defun citar-insert-edit (&optional arg)
  "Edit the citation at point."
  (interactive "P")
  (citar--major-mode-function
   'insert-edit
   (lambda (&rest _)
     (message "Citation editing is not supported for %s" major-mode))
   arg))

;;;###autoload
(defun citar-insert-reference (keys-entries)
  "Insert formatted reference(s) associated with the KEYS-ENTRIES."
  (interactive (list (citar-select-refs)))
  (let ((key-entry-alist (citar--ensure-entries keys-entries)))
    (insert (funcall citar-format-reference-function key-entry-alist))))

;;;###autoload
(defun citar-copy-reference (keys-entries)
  "Copy formatted reference(s) associated with the KEYS-ENTRIES."
  (interactive (list (citar-select-refs)))
  (let* ((key-entry-alist (citar--ensure-entries keys-entries))
         (references (funcall citar-format-reference-function key-entry-alist)))
    (if (not (equal "" references))
        (progn
          (kill-new references)
          (message (format "Copied:\n%s" references)))
      (message "Key not found."))))

(defun citar-format-reference (key-entry-alist)
  "Return formatted reference(s) for the elements of KEY-ENTRY-ALIST."
  (let* ((template (citar-get-template 'preview))
         (references
          (with-temp-buffer
            (dolist (key-entry key-entry-alist)
              (when template
                (insert (citar--format-entry-no-widths (cdr key-entry) template))))
            (buffer-string))))
    references))

;;;###autoload
(defun citar-insert-keys (keys-entries)
  "Insert KEYS-ENTRIES citekeys.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (citar--major-mode-function
   'insert-keys
   #'citar--insert-keys-comma-separated
   (citar--extract-keys keys-entries)))

(defun citar--insert-keys-comma-separated (keys)
  "Insert comma separated KEYS."
  (insert (string-join keys ", ")))

;;;###autoload
(defun citar-attach-library-file (key-entry)
  "Attach library file associated with KEY-ENTRY to outgoing MIME message.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref
                      :rebuild-cache current-prefix-arg)))
  (let ((embark-default-action-overrides '((file . mml-attach-file))))
     (when (and citar-library-paths
                (stringp citar-library-paths))
       (error "Make sure 'citar-library-paths' is a list of paths"))
     (citar--library-file-action key-entry 'attach)))

;;;###autoload
(defun citar-run-default-action (keys-entries)
  "Run the default action `citar-default-action' on KEYS-ENTRIES."
  (funcall citar-default-action keys-entries))

;;;###autoload
(defun citar-dwim ()
  "Run the default action on citation keys found at point."
  (interactive)
  (if-let ((keys (or (car (citar--major-mode-function 'citation-at-point #'ignore))
                     (car (citar--major-mode-function 'key-at-point #'ignore)))))
      (citar-run-default-action (if (listp keys) keys (list keys)))
    (user-error "No citation keys found")))

(provide 'citar)
;;; citar.el ends here
