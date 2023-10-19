;;; citar.el --- Citation-related commands for org, latex, markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Bruce D'Arcus

;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: February 27, 2021
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 1.4.0
;; Homepage: https://github.com/emacs-citar/citar
;; Package-Requires: ((emacs "27.1") (parsebib "4.2") (org "9.5") (citeproc "0.9"))

;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;;  A completing-read front-end to browse, filter and act on BibTeX, BibLaTeX,
;;  and CSL JSON bibliographic data, including LaTeX, markdown, and org-cite
;;  citation editing support.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'seq)
(require 'map)
(require 'browse-url)
(require 'citar-cache)
(require 'citar-format)
(require 'citar-file)

;;; Declare variables and functions for byte compiler

(defvar embark-default-action-overrides)
(declare-function citar-org-format-note-default "citar-org")

;;; Variables

(defvar-local citar--entries nil
  "Override currently active citar entries.

When non-nil, should be a hash table mapping citation keys to
entries, as returned by `citar-get-entries'. Then all citar
functions will use that hash table as the source of bibliography
data instead of accessing the cache.

This variable should only be let-bound locally for the duration
of individual functions or operations. This is useful when using
multiple Citar functions in quick succession, to guarantee that
all potential cache accesses and updates are performed up-front.
In such cases, use a pattern like this:

  (let ((citar--entries (citar-get-entries)))
    ...)

Note that this variable is buffer-local, since Citar has a
different list of bibliographies (and hence entries) for each
buffer.")

;;;; Faces

(defgroup citar nil
  "Citations and bibliography management."
  :group 'editing)

(defface citar
  '((t :inherit font-lock-doc-face))
  "Default Face for `citar' candidates."
  :group 'citar)

(defface citar-highlight
  '((t))
  "Face used to highlight content in `citar' candidates."
  :group 'citar)

(defface citar-selection
  '((t :inherit highlight :slant italic))
  "Face used for the currently selected candidates."
  :group 'citar)

;;;; Bibliography, file, and note paths

(defcustom citar-bibliography nil
  "A list of bibliography files."
  :group 'citar
  :type '(repeat file))

(defcustom citar-library-paths nil
  "A list of files paths for related PDFs, etc."
  :group 'citar
  :type '(repeat directory))

(defcustom citar-library-file-extensions nil
  "List of file extensions to filter for related files.

These are the extensions the `citar-file-open-function'
will open, via `citar-file-open'.

When nil, the function will not filter the list of files."
  :group 'citar
  :type '(repeat string))

(defcustom citar-notes-paths nil
  "A list of file paths for bibliographic notes."
  :group 'citar
  :type '(repeat directory))

(defcustom citar-crossref-variable "crossref"
  "The bibliography field to look for cross-referenced entries.

When non-nil, find associated files and notes not only in the
original entry, but also in entries specified in the field named
by this variable."
  :group 'citar
  :type '(choice (const "crossref")
          (string :tag "Field name")
          (const :tag "Ignore cross-references" nil)))

(defcustom citar-additional-fields nil
  "A list of fields to add to parsed data.

By default, citar filters parsed data based on the fields
specified in `citar-templates', `citar-file-variable'
`citar-crossref-variable', and `citar-link-fields'. This
specifies additional fields to include."
  :group 'citar
  :type '(repeat string))

;;;; Displaying completions and formatting

(defcustom citar-templates
  '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
    (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
    (preview . "${author editor:%etal} (${year issued date}) ${title}, \
${journal journaltitle publisher container-title collection-title}.\n")
    (note . "Notes on ${author editor:%etal}, ${title}"))
  "Configures formatting for the bibliographic entry.

The main and suffix templates are for candidate display, and note
for the title field for new notes."
  :group 'citar
  :type  '(alist :key-type symbol
           :value-type string
           :options (main suffix preview note)))

(defcustom citar-ellipsis nil
  "Ellipsis string to mark ending of truncated display fields.

If t, use the value of `truncate-string-ellipsis'.  If nil, no
ellipsis will be used.  Otherwise, this should be a non-empty
string specifying the ellipsis."
  :group 'citar
  :type '(choice (const :tag "Use `truncate-string-ellipsis'" t)
          (const :tag "No ellipsis" nil)
          (const "…")
          (const "...")
          (string :tag "Ellipsis string")))

(defcustom citar-open-entry-function #'citar-open-entry-in-file
  "The function to run for `citar-open-entry'.

This function must accept one argument; the citekey."
  :group 'citar
  :type '(choice
          (function-item
           :tag "Open in BibTeX/BibLaTeX/CSL JSON file"
           citar-open-entry-in-file)
          (function-item
           :tag "Open in Zotero/Better BibTeX"
           citar-open-entry-in-zotero)
          (function :tag "Other")))

(defcustom citar-format-reference-function
  #'citar-format-reference
  "Function used to render formatted references.

This function is called by `citar-insert-reference' and
`citar-copy-reference'. The default value,
`citar-format-reference', formats references using the `preview'
template set in `citar-template'. To use `citeproc-el' to format
references according to CSL styles, set the value to
`citar-citeproc-format-reference'. Alternatively, set to a custom
function that takes a list of CITEKEYS and returns formatted
references as a string."
  :group 'citar
  :type '(choice (function-item :tag "Use 'citar-template'" citar-format-reference)
          (function-item :tag "Use 'citeproc-el'" citar-citeproc-format-reference)
          (function :tag "Other")))

(defcustom citar-display-transform-functions
  ;; TODO change this name, as it might be confusing?
  `((sn . (citar--shorten-names))
    (etal . (citar--shorten-names 3 "&")))
  "Configure transformation of field display values from raw values.

When the car symbol is associated with a field, run the cdr function
and optional arguments on the string value."
  :group 'citar
  :type '(alist :key-type symbol
          ;; REVIEW is this OK for now?
          :value-type list))

;; Indicator defstruct

;;;###autoload
(cl-defstruct
 (citar-indicator (:constructor citar-indicator-create)
                  (:copier nil))
 "A citar indicator specification."
 (tag
  nil
  :documentation
  "The string to include as hidden candidate text, and to then determine whether a
candidate predicate function will return non-nil.")
 (symbol
  nil
  :type string
  :documentation
  "The symbol string to use in the UI when predicate function returns non-nil.")
 (padding
  " "
  :type string
  :documentation
  "String to add to the right side of the indicator, for proper padding and such.")
 (emptysymbol
  ;; REVIEW we may not need this, so perhaps remove?
  " "
  :documentation
  "The symbol to use in the UI when predicate function returns nil. Can be useful
in some cases when using icons.")
 (function
  nil
  :type function
  :documentation
  "A predicate function that takes a single CITEKEY argument.")
 (compiledfunction
  nil
  :type compiled-function
  :documentation
  "A compiled version of `function' used during processing."))

;; Indicator specs

(defvar citar-indicator-files
   (citar-indicator-create
    :symbol "F"
    :function #'citar-has-files
    :tag "has:files"))

(defvar citar-indicator-links
   (citar-indicator-create
    :symbol "L"
    :function #'citar-has-links
    :tag "has:links"))

(defvar citar-indicator-notes
  (citar-indicator-create
   :symbol "N"
   :function #'citar-has-notes
   :tag "has:notes"))

(defvar citar-indicator-cited
  (citar-indicator-create
   :symbol "C"
   :function #'citar-is-cited
   :tag "is:cited"))

;; Indicator config

(defvar citar-indicators
  (list citar-indicator-links
        citar-indicator-files
        citar-indicator-notes
        citar-indicator-cited))

(defcustom citar-symbols
  `((file  .  ("F" . " "))
    (note .   ("N" . " "))
    (link .   ("L" . " ")))
  ;; DEPRECATED
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
  ;; DEPRECATED
  :group 'citar
  :type 'string)

(make-obsolete 'citar-symbols nil "1.4")
(make-obsolete 'citar-symbol-separator nil "1.4")

;;;; Citar actions and other miscellany

(defcustom citar-default-action #'citar-open
  "The default action for the `citar-at-point' command.
Should be a function that takes one argument, a list of
CITEKEYS."
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

(defcustom citar-open-prompt (list #'citar-open #'citar-attach-files #'citar-open-note)
  "Always prompt to open files, notes, or links.

If nil, when chosen keys have a single resource, it will be
selected without prompting. When t, `citar-open',
`citar-open-files', `citar-attach-files', `citar-open-links',
`citar-open-notes', and `citar-open-note' will always prompt to
select a resource.

Otherwise, the value should be a list of command names that will
always prompt to select."
  :group 'citar
  :type '(choice (const :tag "Always prompt" t)
          (const :tag "Prompt only for multiple resources" nil)
          (set :tag "Commands that prompt for multiple resources"
               (function-item citar-open)
               (function-item citar-open-files)
               (function-item citar-attach-files)
               (function-item citar-open-links)
               (function-item citar-open-notes)
               (function-item citar-open-note))))

;;;; File, note, and URL handling

(defcustom citar-open-resources '(:files :links :notes :create-notes)
  "Types of resources that `citar-open' offers to open."
  :group 'citar
  :type '(set (const :tag "Library files" :files)
          (const :tag "Links" :links)
          (const :tag "Notes" :notes)
          (const :tag "Create notes" :create-notes)))

(defcustom citar-open-always-create-notes nil
  "Offer to create notes even for keys that already have notes.

If nil, `citar-open' and `citar-open-notes' will only offer to
create new notes for keys that have no existing notes. When t,
offer to create new notes for all chosen keys.

Otherwise, the value should be a list of command names that will
offer to create new notes unconditionally."
  :group 'citar
  :type '(choice (const :tag "Always offer to create notes" t)
          (const :tag "Create notes only if none exist" nil)
          (set :tag "Create notes for commands"
               (function-item citar-open)
               (function-item citar-open-notes))))

(defcustom citar-file-sources
  (list (list :items #'citar-file--get-from-file-field
              :hasitems #'citar-file--has-file-field)
        (list :items #'citar-file--get-library-files
              :hasitems #'citar-file--has-library-files))
  "List of backends used to get library files for bibliography references.

Should be a list of plists, where each plist has the following properties:

  :items Function that takes a list of citation keys and returns
    a hash table mapping each of those keys to a list of files.

  :hasitems Function that takes a citation key and returns
    non-nil if it has associated files."
  :group 'citar
  :type '(repeat (plist :value-type function :options (:items :hasitems))))

(defcustom citar-add-file-sources
  '((?b "buffer" "Current buffer" citar--add-file-from-buffer)
    (?f "file" "Existing file" citar--add-file-from-file)
    (?u "url" "Download from URL" citar--add-file-from-url))
  "List of sources from which library files can be added.

The command `citar-add-file-to-library' prompts for the source of
the file to add, with the options being the elements of this
list. Each source must be a list containing:

  - The unique shortcut character the user types to select the
    source.

  - Short name for the source, displayed in the prompt.

  - Optional longer description, displayed if the user requests
    help.

  - Function that provides the file to add, described next.

The function is called with a bibliography key and should return
a plist containing the following keys:

  :extension The extension of the source file if known, or nil.

  :write-file A function taking arguments DESTFILE and
  OK-IF-ALREADY-EXISTS. When called, it should write the source
  file into DESTFILE. If DESTFILE already exists, it should act
  according to OK-IF-ALREADY-EXISTS, emulating `copy-file'.

The plist returned by this function is passed to
`citar-add-file-function', which see."
  :group 'citar
  :type '(repeat :tag "Sources for `citar-add-file-to-library'"
                 (group (character :tag "Shortcut")
                        (string :tag "Name")
                        (string :tag "Description")
                        (function :tag "Source function"))))

(defcustom citar-add-file-function #'citar-save-file-to-library
  "Function run by `citar-add-file-to-library' to add a file.

This function must accept two arguments: the CITEKEY of a
bibliography item; and the SOURCE-PLIST documented in
`citar-add-file-sources'."
  :group 'citar
  :type '(radio
          (function-item
           :doc "Save the file to `citar-library-paths'."
           citar-save-file-to-library)
          (function :tag "Other")))

(defcustom citar-notes-sources
  `((citar-file .
     ,(list :name "Notes"
            :category 'file
            :items #'citar-file--get-notes
            :hasitems #'citar-file--has-notes
            :open #'find-file
            :create #'citar-file--create-note
            :transform #'file-name-nondirectory)))
  "The alist of notes backends available for configuration.

The format of the cons should be (NAME . PLIST), where the
plist has the following properties:

  :name the group display name

  :category the completion category

  :hasitems a function that takes a CITEKEY and returns non-nil
  if it has associated notes

  :open function that, when given a note ID, opens the note

  :create function that, when given a note ID, creates new note if not present

  :items function to return completion candidate strings for list of CITEKEYS

  :annotate annotation function that returns a string for a note ID (optional)

  :transform transformation function (optional)"
  :group 'citar
  :type '(alist :key-type symbol :value-type plist))

(defcustom citar-notes-source 'citar-file
  "The notes backend."
  :group 'citar
  :type 'symbol)

;; TODO should this be a major mode function?
(defcustom citar-note-format-function #'citar-org-format-note-default
  "Function used by `citar-file' note source to format new notes."
  :group 'citar
  :type 'function)

(defcustom citar-link-fields '((doi . "https://doi.org/%s")
                               (pmid . "https://www.ncbi.nlm.nih.gov/pubmed/%s")
                               (pmcid . "https://www.ncbi.nlm.nih.gov/pmc/articles/%s")
                               (url . "%s"))
  "Bibliography fields to parse into links.

Association list whose keys are symbols naming bibliography
fields and values are URL strings. In each URL, \"%s\" is
replaced by the contents of the corresponding field."
  :group 'citar
  :type '(alist :key-type symbol :value-type string))


;;;; Major mode functions

;; TODO Move this to `citar-org', since it's only used there?
;; Otherwise it seems to overlap with `citar-default-action'
(defcustom citar-at-point-function #'citar-dwim
  "The function to run for `citar-at-point'."
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
      (list-keys . citar-latex-list-keys)))
    ((markdown-mode) .
     ((insert-keys . citar-markdown-insert-keys)
      (insert-citation . citar-markdown-insert-citation)
      (insert-edit . citar-markdown-insert-edit)
      (key-at-point . citar-markdown-key-at-point)
      (citation-at-point . citar-markdown-citation-at-point)
      (list-keys . citar-markdown-list-keys)))
    (t .
       ((insert-keys . citar--insert-keys-comma-space-separated))))
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

;;;; History, including future history list.

(defvar citar-history nil
  "Search history for `citar'.")

(defcustom citar-presets nil
  "List of predefined searches."
  :group 'citar
  :type '(repeat string))

(defcustom citar-select-multiple t
  "Use `completing-read-multiple' for selecting citation keys.
When nil, all citar commands will use `completing-read'."
  :type 'boolean
  :group 'citar)

;;;; Keymaps

(defvar citar-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'citar-insert-citation)
    (define-key map (kbd "k") #'citar-insert-keys)
    (define-key map (kbd "r") #'citar-copy-reference)
    (define-key map (kbd "R") #'citar-insert-reference)
    (define-key map (kbd "b") #'citar-insert-bibtex)
    (define-key map (kbd "o") #'citar-open)
    (define-key map (kbd "e") #'citar-open-entry)
    (define-key map (kbd "l") #'citar-open-links)
    (define-key map (kbd "n") #'citar-open-notes)
    (define-key map (kbd "f") #'citar-open-files)
    (define-key map (kbd "RET") #'citar-run-default-action)
    map)
  "Keymap for Embark minibuffer actions.")

(defvar citar-citation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'citar-insert-edit)
    (define-key map (kbd "o") #'citar-open)
    (define-key map (kbd "e") #'citar-open-entry)
    (define-key map (kbd "l") #'citar-open-links)
    (define-key map (kbd "n") #'citar-open-notes)
    (define-key map (kbd "f") #'citar-open-files)
    (define-key map (kbd "r") #'citar-copy-reference)
    (define-key map (kbd "RET") #'citar-run-default-action)
    map)
  "Keymap for Embark citation-key actions.")

;;; Bibliography cache

(defun citar--bibliography-files (&rest buffers)
  "Bibliography file names for BUFFERS.
The elements of BUFFERS are either buffers or the symbol global.
Returns the absolute file names of the bibliographies in all
these contexts.

When BUFFERS is nil, return local bibliographies for the current
buffer and global bibliographies."
  (citar-file--normalize-paths
   (mapcan (lambda (buffer)
             (if (eq buffer 'global)
                 (if (listp citar-bibliography) citar-bibliography
                   (list citar-bibliography))
               (with-current-buffer buffer
                 (citar--major-mode-function 'local-bib-files #'ignore))))
           (or buffers (list (current-buffer) 'global)))))

(defun citar--bibliographies (&rest buffers)
  "Return bibliographies for BUFFERS."
  (delete-dups
   (mapcan
    (lambda (buffer)
      (citar-cache--get-bibliographies (citar--bibliography-files buffer) buffer))
    (or buffers (list (current-buffer) 'global)))))

;;; Completion functions

(defun citar--completion-table (candidates &optional filter &rest metadata)
  "Return a completion table for CANDIDATES.

CANDIDATES is a hash with references CAND as key and CITEKEY as value,
  where CAND is a display string for the bibliography item.

FILTER, if non-nil, should be a predicate function taking
  argument CITEKEY. Only candidates for which this function
  returns non-nil will be offered for completion.

By default the metadata of the table contains the category and
affixation function. METADATA are extra entries for metadata of
the form (KEY . VAL).

The returned completion table can be used with `completing-read'
and other completion functions."
  (let ((metadata `(metadata . ((category . citar-candidate)
                                . ((affixation-function . ,#'citar--ref-affix)
                                   . ,metadata)))))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          metadata
        ;; REVIEW this now works, but probably needs refinement
        (let ((predicate
               (when (or filter predicate)
                 (lambda (cand key)
                   (and (or (null filter) (funcall filter key))
                        (or (null predicate) (funcall predicate cand)))))))
          (complete-with-action action candidates string predicate))))))

(cl-defun citar-select-refs (&key (multiple t) filter)
  "Select bibliographic references.

A wrapper around `completing-read' that returns a CITEKEY,

Takes the following optional keyword arguments:

MULTIPLE: if t, calls `completing-read-multiple' and returns a
list of CITEKEYS.

FILTER: if non-nil, should be a predicate function taking a
  CITEKEY. Only candidates for which this function returns
  non-nil will be offered for completion. For example:

  (citar-select-ref :filter (citar-has-note))

  (citar-select-ref :filter (citar-has-file))"
  (let* ((candidates (or (citar--format-candidates)
                         (user-error "No bibliography set")))
         (chosen (if (and multiple citar-select-multiple)
                     (citar--select-multiple "References: " candidates
                                             filter 'citar-history citar-presets)
                   (completing-read "Reference: " (citar--completion-table candidates filter)
                                    nil nil nil 'citar-history citar-presets nil))))
    ;; If CAND is not in CANDIDATES, treat it as a citekey (e.g. inserted into the minibuffer by `embark-act')
    (cl-flet ((candkey (cand) (or (gethash cand candidates) cand)))
      ;; Return a list of keys regardless of 1 or many
      (if (listp chosen)
          (mapcar #'candkey chosen)
        (list (candkey chosen))))))

(cl-defun citar-select-ref (&key filter)
  "Select bibliographic references.

Call `citar-select-ref' with optional FILTER; see its
documentation for the return value."
  (car (citar-select-refs :multiple nil :filter filter)))

(defun citar--multiple-completion-table (selected-hash candidates filter)
  "Return a completion table for multiple selection.
SELECTED-HASH is the hash-table containing selected candidates.
CANDIDATES is the list of completion candidates, FILTER is the function
to filter them."
  (citar--completion-table
   candidates filter
   `(group-function . (lambda (cand transform)
                        (pcase (list (not (not transform))
                                     (gethash (substring-no-properties cand) ,selected-hash))
                          ('(nil nil) (concat "Select Multiple ["
                                              (propertize (car citar--multiple-setup)
                                                          'font-lock-face 'help-key-binding
                                                          'face 'help-key-binding) "]"))
                          ('(nil t)   "Selected")
                          ('(t nil) cand)
                          ('(t t)
                           (add-face-text-property 0 (length cand) 'citar-selection nil (copy-sequence cand))
                           cand))))))

(defvar citar--multiple-setup '("TAB" . "RET")
  "Variable whose value should be a cons (SEL . EXIT)
SEL is the key which should be used for selection. EXIT is the key which
is used for exiting the minibuffer during completing read.")

(defun citar--multiple-exit ()
  "Exit with the currently selected candidates."
  (interactive)
  (setq unread-command-events (listify-key-sequence (kbd (car citar--multiple-setup)))))

(defun citar--setup-multiple-keymap ()
  "Make a keymap suitable for `citar--select-multiple'."
  (let ((keymap (make-composed-keymap nil (current-local-map)))
        (kbdselect (kbd (car citar--multiple-setup)))
        (kbdexit (kbd (cdr citar--multiple-setup))))
    (define-key keymap kbdselect (lookup-key keymap kbdexit))
    (define-key keymap kbdexit #'citar--multiple-exit)
    (use-local-map keymap)))

(defun citar--select-multiple (prompt candidates &optional filter history def)
  "Select multiple CANDIDATES with PROMPT.
HISTORY is the `completing-read' history argument."
  ;; Because completing-read-multiple just does not work for long candidate
  ;; strings, and IMO is a poor UI.
  (let* ((selected-hash (make-hash-table :test 'equal)))
    (while (let ((initial-history (symbol-value history))
                 (item (minibuffer-with-setup-hook #'citar--setup-multiple-keymap
                         (completing-read
                          (format "%s (%s/%s): " prompt
                                  (hash-table-count selected-hash)
                                  (hash-table-count candidates))
                          (citar--multiple-completion-table selected-hash candidates filter)
                          nil t nil history `("" . ,def)))))
             (unless (string-empty-p item)
               (if (not (gethash item selected-hash))
                   (puthash item t selected-hash)
                 (remhash item selected-hash)
                 (set history initial-history)))
             (not (or (eq last-command #'citar--multiple-exit)
                      (string-empty-p item)))))
    (hash-table-keys selected-hash)))

(cl-defun citar--get-resource-candidates (citekeys &key files links notes create-notes)
  "Return related resource candidates for CITEKEYS.

Return a list (CATEGORY . CANDIDATES), where CATEGORY is a
completion category and CANDIDATES is a list of resources
associated with CITEKEYS. Return nil if there are no associated
resources.

The resources include:
 * FILES: a list of files or t to use `citar-get-files'.
 * LINKS: a list of links or t to use `citar-get-links'.
 * NOTES: a list of notes or t to use `citar-get-notes'.
 * CREATE-NOTES: a list of cite keys for which to create notes,
     or t to use CITEKEYS. See `citar-open-always-create-notes'.

If any of FILES, LINKS, NOTES, or CREATE-NOTES is nil, that
resource type is omitted from CANDIDATES.

CATEGORY is one of:
 * `file' when returning only files
 * `url' when returning only links
 * the `:category' property of `citar-notes-source' if returning
   only notes
 * `citar-reference' when returning notes to create.
 * `multi-category' when CANDIDATES has resources of multiple
   types. The `multi-category' text property is applied to each
   element of CANDIDATES."
  (cl-flet ((getresources (table) (when table
                                    (delete-dups (apply #'append (hash-table-values table)))))
            (keycands (type citekeys)
              (let ((format (citar-format--parse (citar--get-template 'completion)))
                    (width (- (frame-width) 2)))
                (mapcar (lambda (key)
                          (let* ((entry (citar-get-entry key))
                                 (cand (citar-format--entry format entry width
                                                            :ellipsis citar-ellipsis))
                                 (keycand (citar--prepend-candidate-citekey key cand))
                                 (target (cons 'citar-reference
                                               (propertize key 'citar--resource type))))
                            (propertize keycand 'multi-category target)))
                        citekeys)))
            (withtype (type cat cands) (when cands
                                         (cons cat (mapcar (lambda (cand)
                                                             (propertize cand 'citar--resource type))
                                                           cands)))))
    (let* ((citar--entries (citar-get-entries))
           (files (if (listp files) files (getresources (citar-get-files citekeys))))
           (links (if (listp links) links (getresources (citar-get-links citekeys))))
           (keynotes (unless (and (listp notes) (listp create-notes)) (citar-get-notes citekeys)))
           (notes (if (listp notes) notes (getresources keynotes)))
           (create-notes
            (keycands 'create-note
                      (cond ((listp create-notes) create-notes)
                            ((or (eq t citar-open-always-create-notes)
                                 (memq this-command citar-open-always-create-notes)
                                 (not keynotes))
                             citekeys)
                            (t (seq-remove (lambda (citekey) (gethash citekey keynotes)) citekeys)))))
           (notecat (citar--get-notes-config :category))
           (sources (delq nil (list (withtype 'file 'file files)
                                    (withtype 'url 'url links)
                                    (withtype 'note notecat notes)
                                    (withtype 'create-note 'citar-candidate create-notes)))))
      (if (null (cdr sources))          ; if sources is nil or singleton list,
          (car sources)                 ; return either nil or the only source.
        (cons 'multi-category           ; otherwise, combine all sources
              (mapcan
               (pcase-lambda (`(,cat . ,cands))
                 (if (not cat)
                     cands
                   (mapcar (lambda (cand)
                             (if (get-text-property 0 'multi-category cand)
                                 cand
                               (propertize cand 'multi-category (cons cat cand))))
                           cands)))
               sources))))))

(defun citar--annotate-note (candidate)
  "Annotate note CANDIDATE."
  (when-let (((eq 'note (get-text-property 0 'citar--resource candidate)))
             (annotate (citar--get-notes-config :annotate)))
    (funcall annotate (substring-no-properties candidate))))

(cl-defun citar--select-resource (citekeys &key files links notes create-notes)
  "Select related FILES, NOTES, or LINKS resource for CITEKEYS.

Return (TYPE . RESOURCE), where TYPE is `file', `link', `note',
or `create-note' and RESOURCE is the selected resource string.
Return nil if there are no resources.

Use `completing-read' to prompt for a resource, unless there is
only one resource and `citar-open-prompt' is t or contains
`this-command'. Return nil if the user declined to choose."
  (when-let ((resources (citar--get-resource-candidates citekeys :files files :links links
                                                        :notes notes :create-notes create-notes)))
    (pcase-let ((`(,category . ,cands) resources))
      (when-let ((selected
                  (if (not (or (cdr cands) (eq t citar-open-prompt) (memq this-command citar-open-prompt)))
                      (car cands)
                    (let* ((metadata `(metadata
                                       (group-function . ,#'citar--select-group-related-resources)
                                       (annotation-function . ,#'citar--annotate-note)
                                       ,@(when category `((category . ,category)))))
                           (table (lambda (string predicate action)
                                    (if (eq action 'metadata)
                                        metadata
                                      (complete-with-action action cands string predicate))))
                           (selected (completing-read "Select resource: " table nil t)))
                      (car (member selected cands))))))
        (pcase (get-text-property 0 'citar--resource selected)
          ('create-note (cons 'create-note (citar--extract-candidate-citekey selected)))
          ;; Embark expects the plain string here.
          (type (cons type (substring-no-properties selected))))))))

(defun citar--select-group-related-resources (resource transform)
  "Group RESOURCE by type or TRANSFORM."
  (pcase (get-text-property 0 'citar--resource resource)
    ('file (if transform (file-name-nondirectory resource) "Library Files"))
    ('url (if transform resource "Links"))
    ('note
     (if transform
         (funcall (or (citar--get-notes-config :transform) #'identity) resource)
       (or (citar--get-notes-config :name) "Notes")))
    ('create-note
     (if transform
         resource
       (format "Create %s" (or (citar--get-notes-config :name) "Notes"))))
    (_ (if transform
           resource
         nil))))

;; Indicator functions

(defun citar--make-indicator-processors (ispecs)
  "Set the compiledfunction slots in ISPECS."
  (mapc
   (lambda (ispec)
     (let ((fnsym (citar-indicator-function ispec)))
       (setf (citar-indicator-compiledfunction ispec)
             (funcall fnsym))))
   ispecs))

(defun citar--make-indicator-tags (citekey iprocs)
  "Return indicator tags string for CITEKEY, using IPROCS.

This string is incorporated in the candidates as hidden text, so
it can be searched against, and to contruct the indicator symbols
visible in the completion UI."
  (mapconcat
   (lambda (iproc)
     (when-let* ((cfun (citar-indicator-compiledfunction iproc))
                 (tag (citar-indicator-tag iproc)))
       (when (funcall cfun citekey)
         (propertize (concat " " tag) 'invisible t))))
   iprocs ""))

(defun citar--make-indicator-symbols (candidate)
  "Return indicator string for CANDIDATE display."
  (seq-reduce
    (lambda (constructed ispec)
      ;; first arg is the accumulated string
      (let* ((matchtext (citar-indicator-tag ispec))
             (matchtagp (string-match-p matchtext candidate))
             (sym (citar-indicator-symbol ispec))
             (emptysym (citar-indicator-emptysymbol ispec))
             (padding (citar-indicator-padding ispec))
             (str (concat
                   constructed
                   (if matchtagp sym emptysym)
                   padding))
             (pos (length str)))
        ;; See https://github.com/emacs-citar/citar/issues/764 for explanation of what this code is doing.
        ;;
        ;; We say that the last character of the padding should be replaced by a space which stretches to the
        ;; position we want to.
        ;;
        ;; So for example if the icon has string width 1 but occupies 0.9 the space will stretch to occupy 1.1
        ;; and if the icon occupies 1.5 it will shrink to occupy 0.5.
        ;;
        ;; Emacs 29 has `vtable', which we might use here in the future as well.
        (put-text-property (- pos 1) pos 'display
                           (cons 'space
                                 (list :align-to (string-width str)))
                           str)
        str))
    citar-indicators ""))

(defun citar--format-candidates ()
  "Format completion candidates for bibliography entries.

Return a hash table with the keys being completion candidate
strings and values being citation keys.

Return nil if `citar-bibliographies' returns nil."
  ;; Populate bibliography cache.
  (when-let ((bibs (citar--bibliographies)))
    (let* ((citar--entries (citar-cache--entries bibs))
           (preformatted (citar-cache--preformatted bibs))
           (indicatorprocs (citar--make-indicator-processors citar-indicators))
           (symbolswidth (string-width
                          (citar--make-indicator-symbols "")))
           (width (- (frame-width) symbolswidth 2))
           (completions (make-hash-table :test 'equal :size (hash-table-count citar--entries))))
      (prog1 completions
        (maphash
         (lambda (citekey _entry)
           (let* ((preform (or (gethash citekey preformatted)
                               (error "No preformatted candidate string: %s" citekey)))
                  (display (citar-format--star-widths
                            (- width (car preform)) (cdr preform)
                            t citar-ellipsis))
                  (tagged (concat display
                                  (citar--make-indicator-tags citekey indicatorprocs))))
             (puthash tagged citekey completions)))
         citar--entries)))))

(defun citar--prepend-candidate-citekey (citekey candidate)
  "Prepend invisible CITEKEY to CANDIDATE string.
CITEKEY is quoted if necessary and can be extracted using
`citar--extract-candidate-citekey'."
  (let* ((keyquoted (if (or (string-empty-p citekey)          ; quote citekey if it's empty,
                            (= ?\" (aref citekey 0))          ; or starts with ",
                            (seq-contains-p citekey ?\s #'=)) ; or has a space.
                        (prin1-to-string citekey)
                      citekey))
         (prefix (propertize (concat keyquoted
                                     (when (and candidate (not (string-empty-p candidate))) " "))
                             'invisible t)))
    (concat prefix candidate)))

(defun citar--extract-candidate-citekey (candidate)
  "Extract the citation key from string CANDIDATE."
  (unless (string-empty-p candidate)
    (if (= ?\" (aref candidate 0))
        (read candidate)
      (substring-no-properties candidate 0 (seq-position candidate ?\s #'=)))))

(defun citar--key-at-point ()
  "Return bibliography key at point in current buffer, along with its bounds.
Return (KEY . BOUNDS), where KEY is a string and BOUNDS is either
nil or a (BEG . END) pair indicating the location of KEY in the
buffer. Return nil if there is no key at point or the current
major mode is not supported."
  (citar--major-mode-function 'key-at-point #'ignore))

(defun citar--citation-at-point ()
  "Return citation at point in current buffer, along with its bounds.
Return (KEYS . BOUNDS), where KEYS is a list of citation keys and
BOUNDS is either nil or a (BEG . END) pair indicating the
location of the citation in the buffer. Return nil if there is no
citation at point or the current major mode is not supported."
  (citar--major-mode-function 'citation-at-point #'ignore))

(defun citar-key-at-point ()
  "Return the citation key at point in the current buffer.
Return nil if there is no key at point or the major mode is not
supported."
  (car (citar--key-at-point)))

(defun citar-citation-at-point ()
  "Return a list of keys comprising the citation at point in the current buffer.
Return nil if there is no citation at point or the major mode is
not supported."
  (car (citar--citation-at-point)))

;;; Major-mode functions

(defun citar--get-major-mode-function (key &optional default)
  "Return function associated with KEY in `major-mode-functions'.
If no function is found matching KEY for the current major mode,
return DEFAULT."
  (alist-get
   key
   (cdr (seq-find
         (pcase-lambda (`(,modes . ,_functions))
           (or (eq t modes)
               (apply #'derived-mode-p (if (listp modes) modes (list modes)))))
         citar-major-mode-functions))
   default))

(defun citar--major-mode-function (key default &rest args)
  "Function for the major mode corresponding to KEY applied to ARGS.
If no function is found, the DEFAULT function is called."
  (apply (citar--get-major-mode-function key default) args))

;;; Data access functions

(defun citar-get-entry (citekey)
  "Return entry for reference CITEKEY, as an association list.
Note: this function accesses the bibliography cache and should
not be used for retreiving a large number of entries. Instead,
prefer `citar--get-entries'."
  (if citar--entries
      (gethash citekey citar--entries)
    (citar-cache--entry citekey (citar--bibliographies))))

(defun citar-get-entries ()
  "Return all entries for currently active bibliographies.
Return a hash table whose keys are citation keys and values are
the corresponding entries."
  (or citar--entries (citar-cache--entries (citar--bibliographies))))

(defun citar-get-value (field citekey-or-entry)
  "Return value of FIELD in reference CITEKEY-OR-ENTRY.
KEY-OR-ENTRY should be either a string key, or an entry alist as
returned by `citar-get-entry'. Return nil if the FIELD is not
present in CITEKEY-OR-ENTRY."
  (let ((entry (if (stringp citekey-or-entry)
                   (citar-get-entry citekey-or-entry)
                 citekey-or-entry)))
    (cdr (assoc-string field entry 'case-fold))))

(defun citar-get-field-with-value (fields citekey-or-entry)
  "Find the first field among FIELDS that has a value in CITEKEY-OR-ENTRY.
Return (FIELD . VALUE), where FIELD is the element of FIELDS that
was found to have a value, and VALUE is its value."
  (let ((entry (if (stringp citekey-or-entry)
                   (citar-get-entry citekey-or-entry)
                 citekey-or-entry)))
    (seq-some (lambda (field)
                (when-let ((value (citar-get-value field entry)))
                  (cons field value)))
              fields)))

(defun citar-get-display-value (fields citekey-or-entry &optional transform)
  "Return the first non nil value for CITEKEY-OR-ENTRY among FIELDS .

When TRANSFORM, use the list of function symbol and optional
arguments to transform the string."
  (let* ((fieldvalue (citar-get-field-with-value fields citekey-or-entry))
         (fnsymbol (car transform))
         (fnargs (cdr transform))
         (tvalue (if transform
                     (apply fnsymbol (cdr fieldvalue) fnargs)
                   (cdr fieldvalue))))
    ;; Make sure we always return a string, even if empty.
    (or tvalue "")))

;;;; File, notes, and links

(defun citar--get-notes-config (property)
  "Return PROPERTY value for configured notes backend."
  (plist-get
   (alist-get citar-notes-source citar-notes-sources) property))

(defun citar-register-notes-source (name config)
  "Register note backend.

NAME is a symbol, and CONFIG is a plist."
  (citar--check-notes-source name config)
  (setf (alist-get name citar-notes-sources) config))

(defun citar-remove-notes-source (name)
  "Remove note backend NAME."
  (cl-callf2 assq-delete-all name citar-notes-sources))

(cl-defun citar-get-notes (&optional (citekey-or-citekeys nil filter-p))
  "Return notes associated with CITEKEY-OR-CITEKEYS.

CITEKEY-OR-CITEKEYS should be either a list CITEKEYS or a single
CITEKEY. Return a hash table mapping elements of CITEKEYS to
lists of associated notes found using `citar-notes-source'.
Include notes associated with cross-referenced keys.

If CITEKEY-OR-CITEKEYS is omitted, return notes for all entries.
If it is nil, return nil."
  (when (or citekey-or-citekeys (not filter-p))
    (citar--get-resources citekey-or-citekeys
                          (citar--get-notes-config :items))))

(defun citar-create-note (key &optional entry)
  "Create a note for KEY and ENTRY.
If ENTRY is nil, use `citar-get-entry' with KEY."
  (interactive (list (citar-select-ref)))
  (funcall (citar--get-notes-config :create) key (or entry (citar-get-entry key))))

(cl-defun citar-get-files (&optional (citekey-or-citekeys nil filter-p))
  "Return files associated with CITEKEY-OR-CITEKEYS.

CITEKEY-OR-CITEKEYS should be either a list CITEKEYS or a single
KEY. Return a hash table mapping elements of CITEKEYS to lists of
associated files found using `citar-file-sources'. Include files
associated with cross-referenced citekeys.

If KEY-OR-KEYS is omitted, return files for all entries. If it is
nil, return nil."
  (when (or citekey-or-citekeys (not filter-p))
    (citar--get-resources citekey-or-citekeys
                          (mapcar (lambda (source)
                                    (plist-get source :items))
                                  citar-file-sources))))

(cl-defun citar-get-links (&optional (key-or-keys nil filter-p))
  "Return links associated with KEY-OR-KEYS.

KEY-OR-KEYS should be either a list KEYS or a single key. Return
a hash table mapping elements of KEYS to lists of associated
links found using `citar-link-fields'. Include links associated
with cross-referenced keys.

If KEY-OR-KEYS is omitted, return notes for all entries. If it is
nil, return nil."
  (when (or key-or-keys (not filter-p))
    (citar--get-resources key-or-keys
                          (apply-partially
                           #'citar--get-resources-using-function
                           (lambda (_citekey entry)
                             (let (keylinks)
                               (when entry
                                 (pcase-dolist (`(,fieldname . ,urlformat) citar-link-fields)
                                   (when-let ((fieldvalue (citar-get-value fieldname entry)))
                                     (push (format urlformat fieldvalue) keylinks))))
                               (nreverse keylinks)))))))

(defun citar-is-cited ()
  "Return function to check if reference is cited in buffer."
  (let ((iscited
         (citar--major-mode-function 'list-keys #'ignore)))
    (lambda (citekey)
      (member citekey iscited))))

(defun citar-has-files ()
  "Return predicate testing whether entry has associated files.

Return a function that takes CITEKEY and returns non-nil when the
corresponding bibliography entry has associated files. The
returned predicated may by nil if no entries have associated
files.

For example, to test whether CITEKEY has associated files:

  (when-let ((hasfilesp (citar-has-files)))
    (funcall hasfilesp CITEKEY))

When testing many citekeys, call this function once and use the
returned predicate repeatedly.

Files are detected using `citar-file-sources', which see. Also
check any bibliography entries that are cross-referenced from the
given CITEKEY; see `citar-crossref-variable'."
  (citar--has-resources
   (mapcar (lambda (source)
             (when-let ((hasitems (plist-get source :hasitems)))
               (funcall hasitems)))
           citar-file-sources)))


(defun citar-has-notes ()
  "Return predicate testing whether entry has associated notes.

Return a function that takes CITEKEY and returns non-nil when the
corresponding bibliography entry has associated notes. The
returned predicate may be nil if no entries have associated
notes.

For example, to test whether CITEKEY has associated notes:

  (let ((hasnotesp (citar-has-notes)))
    (funcall hasnotesp CITEKEY))

When testing many citekeys, call this function once and use the
returned predicate repeatedly.

Notes are detected using `citar-has-notes-functions', which see.
Also check any bibliography entries that are cross-referenced
from the given CITEKEY; see `citar-crossref-variable'."
  (citar--has-resources
   (funcall (citar--get-notes-config :hasitems))))


(defun citar-has-links ()
  "Return predicate testing whether entry has links.

Return a function that takes CITEKEY and returns non-nil when the
corresponding bibliography entry has associated links. See the
documentation of `citar-has-files' and `citar-has-notes', which
have similar usage."
  (let ((linkfields (mapcar (lambda (field) (symbol-name (car field))) citar-link-fields)))
    (citar--has-resources
     (apply-partially #'citar-get-field-with-value linkfields))))


(defun citar--has-resources (predicates)
  "Combine PREDICATES into a single resource predicate.

PREDICATES should be a list of functions that take a bibliography
KEY and return non-nil if the item has a resource. It may also be
a single such function.

Return a predicate that returns non-nil for a given CITEKEY when
any of the elements of PREDICATES return non-nil for that
CITEKEY. If PREDICATES is empty or all its elements are nil, then
the returned predicate is nil.

When `citar-crossref-variable' is the name of a crossref field,
the returned predicate also tests if an entry cross-references
another entry in ENTRIES that has associated resources."
  (when-let ((hasresourcep (if (functionp predicates)
                               predicates
                             (let ((predicates (remq nil predicates)))
                               (if (null (cdr predicates))
                                   ;; optimization for single predicate; just use it directly
                                   (car predicates)
                                 ;; otherwise, call all predicates until one returns non-nil
                                 (lambda (citekey)
                                   (seq-some (lambda (predicate)
                                               (funcall predicate citekey))
                                             predicates)))))))
    (if-let ((xref citar-crossref-variable))
        (lambda (citekey)
          (or (funcall hasresourcep citekey)
              (when-let ((xkey (citar-get-value xref citekey)))
                (funcall hasresourcep xkey))))
      hasresourcep)))

(defun citar--get-resources (citekey-or-citekeys functions)
  "Return hash table mapping elements of CITEKEY-OR-CITEKEYS to resource lists.

CITEKEY-OR-CITEKEYS should be either a list CITEKEYS or a single
citekey. FUNCTIONS should be a list of functions, each of which
takes a list of citekeys and returns a hash table mapping each of
those citekeys to a list of resources. FUNCTIONS may also be a single
such function.

Return a hash table mapping each element of CITEKEYS to the
concatenated list of resources returned by all the FUNCTIONS.
Also include resources associated with cross-references from
CITEKEYS."
  (let* ((citar--entries (citar-get-entries))
         (citekeys (if (listp citekey-or-citekeys) (delete-dups citekey-or-citekeys)
                     (list citekey-or-citekeys)))
         (functions (if (functionp functions) (list functions) (remq nil functions)))
         (xref citar-crossref-variable)
         (getxref (apply-partially #'citar-get-value xref))
         (xkeys (if (not xref)
                    citekeys
                  (delete-dups (append citekeys (delq nil (mapcar getxref citekeys))))))
         (resources (delq nil (mapcar (lambda (func) (funcall func xkeys)) functions))))
    (cl-flet* ((getreslists (citekey) (delq nil (mapcar (apply-partially #'gethash citekey) resources)))
               (xresources (citekey entry) (apply #'append
                                                  (nconc (getreslists citekey)
                                                         (when-let ((xkey (and xref
                                                                               (citar-get-value xref entry))))
                                                           (getreslists xkey))))))
      (citar--get-resources-using-function #'xresources citekeys))))

(defun citar--get-resources-using-function (func &optional citekeys)
  "Collect resources for CITEKEYS returned by FUNC.

Return a hash table mapping each element of CITEKEYS to the
result of calling FUNC on that key and corresponding bibliography
entry. If CITEKEYS is nil, call FUNC on every key and entry
returned by `citar-get-entries'.

Note: This is a helper function to make it easier to write
getters for file, note, and link resources."
  (let ((resources (make-hash-table :test 'equal)))
    (prog1 resources
      (cl-flet ((putresult (citekey entry) (when-let ((result (funcall func citekey entry)))
                                             (puthash citekey result resources))))
        (if (null citekeys)
            (maphash #'putresult (citar-get-entries))
          (dolist (citekey citekeys)
            (putresult citekey (citar-get-entry citekey))))))))

(defun citar-save-file-to-library (citekey source-plist)
  "Use as `citar-add-file-function' to save files to `citar-library-paths'.

Save the file with the filename CITEKEY.EXTENSION to a directory
from `citar-library-paths'. If there are multiple such
directories, prompt the user to choose one. If SOURCE-PLIST has
an :extension key, use its value as EXTENSION; otherwise prompt
the user. If the file already exists, confirm before overwriting
it.

SOURCE-PLIST must be as specified in the documentation of
`citar-add-file-sources'."
  (let* ((directory (if (cdr citar-library-paths)
                        (completing-read "Directory: " citar-library-paths)
                      (car citar-library-paths)))
         (extension (or (plist-get source-plist :extension)
                        (read-string "File extension: ")))
         (destfile (expand-file-name citekey directory))
         (destfile (if (string-empty-p extension)
                       destfile
                     (concat destfile "." extension)))
         ;; an integer means to confirm before overwriting
         (ok-if-already-exists 1))
    (funcall (plist-get source-plist :write-file)
             destfile ok-if-already-exists)))

(defun citar--add-file-from-buffer (_citekey)
  "Include in `citar-add-file-sources' to add contents of current buffer.

See the documentation for `citar-add-file-sources' for more details."
  (let ((buf (get-buffer (read-buffer "Add file buffer: " (current-buffer)))))
    (list :write-file
          (lambda (destfile ok-if-already-exists)
            (with-current-buffer buf
              (if (and buffer-file-name
                       (file-equal-p destfile buffer-file-name))
                  (if (not (buffer-modified-p))
                      (message "%s exists and the current buffer is visiting it."
                               (file-name-nondirectory buffer-file-name))
                    (unless ok-if-already-exists
                      (signal 'file-already-exists
                              (list "File already exists" destfile)))
                    (when (or (not (integerp ok-if-already-exists))
                              (yes-or-no-p
                               (format
                                "%s exists and the current buffer is visiting it.  Save anyway? "
                                (file-name-nondirectory buffer-file-name))))
                      (save-buffer)))
                (write-region nil nil destfile
                              (if ok-if-already-exists
                                  ;; Confirm if integer, otherwise overwrite silently:
                                  (integerp ok-if-already-exists)
                                ;; Otherwise signal 'file-already-exists error
                                'excl)))))
          :extension
          (when (buffer-file-name buf)
            (file-name-extension (buffer-file-name buf))))))

(defun citar--add-file-from-file (_citekey)
  "Include in `citar-add-file-sources' to add copy of existing file.

See the documentation for `citar-add-file-sources' for more details."
  (let ((file (read-file-name "Add file: " nil nil t)))
    (list :write-file
          (lambda (destfile ok-if-already-exists)
            (copy-file file destfile ok-if-already-exists))
          :extension (file-name-extension file))))

(defun citar--add-file-from-url (_citekey)
  "Include in `citar-add-file-sources' to add from URL.

See the documentation for `citar-add-file-sources' for more details."
  (let* ((url (read-string "Add file URL: "))
         ;; TODO: Use Content-Type HTTP response header to guess file extension
         (dot-ext (url-file-extension url)))
    (list :write-file
          (lambda (destfile ok-if-already-exists)
            (url-copy-file url destfile ok-if-already-exists))
          :extension (when (> (length dot-ext) 1) (substring dot-ext 1)))))

;;; Format and display field values

(defun citar--shorten-name-position (namelist name)
  "Return NAME position in a NAMELIST."
  (+ (seq-position namelist name) 1))

(defun citar--shorten-name (name)
  "Return family NAME in `family, given' string.

Otherwise, return as is."
  (car (split-string name ", ")))

(defun citar--shorten-names (namestr &optional truncate andstr)
  "Return a list of family names from a list of full NAMESTR.
To better accommodate corporate names, this will only shorten
personal names of the form \"family, given\".

With an integer TRUNCATE, will shorten the list, and ANDSTR will
replace last comma."
  (let* ((namelist (split-string (or namestr "") " and "))
         (namelength (length namelist))
         (tnamelist (seq-take namelist (or truncate namelength)))
         (tnamelength (length tnamelist)))
    (mapconcat
     (lambda (n)
       (let* ((shortname (citar--shorten-name n))
              (pos (citar--shorten-name-position tnamelist n))
              (suffix
               (cond
                ;; if last name in the list and we're truncating add et al.; otherwise, no suffix
                ((equal pos tnamelength)
                 (if (< tnamelength namelength) " et al." ""))
                ;; if second to last in the list, and ANDSTR, use that
                ((and andstr (equal pos (- tnamelength 1)))
                 (concat " " andstr " "))
                ;; otherwise, use a comma
                (t ", "))))
         (concat shortname suffix)))
     tnamelist "")))

(defun citar--fields-for-format (template)
  "Return list of fields for TEMPLATE."
  (mapcan (lambda (fieldspec) (when (consp fieldspec) (cdr fieldspec)))
          (citar-format--parse template)))

(defun citar--fields-in-formats ()
  "Find the fields to mentioned in the templates."
  (seq-mapcat #'citar--fields-for-format
              (list (citar--get-template 'main)
                    (citar--get-template 'suffix)
                    (citar--get-template 'preview)
                    (citar--get-template 'note))))

(defun citar--fields-to-parse ()
  "Determine the fields to parse from the template."
  (delete-dups `(,@(citar--fields-in-formats)
                 ,@(when citar-file-variable
                     (list citar-file-variable))
                 ,@(when citar-crossref-variable
                     (list citar-crossref-variable))
                 ,@(mapcar (lambda (field) (symbol-name (car field))) citar-link-fields)
                 . ,citar-additional-fields)))

;;; Affixations and annotations

(defun citar--ref-affix (candidates)
  "Add affixation prefix to CANDIDATES."
  (seq-map
   (lambda (candidate)
     (let ((symbols
            (citar--make-indicator-symbols candidate)))
       (list candidate symbols "")))
   candidates))

(defun citar--ref-annotate (candidate)
  "Add annotation to CANDIDATE."
  ;; REVIEW/TODO we don't currently use this, but could, for Emacs 27.
  (citar--make-indicator-symbols candidate))

(defun citar--get-template (template-name)
  "Return template string for TEMPLATE-NAME."
  (or
   (cdr (assq template-name citar-templates))
   (when (eq template-name 'completion)
     (concat (propertize (citar--get-template 'main) 'face 'citar-highlight)
             (propertize (citar--get-template 'suffix) 'face 'citar)))
   (error "No template for \"%s\" - check variable 'citar-templates'" template-name)))

;;;###autoload
(defun citar-insert-preset ()
  "Prompt for and insert a predefined search."
  (interactive)
  (unless (minibufferp)
    (user-error "Command can only be used in minibuffer"))
  (when-let ((enable-recursive-minibuffers t)
             (search (completing-read "Preset: " citar-presets)))
    (insert search)))

(defun citar--stringify-keys (citekeys)
  "Encode a list of CITEKEYS as a single string."
  (combine-and-quote-strings (if (listp citekeys) citekeys (list citekeys)) " & "))

(defun citar--unstringify-keys (citekeystring)
  "Split CITEKEYSTRING into a list of keys."
  (split-string-and-unquote citekeystring " & "))

;;; Commands

;;;###autoload
(defun citar-open (citekeys)
  "Open related resources (links, files, or notes) for CITEKEYS."
  (interactive (list (citar-select-refs)))
  (pcase (let ((embark-default-action-overrides
                (cons (cons t #'citar--open-resource)
                      (bound-and-true-p embark-default-action-overrides))))
           (apply #'citar--select-resource citekeys
                  (mapcan (lambda (type) (list type t)) citar-open-resources)))
    (`(,type . ,resource) (citar--open-resource resource type))
    (_ (error "No associated resources: %s" citekeys))))

(defun citar--open-resource (resource &optional type)
  "Open RESOURCE of TYPE.
TYPE should be `file', `url', `note', or `create-note'. If TYPE
is nil, then RESOURCE must have a `citar--resource' text property
specifying TYPE."
  ;; IMPORTANT: This function must not have an `interactive' specification. When called as an Embark action,
  ;; it relies on RESOURCE having the `citar--resource' text property to decide which action to take. However,
  ;; `embark-act' strips text properties for interactive commands, for which it injects the target text as
  ;; minibuffer input.
  (if-let ((opener (pcase (or type (get-text-property 0 'citar--resource resource))
                     ('file #'citar-file-open)
                     ('url #'browse-url)
                     ('note #'citar-open-note)
                     ('create-note #'citar-create-note))))
      (funcall opener (substring-no-properties resource))
    (error "Could not open resource of type `%s': %S" type resource)))

;;;###autoload
(defun citar-open-files (citekey-or-citekeys)
  "Open library file associated with CITEKEY-OR-CITEKEYS."
  (interactive (list (citar-select-refs)))
  ;; TODO filter to refs have files?
  (citar--library-file-action citekey-or-citekeys #'citar-file-open))

;;;###autoload
(defun citar-attach-files (citekey-or-citekeys)
  "Attach library file associated with CITEKEY-OR-CITEKEYS to outgoing MIME message."
  (interactive (list (citar-select-ref)))
  (citar--library-file-action citekey-or-citekeys #'mml-attach-file))

(defun citar--library-file-action (citekey-or-citekeys action)
  "Run ACTION on file associated with CITEKEY-OR-CITEKEYS.
Use `citar--select-resource' to choose a file."
  (let ((citar--entries (citar-get-entries)))
    (pcase (let ((embark-default-action-overrides
                  (cons (cons `(file . ,this-command) action)
                        (bound-and-true-p embark-default-action-overrides))))
             (citar--select-resource citekey-or-citekeys :files t))
      (`(file . ,file) (funcall action file))
      (`(,type . ,resource) (error "Expected resource of type `file', got `%s': %S" type resource))
      ('nil
       (ignore
        ;; If some key had files according to the `:hasitems' function, but `:items' returned nothing, then
        ;; don't print the following message. The `:items' function is responsible for telling the user why it
        ;; failed, and we want that explanation to appear in the echo area.
        (let ((keys (if (listp citekey-or-citekeys) citekey-or-citekeys (list citekey-or-citekeys)))
              (hasfilep (citar-has-files)))
          (unless (and hasfilep (seq-some hasfilep keys))
            (message "No associated files for %s" citekey-or-citekeys))))))))

;;;###autoload
(defun citar-open-note (note)
  "Open a single NOTE directly.
The note should be represented as a string returned by
`citar-get-notes'. When called interactively, prompt for a note
to open from a list of all notes."
  (interactive (list (when-let* ((notes (citar-get-notes))
                                 (allnotes (delete-dups (apply #'append (hash-table-values notes)))))
                       (cdr (citar--select-resource nil :notes allnotes)))))
  (when note
    (funcall (citar--get-notes-config :open) note)))

;;;###autoload
(defun citar-open-notes (citekeys)
  "Open notes associated with the CITEKEYS."
  (interactive (list (citar-select-refs)))
  (pcase (let ((embark-default-action-overrides
                (cons (cons t #'citar--open-resource)
                      (bound-and-true-p embark-default-action-overrides))))
           (citar--select-resource citekeys :notes t :create-notes t))
    (`(note . ,note) (citar-open-note note))
    (`(create-note . ,citekey) (citar-create-note citekey))))

;;;###autoload
(defun citar-open-links (citekey-or-citekeys)
  "Open URL or DOI link associated with CITEKEY-OR-CITEKEYS in a browser."
  (interactive (list (citar-select-refs)))
  (pcase (let ((embark-default-action-overrides
                (cons (cons `(url . ,this-command) #'browse-url)
                      (bound-and-true-p embark-default-action-overrides))))
           (citar--select-resource citekey-or-citekeys :links t))
    (`(url . ,url) (browse-url url))
    (`(,type . ,resource) (error "Expected resource of type `url', got `%s': %S" type resource))
    ('nil (message "No link found for %s" citekey-or-citekeys))))

;;;###autoload
(defun citar-open-entry (citekey)
  "Open bibliographic entry associated with the CITEKEY."
  (interactive (list (citar-select-ref)))
  (funcall citar-open-entry-function citekey))

(defun citar-open-entry-in-file (citekey)
  "Open entry for CITEKEY."
  ;; Adapted from 'bibtex-completion-show-entry'.
  (when-let ((bib-files (citar--bibliography-files)))
    (catch 'break
      (dolist (bib-file bib-files)
        (let ((buf (or (get-file-buffer bib-file)
                       (find-buffer-visiting bib-file))))
          (find-file bib-file)
          (widen)
          (goto-char (point-min))
          (when (re-search-forward
                 (concat "^@\\(" parsebib--bibtex-identifier
                         "\\)[[:space:]]*[\\(\\{][[:space:]]*"
                         (regexp-quote citekey) "[[:space:]]*,") nil t)
            (throw 'break t))
          (unless buf
            (kill-buffer)))))))

(defun citar-open-entry-in-zotero (citekey)
  "Open a reference item for CITEKEY in Zotero.

This function assumes a setup where the bibliographic data,
including the citekeys, is maintained in Zotero with Better BibTeX."
  (citar-file-open-external
   (concat "zotero://select/items/@" citekey)))

;;;###autoload
(defun citar-insert-bibtex (citekeys)
  "Insert bibliographic entry associated with the CITEKEYS."
  (interactive (list (citar-select-refs)))
  (dolist (citekey citekeys)
    (citar--insert-bibtex citekey)))

(defun citar--insert-bibtex (citekey)
  "Insert the bibtex entry for CITEKEY at point."
  (let* ((bibtex-files
          (citar--bibliography-files))
         (entry
          (with-temp-buffer
            (bibtex-set-dialect)
            (dolist (bib-file bibtex-files)
              (insert-file-contents bib-file))
            (bibtex-search-entry citekey)
            (let ((beg (bibtex-beginning-of-entry))
                  (end (bibtex-end-of-entry)))
              (buffer-substring-no-properties beg end)))))
    (unless (equal entry "")
      (insert entry "\n\n"))))

;;;###autoload
(defun citar-export-local-bib-file ()
  "Create a new bibliography file from citations in current buffer.

The file is titled \"local-bib\", given the same extension as
the first entry in `citar-bibliography', and created in the same
directory as current buffer."
  (interactive)
  (let* ((citekeys (citar--major-mode-function 'list-keys #'ignore))
         (ext (file-name-extension (car citar-bibliography)))
         (file (format "%slocal-bib.%s" (file-name-directory buffer-file-name) ext)))
    (with-temp-file file
      (dolist (citekey citekeys)
        (citar--insert-bibtex citekey)))))

;;;###autoload
(defun citar-insert-citation (citekeys &optional arg)
  "Insert citation for the CITEKEYS.

Prefix ARG is passed to the mode-specific insertion function. It
should invert the default behaviour for that mode with respect to
citation styles. See specific functions for more detail."
  (interactive
   (if (citar--get-major-mode-function 'insert-citation)
       (list (citar-select-refs) current-prefix-arg)
     (error "Citation insertion is not supported for %s" major-mode)))
  (citar--major-mode-function
   'insert-citation
   #'ignore
   citekeys
   arg))

(defun citar-insert-edit (&optional arg)
  "Edit the citation at point.
ARG is forwarded to the mode-specific insertion function given in
`citar-major-mode-functions'."
  (interactive "P")
  (citar--major-mode-function
   'insert-edit
   (lambda (&rest _)
     (message "Citation editing is not supported for %s" major-mode))
   arg))

;;;###autoload
(defun citar-insert-reference (citekeys)
  "Insert formatted reference(s) associated with the CITEKEYS."
  (interactive (list (citar-select-refs)))
  (insert (funcall citar-format-reference-function citekeys)))

;;;###autoload
(defun citar-copy-reference (citekeys)
  "Copy formatted reference(s) associated with the CITEKEYS."
  (interactive (list (citar-select-refs)))
  (let ((references (funcall citar-format-reference-function citekeys)))
    (if (not (equal "" references))
        (progn
          (kill-new references)
          (message (format "Copied:\n%s" references)))
      (message "Key not found."))))

(defun citar-format-reference (citekeys)
  "Return formatted reference(s) for the elements of CITEKEYS."
  (let* ((entries (mapcar #'citar-get-entry citekeys))
         (template (citar--get-template 'preview)))
    (with-temp-buffer
      (dolist (entry entries)
        (insert (citar-format--entry template entry)))
      (buffer-string))))

;;;###autoload
(defun citar-insert-keys (citekeys)
  "Insert CITEKEYS."
  (interactive (list (citar-select-refs)))
  (citar--major-mode-function
   'insert-keys
   #'citar--insert-keys-comma-space-separated
   citekeys))

(defun citar--insert-keys-comma-separated (citekeys)
  "Insert comma separated CITEKEYS."
  (insert (string-join citekeys ",")))

(defun citar--insert-keys-comma-space-separated (citekeys)
  "Insert comma, space separated CITEKEYS."
  (insert (string-join citekeys ", ")))

;;;###autoload
(defun citar-add-file-to-library (citekey)
  "Add a file to the library for CITEKEY.
The FILE can be added from an open buffer, a file path, or a
URL."
  (interactive (list (citar-select-ref)))
  (citar--check-configuration 'citar-library-paths 'citar-add-file-sources
                              'citar-add-file-function)
  (unless citar-library-paths
    (user-error "Make sure `citar-library-paths' is non-nil"))
  (unless citar-add-file-sources
    (user-error "Make sure `citar-add-file-sources' is non-nil"))
  (let ((source (read-multiple-choice "Add file from" citar-add-file-sources)))
    (funcall citar-add-file-function
             citekey (funcall (nth 3 source) citekey))))

;;;###autoload
(defun citar-run-default-action (citekeys)
  "Run the default action `citar-default-action' on CITEKEYS."
  (funcall citar-default-action citekeys))

;;;###autoload
(defun citar-dwim ()
  "Run the default action on citation keys found at point."
  (interactive)
  (if-let ((citekeys (or (citar-key-at-point) (citar-citation-at-point))))
      (citar-run-default-action (if (listp citekeys) citekeys (list citekeys)))
    (user-error "No citation keys found")))

(defun citar--check-configuration (&rest variables)
  "Signal error if any VARIABLES have values of the wrong type.
VARIABLES should be the names of Citar customization variables."
  (dolist (variable variables)
    (unless (boundp variable)
      (error "Unbound variable in citar--check-configuration: %s" variable))
    (let ((value (symbol-value variable)))
      (pcase variable
        ((or 'citar-library-paths 'citar-notes-paths)
         (unless (and (listp value)
                      (seq-every-p #'stringp value))
           (error "`%s' should be a list of directories: %S" variable `',value)))
        ((or 'citar-library-file-extensions 'citar-file-note-extensions)
         (unless (and (listp value)
                      (seq-every-p #'stringp value))
           (error "`%s' should be a list of strings: %S" variable `',value)))
        ((or 'citar-has-files-functions 'citar-get-files-functions 'citar-file-parser-functions)
         (unless (and (listp value) (seq-every-p #'functionp value))
           (error "`%s' should be a list of functions: %S" variable `',value)))
        ((or 'citar-note-format-function 'citar-add-file-function)
         (unless (functionp value)
           (error "`%s' should be a function: %S" variable `',value)))
        ('citar-add-file-sources
         (unless (listp value)
           (error "`%s' should be a list: %S" variable `',value))
         (unless (seq-every-p (lambda (x)
                                (and (= 4 (length x))
                                     (characterp (nth 0 x))
                                     (stringp (nth 1 x))
                                     (stringp (nth 2 x))
                                     (functionp (nth 3 x))))
                              value)
           (error "Every element of `%s' should be a list of the type (character string string function): %S"
                  variable `',value)))
        (_
         (error "Unknown variable in citar--check-configuration: %s" variable))))))

(defun citar--check-notes-source (name config)
  "Signal error if notes source plist CONFIG has incorrect keys or values.
SOURCE must be a plist representing a notes source with NAME. See
`citar-notes-sources' for the list of valid keys and types."

  (let ((required '(:items :hasitems :open))
        (optional '(:name :category :create :transform :annotate))
        (keys (map-keys config)))
    (when-let ((missing (cl-set-difference required keys)))
      (error "Note source `%s' missing required keys: %s" name missing))
    (when-let ((extra (cl-set-difference keys (append required optional))))
      (warn "Note source `%s' has unknown keys: %s" name extra)))

  (pcase-dolist (`(,type . ,props)
                 '((functionp :items :hasitems :open :create :transform :annotate)
                   (stringp :name)
                   (symbolp :category)))
    (when-let ((wrongtype (seq-filter (lambda (prop)
                                        (when-let ((value (plist-get config prop)))
                                          (not (funcall type value)))) props)))
      (error "Note source `%s' keys must be of type %s: %s" name type wrongtype))))

(provide 'citar)
;;; citar.el ends here
