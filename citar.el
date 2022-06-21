;;; citar.el --- Citation-related commands for org, latex, markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Bruce D'Arcus

;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: February 27, 2021
;; License: GPL-3.0-or-later
;; Version: 0.9.5
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
(require 'map)
(require 'browse-url)
(require 'citar-cache)
(require 'citar-format)
(require 'citar-file)
(require 'crm)

;;; pre-1.0 API cleanup

;; make public
;; (make-obsolete 'citar--get-candidates 'citar-get-candidates "1.0")

;; make all these private
(make-obsolete 'citar-get-template 'citar--get-template "1.0")
(make-obsolete 'citar-get-link 'citar--get-link "1.0")
(make-obsolete 'citar-get-value 'citar--get-value "1.0")
(make-obsolete 'citar-display-value 'citar--display-value "1.0")
(make-obsolete 'citar-open-multi 'citar--open-multi "1.0")
(make-obsolete 'citar-select-group-related-resources
               'citar--select-group-related-resources "1.0")
(make-obsolete 'citar-select-resource 'citar--select-resource "1.0")

;; also rename
(make-obsolete 'citar-has-a-value 'citar--field-with-value "1.0")
(make-obsolete 'citar--open-note 'citar-file--open-note "1.0")

(make-obsolete-variable
 'citar-format-note-function 'citar-create-note-function "1.0")

;;; Declare variables and functions for byte compiler

(defvar embark-default-action-overrides)

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

(defface citar-selection
  '((t :inherit highlight :slant italic))
  "Face used for the currently selected candidates."
  :group 'citar)

(defcustom citar-bibliography nil
  "A list of bibliography files."
  :group 'citar
  :type '(repeat file))

(defcustom citar-has-file-functions '(citar-has-file-field
                                      citar-file-has-library-files)
  "List of functions to test if an entry has associated files."
  :group 'citar
  :type '(repeat function))

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

(defcustom citar-additional-fields '("doi" "url")
  "A list of fields to add to parsed data.

By default, citar filters parsed data based on the fields
specified in `citar-templates'. This specifies additional fields
to include."
  :group 'citar
  :type '(repeat string))

(defcustom citar-templates
  '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
    (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
    (preview . "${author editor} (${year issued date}) ${title}, \
${journal journaltitle publisher container-title collection-title}.\n")
    (note . "Notes on ${author editor}, ${title}"))
  "Configures formatting for the bibliographic entry.

The main and suffix templates are for candidate display, and note
for the title field for new notes."
  :group 'citar
  :type  '(alist :key-type symbol
                 :value-type string
                 :options (main suffix preview note)))

(defcustom citar-ellipsis nil
  "Ellipsis string to mark ending of truncated display fields.

If t, use the value of `truncate-string-ellipsis`.  If nil, no
ellipsis will be used.  Otherwise, this should be a non-empty
string specifying the ellipsis."
  :group 'citar
  :type '(choice (const :tag "Use 'truncate-string-ellipsis'" t)
                 (const :tag "Disable ellipsis" nil)
                 (const "...")
                 (const "…")
                 (string :tag "Ellipsis string")))

(defcustom citar-format-reference-function
  #'citar-format-reference
  "Function used to render formatted references.

This function is called by `citar-insert-reference' and
`citar-copy-reference'. The default value,
`citar-format-reference', formats references using the `preview'
template set in `citar-template'. To use `citeproc-el' to format
references according to CSL styles, set the value to
`citar-citeproc-format-reference'. Alternatively, set to a custom
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

(defcustom citar-prefilter-entries '(nil . t)
  "When non-nil pre-filter note and library files commands.
For commands like 'citar-open-notes', this will only show
completion candidates that have such notes.

The downside is that, if using Embark and you want to use a different
command for the action, you will not be able to remove the
filter.

The value should be a cons of the form:

(FILTER . TOGGLE)

FILTER turns this on by default

TOGGLE use prefix arg to toggle behavior"
  :group 'citar
  :type 'cons)

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
cache to be rebuilt.  It is intended for \"heavy\" operations
which recreate entire bibliography files using an external
reference manager like Zotero or JabRef."
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
  "Always prompt for selection files with `citar-open'.
If nil, single resources will open without prompting."
  :group 'citar
  :type '(boolean))

;;; Note-handling setup

(defcustom citar-open-note-functions
  '(citar-file--open-note)
  "List of functions to open a note."
  ;; REVIEW change to key only arg?
  :group 'citar
  :type '(function))

(defcustom citar-has-note-functions '(citar-file-has-notes)
  "Functions used for displaying note indicators.

Such functions must take KEY and return non-nil when the
reference has associated notes."
  ;; REVIEW change to key only arg?
  :group 'citar
  :type '(function))

(defcustom citar-open-note-function
  'citar--open-note
  "Function to open a new or existing note.

A note function must take two arguments:

KEY: a string to represent the citekey
ENTRY: an alist with the structured data (title, author, etc.)"
  ;; REVIEW change to key only arg?
  :group 'citar
  :type 'function)

(defcustom citar-create-note-function
  'citar-org-format-note-default
  "Function to create a new note.

A note function must take three arguments:

KEY: a string to represent the citekey
ENTRY: an alist with the structured data (title, author, etc.)
FILEPATH: the file name."
  ;; REVIEW change to key only arg?
  :group 'citar
  :type 'function)

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

(defcustom citar-select-multiple t
  "Use `completing-read-multiple' for selecting citation keys.
When nil, all citar commands will use `completing-read`."
  :type 'boolean
  :group 'citar)

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

;;; Bibliography cache

(defun citar--bibliography-files (&rest buffers)
  "Bibliography file names for BUFFERS.
The elements of BUFFERS are either buffers or the symbol 'global.
Returns the absolute file names of the bibliographies in all
these contexts.

When BUFFERS is empty, return local bibliographies for the
current buffer and global bibliographies."
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
  argument KEY. Only candidates for which this function returns
  non-nil will be offered for completion.

By default the metadata of the table contains the category and
affixation function. METADATA are extra entries for metadata of
the form (KEY . VAL).

The returned completion table can be used with `completing-read`
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
                 (lambda (cand _)
                   (let* ((key (gethash cand candidates))
                          (entry (citar--get-entry key)))
                     (and (or (null filter) (funcall filter key entry))
                          (or (null predicate) (funcall predicate string))))))))
          (complete-with-action action candidates string predicate))))))

(cl-defun citar-select-ref (&optional &key multiple filter)
  "Select bibliographic references.

A wrapper around `completing-read' that returns (KEY . ENTRY),
where ENTRY is a field-value alist.  Therefore `car' of the
return value is the cite key, and `cdr' is an alist of structured
data.

Takes the following optional keyword arguments:

MULTIPLE: if t, calls `completing-read-multiple' and returns an
  alist of (KEY . ENTRY) pairs.

FILTER: if non-nil, should be a predicate function taking
  arguments KEY and ENTRY.  Only candidates for which this
  function returns non-nil will be offered for completion.  For
  example:

  (citar-select-ref :filter (citar-has-note))

  (citar-select-ref :filter (citar-has-file))"
  (let* ((candidates (or (citar--ref-completion-table)
                         (user-error "No bibliography set")))
         (chosen (if (and multiple citar-select-multiple)
                     (citar--select-multiple "References: " candidates
                                             filter 'citar-history citar-presets)
                   (completing-read "Reference: " (citar--completion-table candidates filter)
                                    nil nil nil 'citar-history citar-presets nil))))
    ;; Return a list of keys regardless of 1 or many
    (if (stringp chosen)
        (list (gethash chosen candidates))
      (seq-map
       (lambda (choice)
         (gethash choice candidates))
       chosen))))

(cl-defun citar-select-refs (&optional &key filter)
  "Select bibliographic references.

Call `citar-select-ref' with argument `:multiple'; see its
documentation for the return value and the meaning of
REBUILD-CACHE and FILTER."
  (citar-select-ref :multiple t :filter filter))

(defun citar--multiple-completion-table (selected-hash candidates filter)
  "Return a completion table for multiple selection.
SELECTED-HASH is the hash-table containining selected-candidates.
CANDIDATES is the list of completion candidates, FILTER is the function
to filter them."
  (citar--completion-table
   candidates filter
   `(group-function . (lambda (cand transform)
                        (pcase (list (not (not transform))
                                     (gethash (substring-no-properties cand) ,selected-hash))
                          ('(nil nil) "Select Multiple")
                          ('(nil t)   "Selected")
                          ('(t nil) cand)
                          ('(t t)
                           (add-face-text-property 0 (length cand) 'citar-selection nil cand)
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
  (let* ((selected-hash (make-hash-table :test #'equal)))
    (while (let ((item (minibuffer-with-setup-hook #'citar--setup-multiple-keymap
                         (completing-read
                          (format "%s (%s/%s): " prompt
                                  (hash-table-count selected-hash)
                                  (hash-table-count candidates))
                          (citar--multiple-completion-table selected-hash candidates filter)
                          nil t nil history `("" . ,def)))))
             (unless (equal item "")
               (cond ((gethash item selected-hash)
                      (remhash item selected-hash)
                      (cl-callf cdr (symbol-value history)))
                     (t
                      (puthash item t selected-hash))))
             (not (or (eq last-command #'citar--multiple-exit)
                      (equal item "")))))
    (hash-table-keys selected-hash)))

(defun citar--select-resource (files &optional links)
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
             (group-function . citar--select-group-related-resources)
             (category . multi-category))
         (complete-with-action action (delete-dups resources) string predicate))))))

(defun citar--select-group-related-resources (resource transform)
  "Group RESOURCE by type or TRANSFORM."
  (let ((extension (file-name-extension resource)))
    (if transform
        (if (file-regular-p resource)
            (file-name-nondirectory resource)
          resource)
      (cond
       ((member extension citar-file-note-extensions) "Notes")
       ((string-match "http" resource 0) "Links")
       (t "Library Files")))))

(cl-defun citar--ref-completion-table (&optional (bibs (citar--bibliographies))
                                                 (entries (citar-cache--entries bibs)))
  "Return completion table for cite keys, as a hash table.
In this hash table, keys are a strings with author, date, and
title of the reference.  Values are the cite keys.
Return nil if there are no bibliography files or no entries."
  ;; Populate bibliography cache.
  (when bibs
    (let* ((preformatted (citar-cache--preformatted bibs))
           (hasnotep (citar-has-notes-for-entries entries))
           (hasfilep (citar-has-files-for-entries entries))
           (hasnotetag (propertize " has:notes" 'invisible t))
           (hasfiletag (propertize " has:files" 'invisible t))
           (symbolswidth (string-width (citar--symbols-string t t t)))
           (width (- (frame-width) symbolswidth 2))
           (completions (make-hash-table :test 'equal :size (hash-table-count entries))))
      (maphash
       (lambda (citekey _entry)
         (let* ((hasfile (and hasfilep (funcall hasfilep citekey)))
                (hasnote (and hasnotep (funcall hasnotep citekey)))
                (hasnote t)
                (preform (or (gethash citekey preformatted)
                             (error "No preformatted candidate string: %s" citekey)))
                (display (citar-format--star-widths
                          (- width (car preform)) (cdr preform)
                          t citar-ellipsis))
                (tagged (if (not (or hasfile hasnote))
                            display
                          (concat display
                                  (when hasnote hasnotetag)
                                  (when hasfile hasfiletag)))))
           (puthash tagged citekey completions)))
       entries)
      completions)))

(defun citar--extract-candidate-citekey (candidate)
  "Extract the citation key from string CANDIDATE."
  (unless (string-empty-p candidate)
    (if (= ?\" (aref candidate 0))
        (read candidate)
      (substring-no-properties candidate 0 (cl-position ?\s candidate)))))

;;; Major-mode functions

(defun citar--get-major-mode-function (key &optional default)
  "Return function associated with KEY in `major-mode-functions'.
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

;;; Data access functions

(defun citar--get-entry (key)
  "Return entry for KEY, as an association list."
  (citar-cache--entry key (citar--bibliographies)))

(defun citar--get-entries ()
  (citar-cache--entries (citar--bibliographies)))

(defun citar--get-value (field key-or-entry)
  "Return FIELD value for KEY-OR-ENTRY."
  (let ((entry (if (stringp key-or-entry)
                   (citar--get-entry key-or-entry)
                 key-or-entry)))
    (cdr (assoc-string field entry))))

(defun citar--field-with-value (fields entry)
  "Return the first field that has a value in ENTRY among FIELDS ."
  (seq-find (lambda (field) (citar--get-value field entry)) fields))

(defun citar--display-value (fields entry)
  "Return the first non nil value for ENTRY among FIELDS .

The value is transformed using `citar-display-transform-functions'"
  (let ((field (citar--field-with-value fields entry)))
    (seq-reduce (lambda (string fun)
                  (if (or (eq t (car fun))
                          (member field (car fun)))
                      (funcall (cdr fun) string)
                    string))
                citar-display-transform-functions
                ;; Make sure we always return a string, even if empty.
                (or (citar--get-value field entry) ""))))

;; Lifted from bibtex-completion
(defun citar-clean-string (s)
  "Remove quoting brackets and superfluous whitespace from string S."
  (replace-regexp-in-string "[\n\t ]+" " "
                            (replace-regexp-in-string "[\"{}]+" "" s)))

(defun citar-shorten-names (names)
  "Return a list of family names from a list of full NAMES.

To better accommodate corporate names, this will only shorten
personal names of the form \"family, given\"."
  (when (stringp names)
    (mapconcat
     (lambda (name)
       (if (eq 1 (length name))
           (cdr (split-string name " "))
         (car (split-string name ", "))))
     (split-string names " and ") ", ")))

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
  (delete-dups (append (citar--fields-in-formats)
                       (when citar-file-variable
                         (list citar-file-variable))
                       (when citar-crossref-variable
                         (list citar-crossref-variable))
                       citar-additional-fields)))

(defun citar-has-file-field (entries)
  "Return predicate to test if bibliography entry has a file field."
  (when-let ((fieldname citar-file-variable))
    (lambda (key)
      (when-let ((entry (map-elt entries key)))
        (citar--get-value fieldname entry)))))

(defun citar-has-file-p (key &optional entry)
  "Return predicate testing whether entry has associated files.

Return a function that takes arguments KEY and ENTRY and returns
non-nil when the entry has associated files, either in
`citar-library-paths` or the field named in
`citar-file-variable`.

Note: for performance reasons, this function should be called
once per command; the function it returns can be called
repeatedly."
  (when-let ((entry (or entry (citar--get-entry entry)))
             (hasfilep (citar-has-files-for-entries '((key . entry)))))
    (funcall hasfilep key)))

(defun citar-has-note-p (key &optional entry)
  "Return predicate testing whether entry has associated notes.

Return a function that takes arguments KEY and ENTRY and returns
non-nil when the entry has associated notes in `citar-notes-paths`.

Note: for performance reasons, this function should be called
once per command; the function it returns can be called
repeatedly."
  (when-let ((entry (or entry (citar--get-entry entry)))
             (hasnotep (citar-has-notes-for-entries '((key . entry)))))
    (funcall hasnotep key)))

(defun citar-has-notes-for-entries (entries)
  (citar--has-resources-for-entries citar-has-note-functions entries))

(defun citar-has-files-for-entries (entries)
  (citar--has-resources-for-entries citar-has-file-functions entries))

(defun citar--has-resources-for-entries (functions entries)
  "Return predicate combining results of calling FUNCTIONS.

FUNCTIONS should be a list of functions, each of which returns a
predicate function that takes KEY and ENTRY arguments. Run each
function in the list, and return a predicate that is the logical
or of all these predicates.

The FUNCTIONS may also return nil, which is treated as an
always-false predicate and ignored. If there is only one non-nil
predicate, return it."
  (when-let ((predicates (delq nil (mapcar (lambda (fn)
                                             (funcall fn entries))
                                           functions))))
    (let ((hasresourcep (if (null (cdr predicates))
                            ;; optimization for single predicate; just use it directly
                            (car predicates)
                          ;; otherwise, call all predicates until one returns non-nil
                          (lambda (citekey)
                            (seq-some (lambda (predicate)
                                        (funcall predicate citekey))
                                      predicates)))))
      (if-let ((crossref citar-crossref-variable))
          (lambda (citekey)
            (or (funcall hasresourcep citekey)
                (when-let ((entry (map-elt entries citekey))
                           (crossrefkey (citar--get-value crossref entry)))
                  (funcall hasresourcep crossrefkey))))
        hasresourcep))))

(defun citar--ref-affix (cands)
  "Add affixation prefix to CANDS."
  (seq-map
   (lambda (candidate)
     (let ((symbols (citar--ref-make-symbols candidate)))
       (list candidate symbols "")))
   cands))

(defun citar--ref-make-symbols (cand)
  "Make CAND annotation or affixation string for has-symbols."
  (let ((candidate-symbols (citar--symbols-string
                            (string-match "has:file" cand)
                            (string-match "has:note" cand)
                            (string-match "has:link" cand))))
    candidate-symbols))

(defun citar--ref-annotate (cand)
  "Add annotation to CAND."
  ;; REVIEW/TODO we don't currently use this, but could, for Emacs 27.
  (citar--ref-make-symbols cand))

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

(defun citar--get-template (template-name)
  "Return template string for TEMPLATE-NAME."
  (or
   (cdr (assq template-name citar-templates))
   (when (eq template-name 'completion)
     (concat (propertize (citar--get-template 'main) 'face 'citar-highlight)
             (propertize (citar--get-template 'suffix) 'face 'citar)))
   (error "No template for \"%s\" - check variable 'citar-templates'" template-name)))

(defun citar--get-link (entry)
  "Return a link for an ENTRY."
  (let* ((field (citar--field-with-value '(doi pmid pmcid url) entry))
         (base-url (pcase field
                     ('doi "https://doi.org/")
                     ('pmid "https://www.ncbi.nlm.nih.gov/pubmed/")
                     ('pmcid "https://www.ncbi.nlm.nih.gov/pmc/articles/"))))
    (when field
      (concat base-url (citar--get-value field entry)))))

;; REVIEW I removed 'citar--ensure-entries'

;;;###autoload
(defun citar-insert-preset ()
  "Prompt for and insert a predefined search."
  (interactive)
  (unless (minibufferp)
    (user-error "Command can only be used in minibuffer"))
  (when-let ((enable-recursive-minibuffers t)
             (search (completing-read "Preset: " citar-presets)))
    (insert search)))

(defun citar--stringify-keys (keys)
  "Return a list of KEYS as a crm-string for `embark'."
  (if (listp keys) (string-join keys " & ") keys))

;;; Commands

;;;###autoload
(defun citar-open (keys)
  "Open related resources (links or files) for KEYS."
  (interactive (list
                (list (citar-select-ref))))
  (when (and citar-library-paths
             (stringp citar-library-paths))
    (message "Make sure 'citar-library-paths' is a list of paths"))
  (let* ((embark-default-action-overrides
          '((multi-category . citar--open-multi)
            (file . citar-file-open)
            (url . browse-url)))
         (key-entry-alist (citar--ensure-entries keys-entries))
         (files
          (citar-file--files-for-multiple-entries
           keys
           (append citar-library-paths citar-notes-paths)
           ;; find files with any extension:
           nil))
         (links
          (seq-map
           (lambda (key)
             (citar--get-link key))
           keys))
         (resource-candidates (delete-dups (append files (remq nil links)))))
    (cond
     ((eq nil resource-candidates)
      (error "No associated resources"))
     ((unless citar-open-prompt
        (eq 1 (length resource-candidates)))
      (citar--open-multi (car resource-candidates)))
     (t (citar--open-multi
         (citar--select-resource files links))))))

(defun citar--open-multi (selection)
  "Act appropriately on SELECTION when type is `multi-category'.
For use with `embark-act-all'."
  (cond ((string-match "http" selection 0)
         (browse-url selection))
        ((member t (mapcar (lambda (x)
                             (file-in-directory-p selection x))
                           citar-notes-paths))
         (find-file selection))
        (t (citar-file-open selection))))

(defun citar--library-file-action (key action)
  "Run ACTION on file associated with KEY."
  (let* ((fn (pcase action
               ('open 'citar-file-open)
               ('attach 'mml-attach-file)))
         (entry (citar--get-entry key))
         (files
          (citar-file--files-for-entry
           key
           entry
           citar-library-paths
           citar-library-file-extensions))
         (file
          (pcase (length files)
            (1 (car files))
            ((guard (> 1))
             (citar--select-resource files)))))
    (if file
        (funcall fn file)
      (message "No associated file"))))

;;;###autoload
(defun citar-open-library-file (key-entry)
  "Open library file associated with the KEY-ENTRY.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref)))
  (let ((embark-default-action-overrides '((file . citar-file-open))))
    (when (and citar-library-paths
               (stringp citar-library-paths))
      (error "Make sure 'citar-library-paths' is a list of paths"))
    (citar--library-file-action key 'open)))

;;;###autoload
(defun citar-open-notes (key)
  "Open notes associated with the KEY.
With prefix, rebuild the cache before offering candidates."
  ;; REVIEW KEY, or KEYS
  (interactive (list (citar-select-ref)))
  (let* ((embark-default-action-overrides '((file . find-file)))
         (entry (citar--get-entry key)))
    (if (listp citar-open-note-functions)
        (citar--open-notes (car key) entry)
      (error "Please change the value of 'citar-open-note-functions' to a list"))))

(defun citar--open-notes (key entry)
  "Open note(s) associated with KEY and ENTRY."
  (or (seq-some
       (lambda (opener)
         (funcall opener key entry)) citar-open-note-functions)
      (funcall citar-create-note-function key entry)))

;;;###autoload
(defun citar-open-entry (key)
  "Open bibliographic entry associated with the KEY.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref)))
  (when-let ((bibtex-files (citar--bibliography-files)))
    (bibtex-search-entry (car key) t nil t)))

;;;###autoload
(defun citar-insert-bibtex (keys)
  "Insert bibliographic entry associated with the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs)))
  (dolist (key keys)
    (citar--insert-bibtex key)))

(defun citar--insert-bibtex (key)
  "Insert the bibtex entry for KEY at point."
  (let* ((bibtex-files
          (citar--bibliography-files))
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
the first entry in `citar-bibliography', and created in the same
directory as current buffer."
  (interactive)
  (let* ((keys (citar--major-mode-function 'list-keys #'ignore))
         (ext (file-name-extension (car citar-bibliography)))
         (file (format "%slocal-bib.%s" (file-name-directory buffer-file-name) ext)))
    (with-temp-file file
      (dolist (key keys)
        (citar--insert-bibtex key)))))

;;;###autoload
(defun citar-open-link (key)
  "Open URL or DOI link associated with the KEY in a browser.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref
                      :rebuild-cache current-prefix-arg)))
  (let* ((entry (citar--get-entry key))
         (link (citar--get-link entry)))
    (if link
        (browse-url link)
      (message "No link found for %s" key))))

;;;###autoload
(defun citar-insert-citation (keys &optional arg)
  "Insert citation for the KEYS.

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
   keys
   arg))

(defun citar-insert-edit (&optional arg)
  "Edit the citation at point.
ARG is forwarded to the mode-specific insertion function given in
`citar-major-mode-functions`."
  (interactive "P")
  (citar--major-mode-function
   'insert-edit
   (lambda (&rest _)
     (message "Citation editing is not supported for %s" major-mode))
   arg))

;;;###autoload
(defun citar-insert-reference (keys)
  "Insert formatted reference(s) associated with the KEYS."
  (interactive (list (citar-select-refs)))
  (insert (funcall citar-format-reference-function keys)))

;;;###autoload
(defun citar-copy-reference (keys)
  "Copy formatted reference(s) associated with the KEYS."
  (interactive (list (citar-select-refs)))
  (let ((references (funcall citar-format-reference-function keys)))
    (if (not (equal "" references))
        (progn
          (kill-new references)
          (message (format "Copied:\n%s" references)))
      (message "Key not found."))))

(defun citar-format-reference (keys)
  "Return formatted reference(s) for the elements of KEYS."
  (let* ((entries (mapcar #'citar--get-entry keys))
         (template (citar--get-template 'preview)))
    (with-temp-buffer
      (dolist (entry entries)
        (insert (citar-format--entry template entry)))
      (buffer-string))))

;;;###autoload
(defun citar-insert-keys (keys)
  "Insert KEYS citekeys.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-refs
                      :rebuild-cache current-prefix-arg)))
  (citar--major-mode-function
   'insert-keys
   #'citar--insert-keys-comma-separated
   keys))

(defun citar--insert-keys-comma-separated (keys)
  "Insert comma separated KEYS."
  (insert (string-join keys ", ")))

;;;###autoload
(defun citar-attach-library-file (key)
  "Attach library file associated with KEY to outgoing MIME message.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (citar-select-ref)))
  (let ((embark-default-action-overrides '((file . mml-attach-file))))
    (when (and citar-library-paths
               (stringp citar-library-paths))
      (error "Make sure 'citar-library-paths' is a list of paths"))
    (citar--library-file-action key-entry 'attach)))

(defun citar--add-file-to-library (key)
  "Add a file to the library for KEY.
The FILE can be added from an open buffer, a file path, or a
URL."
  (let* ((source
          (char-to-string
           (read-char-choice
            "Add file from [b]uffer, [f]ile, or [u]rl? " '(?b ?f ?u))))
         (directory (if (cdr citar-library-paths)
                        (completing-read "Directory: " citar-library-paths)
                      (car citar-library-paths)))
         (file-path
          ;; Create the path without extension here.
          (expand-file-name key directory)))
    (pcase source
      ("b"
       (with-current-buffer (read-buffer-to-switch "Add file buffer: ")
         (let ((extension (file-name-extension (buffer-file-name))))
           (write-file (concat file-path "." extension) t))))
      ("f"
       (let* ((file (read-file-name "Add file: " nil nil t))
              (extension (file-name-extension file)))
         (copy-file file
                    (concat file-path "." extension) 1)))
      ("u"
       (let* ((url (read-string "Add file URL: "))
              (extension (url-file-extension url)))
         (when (< 1 extension)
           ;; TODO what if there is no extension?
           (url-copy-file url (concat file-path extension) 1)))))))

;;;###autoload
(defun citar-add-file-to-library (key)
  "Add a file to the library for KEY.
The FILE can be added either from an open buffer, a file, or a
URL."
  (interactive (list (citar-select-ref
                      :rebuild-cache current-prefix-arg)))
  (citar--add-file-to-library (car key-entry)))

;;;###autoload
(defun citar-run-default-action (keys)
  "Run the default action `citar-default-action' on KEYS."
  (funcall citar-default-action keys))

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
