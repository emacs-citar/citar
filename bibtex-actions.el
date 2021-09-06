;;; bibtex-actions.el --- Bibliographic commands based on completing-read -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <https://github.com/bdarcus>
;; Maintainer: Bruce D'Arcus <https://github.com/bdarcus>
;; Created: February 27, 2021
;; License: GPL-3.0-or-later
;; Version: 0.4
;; Homepage: https://github.com/bdarcus/bibtex-actions
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
(require 'bibtex-actions-file)
(require 'bibtex-completion)
(require 'parsebib)
(require 's)

(declare-function org-element-context "org-element")
(declare-function org-element-property "org-element")
(declare-function org-element-type "org-element")
(declare-function org-cite-get-references "org-cite")
(declare-function org-cite-register-processor "org-cite")
(declare-function org-cite-make-insert-processor "org-cite")
(declare-function org-cite-basic--complete-style "org-cite")
(declare-function embark-act "ext:embark")

;;; Declare variables for byte compiler

(defvar crm-separator)
(defvar embark-keymap-alist)
(defvar embark-target-finders)
(defvar embark-general-map)
(defvar embark-meta-map)
(defvar bibtex-actions-file-open-note-function)
(defvar bibtex-actions-file-extensions)
(defvar bibtex-actions-file-open-prompt)
(defvar bibtex-actions-file-variable)

;;; Variables

(defface bibtex-actions
  '((t :inherit font-lock-doc-face))
  "Default Face for `bibtex-actions' candidates."
  :group 'bibtex-actions)

(defface bibtex-actions-highlight
  '((t :weight bold))
  "Face used to highlight content in `bibtex-actions' candidates."
  :group 'bibtex-actions)

(defcustom bibtex-actions-bibliography
  (bibtex-actions-file--normalize-paths bibtex-completion-bibliography)
  "A list of bibliography files."
  ;; The bibtex-completion default is likely to be removed in the future.
  :group 'bibtex-actions
  :type '(repeat file))

(defcustom bibtex-actions-library-paths
  (bibtex-actions-file--normalize-paths bibtex-completion-library-path)
  "A list of files paths for related PDFs, etc."
  ;; The bibtex-completion default is likely to be removed in the future.
  :group 'bibtex-actions
  :type '(repeat path))

(defcustom bibtex-actions-notes-paths
  (bibtex-actions-file--normalize-paths bibtex-completion-notes-path)
  "A list of file paths for bibliographic notes."
  ;; The bibtex-completion default is likely to be removed in the future.
  :group 'bibtex-actions
  :type '(repeat path))


(defcustom bibtex-actions-open-file-function 'find-file
  "Function to use to open files."
  ;; tODO maybe this should be higher-level; eg:
  ;; 'bibtex-actions-open-file?
  :group 'bibtex-actions
  :type '(function))

(defcustom bibtex-actions-open-library-file-external t
  "Whether to open a library file in an external application."
  :group 'bibtex-actions
  :type '(boolean))


(defcustom bibtex-actions-template
  (cons
   "${author editor:30}     ${date year issued:4}     ${title:48}"
   "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
  "Configures formatting for the bibliographic entry.
car is for the main body of the candidate and cdr is for suffix.
The same string is used for display and for search."
    :group 'bibtex-actions
    :type  '(cons string string))

(defcustom bibtex-actions-display-transform-functions
  ;; TODO change this name, as it might be confusing?
  '((t  . bibtex-actions-clean-string)
    (("author" "editor") . bibtex-actions-shorten-names))
  "Configure transformation of field display values from raw values.

All functions that match a particular field are run in order."
  :group 'bibtex-actions
  :type '(alist :key-type   (choice (const t) (repeat string))
                :value-type function))

(defcustom bibtex-actions-symbols
  `((file  .  ("⌘" . " "))
    (note .   ("✎" . " "))
    (link .   ("🔗" . "  ")))
  "Configuration alist specifying which symbol or icon to pick for a bib entry.
This leaves room for configurations where the absense of an item
may be indicated with the same icon but a different face.

To avoid alignment issues make sure that both the car and cdr of a symbol have
the same width."
  :group 'bibtex-actions
  :type '(alist :key-type string
                :value-type (choice (string :tag "Symbol"))))

(defcustom bibtex-actions-symbol-separator " "
  "The padding between prefix symbols."
  :group 'bibtex-actions
  :type 'string)

(defcustom bibtex-actions-force-refresh-hook nil
  "Hook run when user forces a (re-) building of the candidates cache.
This hook is only called when the user explicitly requests the
cache to be rebuilt.  It is intended for 'heavy' operations which
recreate entire bibliography files using an external reference
manager like Zotero or JabRef."
  :group 'bibtex-actions
  :type '(repeat function))

(defcustom bibtex-actions-default-action 'bibtex-actions-open
  "The default action for the `bibtex-actions-at-point' command."
  :group 'bibtex-actions
  :type 'function)

(defcustom bibtex-actions-at-point-fallback 'prompt
  "Fallback action for `bibtex-actions-at-point'.
The action is used when no citation key is found at point.
`prompt' means choosing entries via `bibtex-actions-select-keys'
and nil means no action."
  :group 'bibtex-actions
  :type '(choice (const :tag "Prompt" 'prompt)
                 (const :tag "Ignore" nil)))

(defcustom bibtex-actions-at-point-function 'bibtex-actions-dwim
  "The function to run for 'bibtex-actions-at-point'."
  :group 'bibtex-actions
  :type 'function)

;;; History, including future history list.

(defvar bibtex-actions-history nil
  "Search history for `bibtex-actions'.")

(defcustom bibtex-actions-presets nil
  "List of predefined searches."
  :group 'bibtex-actions
  :type '(repeat string))

;;; Keymaps

(defvar bibtex-actions-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") '("add pdf attachment" . bibtex-actions-add-pdf-attachment))
    (define-key map (kbd "a") '("add pdf to library" . bibtex-actions-add-pdf-to-library))
    (define-key map (kbd "b") '("insert bibtex" . bibtex-actions-insert-bibtex))
    (define-key map (kbd "c") '("insert citation" . bibtex-actions-insert-citation))
    (define-key map (kbd "k") '("insert key" . bibtex-actions-insert-key))
    (define-key map (kbd "fr") '("insert formatted reference" . bibtex-actions-insert-reference))
    (define-key map (kbd "o") '("open source document" . bibtex-actions-open))
    (define-key map (kbd "e") '("open bibtex entry" . bibtex-actions-open-entry))
    (define-key map (kbd "l") '("open source URL or DOI" . bibtex-actions-open-link))
    (define-key map (kbd "n") '("open notes" . bibtex-actions-open-notes))
    (define-key map (kbd "f") '("open library files" . bibtex-actions-open-library-files))
    (define-key map (kbd "r") '("refresh" . bibtex-actions-refresh))
    ;; Embark doesn't currently use the menu description.
    ;; https://github.com/oantolin/embark/issues/251
    (define-key map (kbd "RET") '("default action" . bibtex-actions-run-default-action))
    map)
  "Keymap for 'bibtex-actions'.")

(defvar bibtex-actions-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") '("open source document" . bibtex-actions-open))
    (define-key map (kbd "e") '("open bibtex entry" . bibtex-actions-open-entry))
    (define-key map (kbd "l") '("open source URL or DOI" . bibtex-actions-open-link))
    (define-key map (kbd "n") '("open notes" . bibtex-actions-open-notes))
    (define-key map (kbd "f") '("open library files" . bibtex-actions-open-library-files))
    (define-key map (kbd "r") '("refresh library" . bibtex-actions-refresh))
    ;; Embark doesn't currently use the menu description.
    ;; https://github.com/oantolin/embark/issues/251
    (define-key map (kbd "RET") '("default action" . bibtex-actions-run-default-action))
    map)
  "Keymap for Embark citation-key actions.")

;;; Completion functions

(cl-defun bibtex-actions-select-keys (&optional &key rebuild-cache)
  "Read bibliographic entries for completing citekeys.

This provides a wrapper around 'completing-read-multiple', with
the following optional arguments:

'REBUILD-CACHE' if t, forces rebuilding the cache before
offering the selection candidates"
  (let* ((crm-separator "\\s-*&\\s-*")
         (candidates (bibtex-actions--get-candidates rebuild-cache))
         (chosen
          (completing-read-multiple
           "References: "
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   (affixation-function . bibtex-actions--affixation)
                   (category . bib-reference))
               (complete-with-action action candidates string predicate)))
           nil nil nil
           'bibtex-actions-history bibtex-actions-presets nil)))
    (seq-map
     (lambda (choice)
       ;; Collect citation keys of selected candidate(s).
       (or (cadr (assoc choice candidates))
           ;; Key is literal coming from embark, just pass it on
           choice))
     chosen)))

(defun bibtex-actions-select-file (files)
  "Select file(s) from a list of FILES."
  ;; TODO add links to candidates
  (completing-read
   "Open related resource: "
   (lambda (string predicate action)
     (if (eq action 'metadata)
         `(metadata
           (group-function . bibtex-actions-select-group-related-sources)
           (category . file))
       (complete-with-action action files string predicate)))))

(defun bibtex-actions-select-group-related-sources (file transform)
  "Group by FILE by source, TRANSFORM."
    (let ((extension (file-name-extension file)))
      (if transform file
        ;; Transform for grouping and group title display.
        (cond
         ((string= extension (or "org" "md")) "Notes")
          (t "Library Files")))))


(defun bibtex-actions--local-files-to-cache ()
  "The local bibliographic files not included in the global bibliography."
  ;; We cache these locally to the buffer.
  (let* ((local-bib-files
          (bibtex-actions-file--normalize-paths
           (bibtex-completion-find-local-bibliography))))
    (seq-difference local-bib-files bibtex-actions-bibliography)))

(defun bibtex-actions-get-value (field item)
  "Return the FIELD value for ITEM."
  (cdr (assoc-string field item 'case-fold)))

(defun bibtex-actions-has-a-value (fields item)
  "Return the first field that has a value in ITEM among FIELDS ."
  (seq-find (lambda (field) (bibtex-actions-get-value field item)) fields))

(defun bibtex-actions-display-value (fields item)
  "Return the first non nil value for ITEM among FIELDS .

The value is transformed using `bibtex-actions-display-transform-functions'"
  (let ((field (bibtex-actions-has-a-value fields item)))
    (seq-reduce (lambda (string fun)
                  (if (or (eq t (car fun))
                          (member field (car fun)))
                      (funcall (cdr fun) string)
                    string))
                bibtex-actions-display-transform-functions
            ;; Make sure we always return a string, even if empty.
                (or (bibtex-actions-get-value field item) ""))))

;; Lifted from bibtex-completion
(defun bibtex-actions-clean-string (s)
  "Remove quoting brackets and superfluous whitespace from string S."
  (replace-regexp-in-string "[\n\t ]+" " "
         (replace-regexp-in-string "[\"{}]+" "" s)))

(defun bibtex-actions-shorten-names (names)
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

(defun bibtex-actions--fields-in-formats ()
  "Find the fields to mentioned in the templates."
  (cl-flet ((fields-for-format
             (format-string)
             (split-string
              (s-format format-string
                        (lambda (fields-string) (car (split-string fields-string ":"))))
             "[ ]+")))
     (seq-mapcat #'fields-for-format
                 (list (car bibtex-actions-template)
                       (cdr bibtex-actions-template)))))

(defun bibtex-actions--fields-to-parse ()
  "Determine the fields to parse from the template."
  (seq-concatenate 'list
                   (bibtex-actions--fields-in-formats)
                   (list "doi" "url" bibtex-actions-file-variable)))

(defun bibtex-actions--format-candidates (files &optional context)
  "Format candidates from FILES, with optional hidden CONTEXT metadata.
This both propertizes the candidates for display, and grabs the
key associated with each one."
  (let* ((candidates ())
         (raw-candidates
          (parsebib-parse files :fields (bibtex-actions--fields-to-parse)))
         (main-width (bibtex-actions--format-width (car bibtex-actions-template)))
         (suffix-width (bibtex-actions--format-width (cdr bibtex-actions-template)))
         (symbols-width (string-width (bibtex-actions--symbols-string t t t)))
         (star-width (- (frame-width) (+ 2 symbols-width main-width suffix-width))))
    (maphash
     (lambda (citekey entry)
       (let* ((files
               (when (or (bibtex-actions-get-value
                          bibtex-actions-file-variable entry)
                         (bibtex-actions-file--files-for-key
                          citekey bibtex-actions-library-paths bibtex-actions-file-extensions))
                 " has:files"))
              (notes
               (when (bibtex-actions-file--files-for-key
                      citekey bibtex-actions-notes-paths bibtex-actions-file-extensions)
                 " has:notes"))
              (link (when (bibtex-actions-has-a-value '("doi" "url") entry)
                      "has:link"))
              (candidate-main
               (bibtex-actions--format-entry
                entry
                star-width
                (car bibtex-actions-template)))
              (candidate-suffix
               (bibtex-actions--format-entry
                entry
                star-width
                (cdr bibtex-actions-template)))
              ;; We display this content already using symbols; here we add back
              ;; text to allow it to be searched, and citekey to ensure uniqueness
              ;; of the candidate.
              (candidate-hidden (s-join " " (list files notes link context citekey))))
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
             (propertize candidate-main 'face 'bibtex-actions-highlight) " "
             (propertize candidate-suffix 'face 'bibtex-actions) " "
             (propertize candidate-hidden 'invisible t)))
           (cons citekey entry))
          candidates)))
       raw-candidates)
    candidates))

  (defun bibtex-actions--affixation (cands)
    "Add affixation prefix to CANDS."
    (seq-map
     (lambda (candidate)
       (let ((candidate-symbols (bibtex-actions--symbols-string
                                 (string-match "has:files" candidate)
                                 (string-match "has:note" candidate)
                                 (string-match "has:link" candidate))))
         (list candidate candidate-symbols "")))
     cands))

(defun bibtex-actions--symbols-string (has-files has-note has-link)
  "String for display from booleans HAS-FILES HAS-LINK HAS-NOTE."
  (cl-flet ((thing-string (has-thing thing-symbol)
                          (if has-thing
                              (cadr (assoc thing-symbol bibtex-actions-symbols))
                            (cddr (assoc thing-symbol bibtex-actions-symbols)))))
    (seq-reduce (lambda (constructed newpart)
                  (let* ((str (concat constructed newpart
                                      bibtex-actions-symbol-separator))
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

(defvar bibtex-actions--candidates-cache 'uninitialized
  "Store the global candidates list.

Default value of 'uninitialized is used to indicate that cache
has not yet been created")

(defvar-local bibtex-actions--local-candidates-cache 'uninitialized
  ;; We use defvar-local so can maintain per-buffer candidate caches.
  "Store the local (per-buffer) candidates list.")

(defun bibtex-actions-get-entry (key)
  "Return the cached entry for KEY."
  (if (or (eq 'uninitialized bibtex-actions--candidates-cache)
          (eq 'uninitialized bibtex-actions--local-candidates-cache))
      (message "Something is wrong; your library is not initialized.")
    (cddr (seq-find
           (lambda (entry)
             (string-equal key (cadr entry)))
           (bibtex-actions--get-candidates)))))

(defun bibtex-actions--get-candidates (&optional force-rebuild-cache)
  "Get the cached candidates.
If the cache is unintialized, this will load the cache.
If FORCE-REBUILD-CACHE is t, force reload the cache."
  (if force-rebuild-cache
      (bibtex-actions-refresh force-rebuild-cache)
      (when (eq 'uninitialized bibtex-actions--candidates-cache)
        (bibtex-actions-refresh nil 'global))
      (when (eq 'uninitialized bibtex-actions--local-candidates-cache)
        (bibtex-actions-refresh nil 'local)))
  (seq-concatenate 'list
                   bibtex-actions--local-candidates-cache
                   bibtex-actions--candidates-cache))

(defun bibtex-actions-get-link (key)
  "Return a link for a KEY."
  (let* ((entry (bibtex-actions-get-entry key))
         (field (bibtex-actions-has-a-value '(doi pmid pmcid url) entry))
         (base-url (pcase field
                     ('doi "https://doi.org/")
                     ('pmid "https://www.ncbi.nlm.nih.gov/pubmed/")
                     ('pmcid "https://www.ncbi.nlm.nih.gov/pmc/articles/"))))
    (when field
      (concat base-url (bibtex-actions-get-value field entry)))))

;;;###autoload
(defun bibtex-actions-refresh (&optional force-rebuild-cache scope)
  "Reload the candidates cache.

If called interactively with a prefix or if FORCE-REBUILD-CACHE
is non-nil, also run the `bibtex-actions-before-refresh-hook' hook.

If SCOPE is `global' only global cache is refreshed, if it is
`local' only local cache is refreshed.  With any other value both
are refreshed."
  (interactive (list current-prefix-arg nil))
  (when force-rebuild-cache
    (run-hooks 'bibtex-actions-force-refresh-hook))
  (unless (eq 'local scope)
    (setq bibtex-actions--candidates-cache
      (bibtex-actions--format-candidates
       bibtex-actions-bibliography)))
  (unless (eq 'global scope)
    (setq bibtex-actions--local-candidates-cache
          (bibtex-actions--format-candidates
           (bibtex-actions--local-files-to-cache) "is:local"))))

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

(defun bibtex-actions--format-width (format-string)
  "Calculate minimal width needed by the FORMAT-STRING."
  (let ((content-width (apply #'+
                              (seq-map #'string-to-number
                                       (split-string format-string ":"))))
        (whitespace-width (string-width (s-format format-string
                                                  (lambda (_) "")))))
    (+ content-width whitespace-width)))

(defun bibtex-actions--fit-to-width (value width)
  "Propertize the string VALUE so that only the WIDTH columns are visible."
  (let* ((truncated-value (truncate-string-to-width value width))
         (display-value (truncate-string-to-width truncated-value width 0 ?\s)))
    (if (> (string-width value) width)
        (concat display-value (propertize (substring value (length truncated-value))
                                          'invisible t))
      display-value)))

(defun bibtex-actions--format-entry (entry width format-string)
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
            (display-value (bibtex-actions-display-value field-names entry)))
       (bibtex-actions--fit-to-width display-value display-width)))))

(defun bibtex-actions--format-entry-no-widths (entry format-string)
  "Format ENTRY for display per FORMAT-STRING."
  (s-format
   format-string
   (lambda (raw-field)
     (let ((field-names (split-string raw-field "[ ]+")))
       (bibtex-actions-display-value field-names entry)))))

;;; At-point functions

;;; Org-cite

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
  (when-let ((keys (or (bibtex-actions-get-key-org-cite)
                      (bibtex-completion-key-at-point))))
    (cons 'citation-key (bibtex-actions--stringify-keys keys))))

(defun bibtex-actions--stringify-keys (keys)
  "Return a list of KEYS as a crm-string for `embark'."
  (if (listp keys) (string-join keys " & ") keys))

;;; Commands

;;;###autoload
(defun bibtex-actions-open (keys)
  "Open related resource (link or file) for KEYS."
  ;; TODO add links
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
  (let* ((files
         (bibtex-actions-file--files-for-multiple-keys
          keys
          (append bibtex-actions-library-paths bibtex-actions-notes-paths)
          bibtex-actions-file-extensions))
         (links
          (seq-map
           (lambda (key)
             (bibtex-actions-get-link key))
           keys))
        (resources
         (completing-read-multiple "Related resources: "
                                   (append files (remq nil links)))))
    (dolist (resource resources)
      (cond ((string-match "http" resource 0)
             (browse-url resource))
            ((equal (file-name-extension resource) (or "org" "md"))
             (funcall bibtex-actions-open-file-function resource))
            (t (bibtex-actions-file-open-external resource))))))

;;;###autoload
(defun bibtex-actions-open-library-files (keys)
 "Open library files associated with the KEYS.

With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
  (let ((files
         (bibtex-actions-file--files-for-multiple-keys
          keys bibtex-actions-library-paths bibtex-actions-file-extensions)))
    (if files
        (dolist (file files)
          (if bibtex-actions-open-library-file-external
              (bibtex-actions-file-open-external file)
            (funcall bibtex-actions-file-open-function file)))
      (message "No file(s) found for %s" keys))))

(make-obsolete 'bibtex-actions-open-pdf
               'bibtex-actions-open-library-files "1.0")

;;;###autoload
(defun bibtex-actions-open-notes (keys)
  "Open notes associated with the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
  (dolist (key keys)
    (funcall bibtex-actions-file-open-note-function key)))

;;;###autoload
(defun bibtex-actions-open-entry (keys)
  "Open bibliographic entry associated with the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
 (bibtex-completion-show-entry keys))

;;;###autoload
(defun bibtex-actions-open-link (keys)
  "Open URL or DOI link associated with the KEYS in a browser.
With prefix, rebuild the cache before offering candidates."
  ;;      (browse-url-default-browser "https://google.com")
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
  (dolist (key keys)
    (let* ((entry (bibtex-actions-get-entry key))
           (doi
            (bibtex-actions-get-value "doi" entry))
           (doi-url
            (when doi
              (concat "https://doi.org/" doi)))
           (url (bibtex-actions-get-value "url" entry))
           (link (or doi-url url)))
      (if link
          (browse-url-default-browser link)
        (message "No link found for %s" key)))))

;;;###autoload
(defun bibtex-actions-insert-citation (keys)
  "Insert citation for the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
 (bibtex-completion-insert-citation keys))

;;;###autoload
(defun bibtex-actions-insert-reference (keys)
  "Insert formatted reference(s) associated with the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
  (bibtex-completion-insert-reference keys))

;;;###autoload
(defun bibtex-actions-insert-key (keys)
  "Insert BibTeX KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
 (bibtex-completion-insert-key keys))

;;;###autoload
(defun bibtex-actions-insert-bibtex (keys)
  "Insert bibliographic entry associated with the KEYS.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
 (bibtex-completion-insert-bibtex keys))

;;;###autoload
(defun bibtex-actions-add-pdf-attachment (keys)
  "Attach PDF(s) associated with the KEYS to email.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
 (bibtex-completion-add-PDF-attachment keys))

;;;###autoload
(defun bibtex-actions-add-pdf-to-library (keys)
 "Add PDF associated with the KEYS to library.
The PDF can be added either from an open buffer, a file, or a
URL.
With prefix, rebuild the cache before offering candidates."
  (interactive (list (bibtex-actions-select-keys
                      :rebuild-cache current-prefix-arg)))
  (bibtex-completion-add-pdf-to-library keys))

(defun bibtex-actions-run-default-action (keys)
  "Run the default action `bibtex-actions-default-action' on KEYS."
  (funcall bibtex-actions-default-action
           (if (stringp keys)
               (split-string keys " & ")
             (split-string (cdr keys) " & "))))

;;;###autoload
(defun bibtex-actions-dwim ()
  "Run the default action on citation keys found at point."
  (interactive)
  (if-let ((keys (cdr (bibtex-actions-citation-key-at-point))))
      (bibtex-actions-run-default-action keys)))

(provide 'bibtex-actions)
;;; bibtex-actions.el ends here
