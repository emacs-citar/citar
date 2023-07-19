;;; citar-org.el --- Org-cite support for citar -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2021-2022 Bruce D'Arcus
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;;  This is a small package that integrates citar and org-cite.  It
;;  provides a simple org-cite processor with "follow", "insert", and
;;  "activate" capabilities.

;;  Simply load this file (or its generated autoloads) and it will
;;  make the processor available to 'org-cite'.  To instruct org-cite
;;  to use citar, set one or more of the customization variables
;;  'org-cite-activate-processor', 'org-cite-follow-processor', and
;;  'org-cite-insert-processor' to the symbol 'citar.

;;; Code:

(require 'citar)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'oc)
(require 'oc-basic)
(require 'oc-csl)

(declare-function org-open-at-point "org")
(declare-function org-element-property "org-element")
(declare-function org-element-type "org-element")
(declare-function org-cite-make-insert-processor "oc")
(declare-function org-cite-get-references "oc")
(declare-function embark-act "ext:embark")
(declare-function evil-insert "ext:evil")
(declare-function org-roam-ref-add "ext:org-roam-node")
(defvar citar-open-note-function)
(defvar citar-file-open-function)
(defvar embark-target-finders)
(defvar embark-keymap-alist)
(defvar embark-pre-action-hooks)

(defface citar-org-style-preview
  ;; Not sure if this is the best parent face.
  '((t :inherit citar))
  "Face for org-cite previews."
  :group 'citar-org)

(defcustom citar-org-styles-format 'long
  "Style format; whether to use full style names or shortcuts."
  :group 'citar-org
  :type '(choice
          (const long)
          (const short)))

(defcustom citar-org-style-targets nil
  "Export processor targets to include in styles list.

If nil, use `org-cite-supported-styles'."
  :group 'citar-org
  :type '(repeat :tag "org-cite export processor" symbol))

(defcustom citar-org-activation-functions
  '(org-cite-basic-activate
    citar-org-activate-keymap)
  "List of activation functions for a citation.
Each function takes one argument, a citation."
  :group 'citar-org
  :type '(repeat function))

;;; Keymaps

(defvar citar-org-citation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") (cons "default action" #'org-open-at-point))
    (with-eval-after-load 'embark
      (define-key map (kbd "<mouse-3>") (cons "embark act" #'embark-act)))
    (define-key map (kbd "C-c C-x DEL") (cons "delete citation" #'citar-org-delete-citation))
    (define-key map (kbd "C-c C-x k") (cons "kill citation" #'citar-org-kill-citation))
    (define-key map (kbd "S-<left>") (cons "shift left" #'citar-org-shift-reference-left))
    (define-key map (kbd "S-<right>") (cons "shift right" #'citar-org-shift-reference-right))
    (define-key map (kbd "M-p") (cons "update prefix/suffix" #'citar-org-update-prefix-suffix))
    map)
  "Keymap for interacting with org citations at point.")

;; TODO maybe connvert to defcustoms. But this is not really the right approach;
;; better to just run the export processors to get the previews. But we need
;; citation context for that, or some other solution to have a citation to
;; process.

(defvar citar-org-style-preview-alist
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

(defun citar-org--style-candidates ()
  "Return a list of supported styles as completion candidates."
  (let ((styles (citar-org--flat-styles)))
    (mapcar
     (lambda (style)
       (if (and (string-match "/" style)
                (< 1 (length style)))
           (propertize style 'face 'citar)
         (propertize style 'face 'citar-highlight)))
     styles)))

(defun citar-org--flat-styles (&optional proc)
  "Return a flat list of supported styles.

With PROC list, limit to specific processor(s)."
  (let ((styles (list)))
    (dolist (style-variants (org-cite-supported-styles proc))
      (seq-let (style &rest variants) style-variants
        (let ((style-name (if (string= "nil" (car style)) "/" (car style))))
          (push style-name styles)
          (dolist (variant variants)
            (let ((fstyle
                   (concat style-name
                           (unless (string= "/" style-name) "/")
                           (cadr variant))))
              (push fstyle styles))))))
    styles))

;;; Org-cite processors

;; NOTE I may move some or all of these to a separate project

;;;###autoload
(defun citar-org-select-key (&optional multiple)
  "Return a list of keys when MULTIPLE, or else a key string."
  (if multiple
      (citar-select-refs)
    (citar-select-ref)))

;;;###autoload
(defun citar-org-insert-citation (keys &optional style)
  "Insert KEYS in org-cite format, with STYLE."
  (let ((context (org-element-context)))
    (when style
      (let ((raw-style
             (citar-org-select-style)))
        (setq style
              (if (string-equal raw-style "") raw-style
                (concat "/" raw-style)))))
    (if-let ((citation (citar-org--citation-at-point context)))
        (when-let ((keys (seq-difference keys (org-cite-get-references citation t)))
                   (keystring (mapconcat (lambda (key) (concat "@" key)) keys "; "))
                   (begin (org-element-property :contents-begin citation)))
          (if (<= (point) begin)
              (org-with-point-at begin
                (insert keystring ";"))
            (let ((refatpt (citar-org--reference-at-point)))
              (org-with-point-at (or (and refatpt (org-element-property :end refatpt))
                                     (org-element-property :contents-end citation))
                (if (char-equal ?\; (char-before))
                    (insert-before-markers keystring ";")
                  (insert-before-markers ";" keystring))))))
      (if (org-cite--allowed-p context)
          (insert
           (format "[cite%s:%s]" (or style "")
                   (mapconcat (lambda (key) (concat "@" key)) keys "; ")))
        (user-error "Cannot insert a citation here")))))

;;;###autoload
(defun citar-org-insert-edit (&optional arg)
  "Run `org-cite-insert' with citar insert processor.
ARG is used as the prefix argument."
  (let ((org-cite-insert-processor 'citar))
    (org-cite-insert arg)))

;;;###autoload
(defun citar-org-follow (_datum _arg)
  "Follow processor for org-cite."
  (call-interactively citar-at-point-function))

;;;###autoload
(defun citar-org-select-style (&optional _arg)
  "Complete a citation style for org-cite with preview."
  (let* ((oc-styles
          ;; Sort the list upfront, but let completion UI handle beyond that.
          (sort (citar-org--style-candidates) 'string-lessp))
         (style
          (completing-read
           "Styles: "
           (lambda (str pred action)
             (if (eq action 'metadata)
                 `(metadata
                   (annotation-function . citar-org--style-preview-annote)
                   (group-function . citar-org--styles-group-fn))
               (complete-with-action action oc-styles str pred)))))
         (style-final (string-trim style)))
    (if (string= style-final "/") "" style-final)))

(defun citar-org--styles-group-fn (style transform)
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

(defun citar-org--style-preview-annote (style &optional _citation)
  "Annotate STYLE with CITATION preview."
  ;; TODO rather than use the alist, run the export processors on the citation.
  (let* ((preview (or (cdr (assoc style citar-org-style-preview-alist)) ""))
         ;; TODO look at how define-face does this.
         (formatted-preview (truncate-string-to-width preview 50 nil 32)))
    (propertize formatted-preview 'face 'citar-org-style-preview)))

;;;###autoload
(defun citar-org-local-bib-files (&rest _args)
  "Return local bib file paths for org buffer."
  (seq-difference (org-cite-list-bibliography-files)
                  org-cite-global-bibliography))

;;; Org note function

(defun citar-org--id-get-create (&optional force)
  "Call `org-id-get-create' while maintaining point.

If point is at the beginning of the buffer and a new properties
drawer is created, move point after the drawer.

More generally, if `org-id-get-create' inserts text at point,
move point after the insertion.

With optional argument FORCE, force the creation of a new ID."
  (let ((point (point-marker)))
    (set-marker-insertion-type point t)
    (unwind-protect
        (org-id-get-create force)
      (goto-char point)
      (set-marker point nil))))

;;;###autoload
(defun citar-org-roam-make-preamble (key)
  "Add a preamble to org-roam note, with KEY."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-roam-buffer-p)
             (org-roam-buffer-p))
    (ignore-errors (citar-org--id-get-create))
    (ignore-errors (org-roam-ref-add (concat "@" key)))))

;;;###autoload
(defun citar-org-format-note-default (key entry)
  "Format a note from KEY and ENTRY."
  (let* ((template (citar--get-template 'note))
         (note-meta (when template
                      (citar-format--entry template entry)))
         (filepath (expand-file-name
                    (concat key ".org")
                    (car citar-notes-paths)))
         (buffer (find-file filepath)))
    (with-current-buffer buffer
      ;; This just overrides other template insertion.
      (erase-buffer)
      (citar-org-roam-make-preamble key)
      (insert "#+title: ")
      (when template (insert note-meta))
      (insert "\n\n|\n\n#+print_bibliography:")
      (search-backward "|")
      (delete-char 1)
      (when (fboundp 'evil-insert)
        (evil-insert 1)))))

;;; Embark target finder

;;;###autoload
(defun citar-org-key-at-point ()
  "Return key at point for org-cite citation-reference or property."
  (or (citar-org--key-at-point)
      (citar-org--prop-key-at-point)))

(defun citar-org--key-at-point ()
  "Return key at point for org-cite citation-reference."
  (when-let ((reference (citar-org--reference-at-point)))
    (cons (org-element-property :key reference)
          (cons (org-element-property :begin reference)
                (org-element-property :end reference)))))

(defun citar-org--prop-key-at-point ()
  "Return citekey at point, when in org property drawer.

Citkey must be formatted as `@key'."
  (when (and (equal (org-element-type (org-element-at-point)) 'node-property)
             (org-in-regexp (concat "[[:space:]]" org-element-citation-key-re)))
    (cons (substring (match-string 0) 2)
          (cons (match-beginning 0)
                (match-end 0)))))

;;;###autoload
(defun citar-org-citation-at-point ()
  "Return org-cite citation keys at point as a list for `embark'."
  (when-let ((citation (citar-org--citation-at-point)))
    (cons (org-cite-get-references citation t)
          (org-cite-boundaries citation))))

;;; Functions for editing/modifying citations

(defun citar-org--reference-at-point (&optional context)
  "Return citation-reference org-element at point, if any.

Argument CONTEXT is an org element at point, usually a citation
or citation-reference."
  (when-let ((context (or context (org-element-context))))
    (when (eq 'citation-reference (org-element-type context))
      context)))

(defun citar-org--citation-at-point (&optional context)
  "Return citation element containing point, if any.

Argument CONTEXT is an org element at point, usually a citation
or citation-reference."
  (let ((element (or context (org-element-context))))
    (while (and element (not (eq 'citation (org-element-type element))))
      (setq element (org-element-property :parent element)))
    (when-let ((bounds (and element (org-cite-boundaries element))))
      (when (and (>= (point) (car bounds))
                 (<= (point) (cdr bounds)))
        element))))

(defun citar-org-list-keys ()
  "List citation keys in the org buffer."
  (let ((org-tree (org-element-parse-buffer)))
    (delete-dups
     (org-element-map org-tree 'citation-reference
       (lambda (r) (org-element-property :key r))
       org-tree))))

;; most of this section is adapted from org-ref-cite

(defun citar-org-activate-keymap (citation)
  "Activation function for CITATION to add keymap and tooltip."
  (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
    ;; Put the keymap on a citation
    (put-text-property beg end 'keymap citar-org-citation-map)))

(defun citar-org--get-ref-index (refs ref)
  "Return index of citation-reference REF within REFS."
  (seq-position refs ref
                (lambda (r1 r2)
                  (and (equal (org-element-property :begin r1)
                              (org-element-property :begin r2))))))

(defun citar-org-delete-citation ()
  "Delete the citation or citation-reference at point."
  (interactive)
  (org-cite-delete-citation (org-element-context)))

(defun citar-org-kill-citation ()
  "Kill (copy) the citation or citation-reference at point."
  (interactive)
  (let* ((datum (org-element-context)))
    (kill-region (org-element-property :begin datum) (org-element-property :end datum))))

(defun citar-org-cite-swap (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)

(defun citar-org--shift-reference (datum direction)
  "When point is on a citation-reference DATUM, shift it in DIRECTION."
  (let*  ((current-citation (if (eq 'citation (org-element-type datum)) datum
                              (org-element-property :parent datum)))
          (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
          (point-offset
           (- (point) (org-element-property :begin current-ref)))
          (refs (org-cite-get-references current-citation))
          (index
           (citar-org--get-ref-index refs current-ref)))

    (when (= 1 (length refs))
      (error "You only have one reference; you cannot shift this"))
    ;; TODO remove this when the shifting is updated to move to front or end of the list.
    (when (or (and (equal index 0)
                   (equal direction 'left))
              (and (equal (+ 1 index) (length refs))
                   (equal direction 'right)))
      (error "You cannot shift the reference in this direction"))
    (when (null index)
      (error "Nothing to shift here"))
    (let*
        ((v1
          (org-element-property :contents-begin current-citation))
         (v2
          (org-element-property :contents-end current-citation))
         (new-index
          (if (eq 'left direction) (- index 1) (+ index 1))))
      (cl--set-buffer-substring v1 v2
                                (org-element-interpret-data
                                 (citar-org-cite-swap index new-index refs)))
      ;; Now move point to the original ref.
      (goto-char (+ (org-element-property :begin (nth new-index
                                                      (org-cite-get-references current-citation)))
                    point-offset)))))

(defun citar-org-shift-reference-left ()
  "When point is on a citation-reference, shift it left."
  (interactive)
  (let ((datum (org-element-context)))
    (citar-org--shift-reference datum 'left)))

(defun citar-org-shift-reference-right ()
  "When point is on a citation-reference, shift it right."
  (interactive)
  (let ((datum (org-element-context)))
    (citar-org--shift-reference datum 'right)))

(defun citar-org--update-prefix-suffix (datum)
  "Change the prefix and suffix text of the DATUM at point.
DATUM should be a reference, otherwise throw an error."
  (let* ((ref-p (eq 'citation-reference (org-element-type datum)))
         (ref (if ref-p datum (error "Not on a reference")))
         (key (org-element-property :key ref))
         (citekey-str (propertize key 'face 'mode-line-emphasis))
         (pre (org-element-interpret-data (org-element-property :prefix ref)))
         (post (org-element-interpret-data (org-element-property :suffix ref)))
         ;; TODO Unsure if we want to process prefix at all
         (prefix (read-string (concat "Prefix for " citekey-str ": ")
                              (string-trim pre)))
         (suffix (string-trim-left        ; Remove leading whitespace
                  (read-string (concat "Suffix for " citekey-str ": ")
                               (string-trim post))))
         ;; Change suffix to have one space prior to the user-inputted suffix, unless suffix is already empty
         ;; or just whitespace
         (suffix-processed
          (concat (unless (string-empty-p suffix) " ") suffix))
         (v1
          (org-element-property :begin ref))
         (v2
          (org-element-property :end ref)))
    (cl--set-buffer-substring v1 v2
                              (org-element-interpret-data
                               `(citation-reference
                                 (:key ,key :prefix ,prefix :suffix ,suffix-processed))))))

(defun citar-org-update-prefix-suffix (&optional arg)
  "Change the prefix and suffix text of the reference at point.
If given ARG, change the prefix and suffix for every reference in
the citation at point.

If point is not on a reference or citation, throw an error."
  (interactive "P")
  (let* ((datum (org-element-context))
         (citation-p (eq 'citation (org-element-type datum)))
         (ref-p (eq 'citation-reference (org-element-type datum)))
         (current-citation (cond
                            (citation-p datum)
                            (ref-p (org-element-property :parent datum))
                            (t (error "Not on a citation or reference"))))
         (refs (org-cite-get-references current-citation)))
    (save-excursion
      (if (or arg citation-p)
          ;; We use dotimes over dolist because the buffer changes as we iterate through the list, meaning we
          ;; cannot simply use the initial value of refs all throughout
          (dotimes (ref-index (length refs))
            (citar-org--update-prefix-suffix (nth ref-index refs))
            ;; Update refs since the begins and ends for the following references could have changed when
            ;; adding a prefix and/or suffix
            (setq refs (org-cite-get-references
                        (if citation-p
                            (org-element-context)
                          (org-element-property :parent (org-element-context))))))
        (citar-org--update-prefix-suffix (org-element-context))))))

;; Load this last.

;;;###autoload
(defun citar-org-activate (citation)
  "Run all the activation functions in `citar-org-activation-functions'.
Argument CITATION is an org-element holding the references."
  (dolist (activate-func citar-org-activation-functions)
    (funcall activate-func citation)))

;;;###autoload
(with-eval-after-load 'oc
  (org-cite-register-processor 'citar
    :insert (org-cite-make-insert-processor
             #'citar-org-select-key
             #'citar-org-select-style)
    :follow #'citar-org-follow
    :activate #'citar-org-activate))

(provide 'citar-org)
;;; citar-org.el ends here
