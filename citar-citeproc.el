;;; citar-citeproc.el --- Citeproc reference support for citar -*- lexical-binding: t; -*-
;;
;; SPDX-FileCopyrightText: 2021-2022 Bruce D'Arcus
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;  Provides functions for formatting bibliographic references according to
;;  Citation Style Language (CSL) styles, using 'citeproc-el'. For
;;  information on using CSL, see <http://www.citationstyles.org>.

;;  Before using, be sure to set the two required directory paths,
;;  'citar-citeproc-csl-styles-dir' and 'citar-citeproc-csl-locales-dir'. The
;;  'styles-dir' should contain CSL style files corresponding to any citation
;;  style you plan to use, for example, "chicago-author-date.csl" for Chicago
;;  Manual of Style 17th edition (author-date) or "ieee.csl" for IEEE
;;  citation style. CSL style files can be found in the official CSL styles
;;  repository, <https://github.com/citation-style-language/styles>. The
;;  'locales-dir' is a directory of files that are used to facilitate
;;  localizing citations and bibliographies that are generated with CSL
;;  styles. The simplest option here is to set this to a clone of the
;;  official CSL locales repository,
;;  <https://github.com/citation-style-language/locales>.

;;  After setting the style and locale directory paths, set the variable
;;  'citar-format-reference-function' to 'citar-citeproc-format-reference'.

;;  Finally, set a CSL style, either by setting 'citar-citeproc-csl-style'
;;  manually to the path to the desired CSL style file or by calling
;;  'citar-citeproc-select-csl-style' to choose from a style file located in
;;  'citar-citeproc-csl-styles-dir'. If a CSL style is not set before running
;;  'citar-citeproc-format-reference', the user is prompted to set a style. A
;;  CSL style can also be set by calling 'citar-insert-reference' or
;;  'citar-copy-reference' with a prefix-argument.

;;  Once these settings are in place, call either 'citar-insert-reference' or
;;  'citar-copy-reference' and select the key or keys to be rendered in the
;;  selected CSL style.

;;; Code:
(require 'xml)
(require 'citar)
(require 'citeproc)

(defcustom citar-citeproc-csl-styles-dir nil
  "Path to CSL style directory."
  :group 'citar
  :type 'directory)

(defcustom citar-citeproc-csl-locales-dir nil
  "Path to CSL locales dir."
  :group 'citar
  :type 'directory)

(defvar citar-citeproc-csl-style nil
  "CSL style file to be used with `citar-citeproc-format-reference'.

If file is located in the directory set to
`citar-citeproc-csl-styles-dir', only the filename itself is
necessary, e.g., \"chicago-author-date.csl\". Full path is also
accepted.")

(defun citar-citeproc-csl-metadata (file)
  "Return metadata value from csl FILE."
  (let* ((parse-tree (xml-parse-file file))
         (style-node (assq 'style parse-tree))
         (info (car (xml-get-children style-node 'info)))
         (title (caddr (car (xml-get-children info 'title)))))
    title))

;;;###autoload
(defun citar-citeproc-select-csl-style ()
  "Select CSL style to be used with `citar-citeproc-format-reference'."
  (interactive)
  (unless citar-citeproc-csl-styles-dir
    (error "Be sure to set 'citar-citeproc-csl-styles-dir' to your CSL styles directory"))
  (let* ((files (directory-files citar-citeproc-csl-styles-dir t "csl"))
         (list (mapcar
                (lambda (file)
                  (cons (citar-citeproc-csl-metadata file) (file-name-nondirectory file)))
                files))
         (style (completing-read "Select CSL style file: " list nil t))
         (file (cdr (assoc style list))))
    (setq citar-citeproc-csl-style file)))

;;;###autoload
(defun citar-citeproc-format-reference (keys)
  "Return formatted reference(s) for KEYS via `citeproc-el'.
Formatting follows CSL style set in `citar-citeproc-csl-style'.
With prefix-argument, select CSL style."
  (when (or (eq citar-citeproc-csl-style nil)
            current-prefix-arg)
    (citar-citeproc-select-csl-style))
  (unless citar-citeproc-csl-locales-dir
    (error "Be sure to set 'citar-citeproc-csl-locales-dir' to your CSL locales directory"))
  (let* ((style (if (string-match-p "/" citar-citeproc-csl-style)
                    citar-citeproc-csl-style
                  (expand-file-name citar-citeproc-csl-style citar-citeproc-csl-styles-dir)))
         (bibs (citar--bibliography-files))
         (proc (citeproc-create style
                                (citeproc-hash-itemgetter-from-any bibs)
                                (citeproc-locale-getter-from-dir citar-citeproc-csl-locales-dir)
                                "en-US"))
         (references (car (progn
                            (citeproc-add-uncited keys proc)
                            (citeproc-render-bib proc 'plain)))))
    references))

(provide 'citar-citeproc)
;;; citar-citeproc.el ends here
