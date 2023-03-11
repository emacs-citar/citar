;;; citar-format.el --- Formatting functions for citar -*- lexical-binding: t; -*-
;;
;; SPDX-FileCopyrightText: 2021-2022 Bruce D'Arcus, Roshan Shariff
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;;  Functions for formatting bibliography entries.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defvar citar-display-transform-functions)
(declare-function citar-get-display-value "citar")


;;; Formatting bibliography entries


(cl-defun citar-format--entry (format entry &optional width
                                      &key hide-elided ellipsis)
  "Format ENTRY according to FORMAT.
FORMAT may be either a format string or a parsed format string as
returned by `citar-format--parse'."
  (let* ((fieldspecs (if (stringp format) (citar-format--parse format) format))
         (preform (citar-format--preformat fieldspecs entry
                                           hide-elided ellipsis)))
    (if width
        (citar-format--star-widths (- width (car preform)) (cdr preform)
                                   hide-elided ellipsis)
      (apply #'concat (cdr preform)))))


;;; Pre-formatting bibliography entries


(defun citar-format--preformat (fieldspecs entry hide-elided ellipsis)
  "Pre-format ENTRY using parsed format string FIELDSPECS.
FIELDSPECS should be the result of `citar-format--parse'. See the
documentation of `citar-format--string' for the meaning of
HIDE-ELIDED and ELLIPSIS."
  (let ((preformatted nil)
        (fields "")
        (width 0))
    (dolist (fieldspec fieldspecs)
      (pcase fieldspec
        ((pred stringp)
         (cl-callf concat fields fieldspec)
         (cl-incf width (string-width fieldspec)))
        (`(,props . ,fieldnames)
         (let* ((fieldwidth (plist-get props :width))
                (textprops (plist-get props :text-properties))
                (transform (plist-get props :transform))
                (value (citar-get-display-value fieldnames entry transform))
                (display (citar-format--string value
                                               :width fieldwidth
                                               :text-properties textprops
                                               :hide-elided hide-elided
                                               :ellipsis ellipsis)))
           (cond
            ((eq '* fieldwidth)
             (push fields preformatted)
             (setq fields "")
             (push display preformatted))
            (t
             (cl-callf concat fields display)
             (cl-incf width (if (numberp fieldwidth)
                                fieldwidth
                              (string-width value)))))))))
    (unless (string-empty-p fields)
      (push fields preformatted))
    (cons width (nreverse preformatted))))


;;; Internal implementation functions


(cl-defsubst citar-format--string (string
                                   &key width text-properties hide-elided ellipsis)
  "Truncate STRING to WIDTH and apply TEXT-PROPERTIES.
If HIDE-ELIDED is non-nil, the truncated part of STRING is
covered by a display property that makes it invisible, instead of
being deleted.  ELLIPSIS, when non-nil, specifies a string to
display instead of the truncated part of the text."
  (when text-properties
    (setq string (apply #'propertize string text-properties)))
  (when (numberp width)
    (setq string (truncate-string-to-width string width 0 ?\s ellipsis hide-elided)))
  string)


(defun citar-format--star-widths (alloc strings &optional hide-elided ellipsis)
  "Concatenate STRINGS and truncate every other element to fit in ALLOC.
Use this function along with `citar-format--preformat' to fit a
formatted string to a desired display width; see
`citar-format--entry' for how to do this.

Return a string consisting of the concatenated elements of
STRINGS.  The odd-numbered elements are included as-is, while the
even-numbered elements are padded or truncated to a total width
of ALLOC, which must be an integer.  All these odd-numbered
elements are allocated close-to-equal widths.

Perform the truncation using `citar-format--string', which see
for the meaning of HIDE-ELIDED and ELLIPSIS."
  (let ((nstars (/ (length strings) 2)))
    (if (= 0 nstars)
        (or (car strings) "")
      (cl-loop
       with alloc = (max 0 alloc)
       with starwidth = (/ alloc nstars)
       with remainder = (% alloc nstars)
       with formatted = (car strings)
       for (starstring following) on (cdr strings) by #'cddr
       for nthstar from 1
       do (let* ((starwidth (if (> nthstar remainder) starwidth
                              (1+ starwidth)))
                 (starstring (citar-format--string
                              starstring
                              :width starwidth
                              :hide-elided hide-elided :ellipsis ellipsis)))
            (cl-callf concat formatted starstring following))
       finally return formatted))))


;;; Parsing format strings

(defun citar-format--get-transform (key)
  "Return transform spec for KEY."
  (cdr (assoc key citar-display-transform-functions)))

(defun citar-format--parse (format-string)
  "Parse FORMAT-STRING."
  (let ((regex (concat "\\${"                ; ${
                       "\\(.*?\\)"           ; field names
                       "\\(?::[[:blank:]]*"  ; : + space
                       "\\(.*?\\)"           ; format spec
                       "[[:blank:]]*\\)?}")) ; space + }
        (position 0)
        (fieldspecs nil))
    (while (string-match regex format-string position)
      (let* ((begin (match-beginning 0))
             (end (match-end 0))
             (textprops (text-properties-at begin format-string))
             (fieldnames (match-string-no-properties 1 format-string))
             (spec (match-string-no-properties 2 format-string))
             (transform
              (let ((tsym
                     (when spec
                       (cadr (split-string spec "%")))))
                (when tsym
                  (citar-format--get-transform (intern tsym)))))
             (width (cond
                     ((or (null spec) (string-empty-p spec)
                          (= 0 (string-to-number spec))) nil)
                     ((string-equal spec "*") '*)
                     (t (string-to-number spec)))))
        (when (< position begin)
          (push (substring format-string position begin) fieldspecs))
        (push (cons (nconc (when width `(:width ,width))
                           (when textprops `(:text-properties ,textprops))
                           (when transform `(:transform ,transform)))
                    (split-string-and-unquote fieldnames))
              fieldspecs)
        (setq position end)))
    (when (< position (length format-string))
      (push (substring format-string position) fieldspecs))
    (nreverse fieldspecs)))


(provide 'citar-format)
;;; citar-format.el ends here
