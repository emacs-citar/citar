;;; citar-format.el --- Formatting functions for citar -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus, Roshan Shariff
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
;;  Functions for formatting bibliography entries.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(declare-function citar--display-value "citar")
(defvar citar-ellipsis)


;;; Formatting bibliography entries


(cl-defun citar-format--entry (format-string entry &key width hide-elided
                                             (ellipsis citar-ellipsis))
  "Format ENTRY according to FORMAT-STRING."
  (cl-flet ((getwidth (fieldspec)
              (unless (stringp fieldspec)
                (plist-get (car fieldspec) :width)))
            (fmtfield (fieldspec)
              (citar-format--fieldspec fieldspec entry
                                       :hide-elided hide-elided
                                       :ellipsis ellipsis)))
    (let* ((fieldspecs (citar-format--parse format-string))
           (widths (mapcar #'getwidth fieldspecs))
           (strings (mapcar #'fmtfield fieldspecs)))
      (citar-format--star-widths widths strings :width width
                                 :hide-elided hide-elided :ellipsis ellipsis))))


;;; Pre-formatting bibliography entries


(cl-defun citar-format--preformat (format-string &key hide-elided
                                                 (ellipsis citar-ellipsis))
  "Preformat according to FORMAT-STRING.
See `citar-format--string for the meaning of HIDE-ELIDED and ELLIPSIS."
  (let ((fieldgroups (citar-format--preformat-parse format-string)))
    (lambda (entry)
      (cl-flet ((fmtfield (fieldspec)
                  (citar-format--fieldspec fieldspec entry
                                           :hide-elided hide-elided
                                           :ellipsis ellipsis)))
        (mapcar (lambda (groupspec)
                  (mapconcat #'fmtfield (cdr groupspec) ""))
                fieldgroups)))))


(cl-defun citar-format--preformatted (format-string &key width hide-elided
                                                    (ellipsis citar-ellipsis))
  "Fit pre-formatted strings to WIDTH according to FORMAT-STRING.
See `citar-format--string for the meaning of HIDE-ELIDED and ELLIPSIS."
  (let* ((fieldgroups (citar-format--preformat-parse format-string))
         (widths (mapcar (lambda (groupspec)
                           (plist-get (car groupspec) :width))
                         fieldgroups)))
    (lambda (preformatted)
      (citar-format--star-widths widths preformatted :width width
                                 :hide-elided hide-elided :ellipsis ellipsis))))


;;; Internal implementation functions


(cl-defun citar-format--fieldspec (fieldspec entry &key hide-elided ellipsis)
  "Format FIELDSPEC using information from ENTRY.
See `citar-format--string` for the meaning of HIDE-ELIDED and ELLIPSIS."
  (if (stringp fieldspec)
      fieldspec
    (let* ((fmtprops (car fieldspec))
           (fieldnames (cdr fieldspec))
           (displaystr (citar--display-value fieldnames entry)))
      (apply #'citar-format--string displaystr
             :hide-elided hide-elided :ellipsis ellipsis
             fmtprops))))


(cl-defun citar-format--string (string
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


(cl-defun citar-format--star-widths (widths strings &key width
                                            hide-elided ellipsis)
  "Format STRINGS according to WIDTHS to fit WIDTH."
  (if (not (and (numberp width) (cl-find '* widths)))
      ;; If width is unlimited or there are no *-width fields, just join strings.
      ;; We only support truncating *-width fields.
      (string-join strings)
    ;; Otherwise, calculate extra space available for *-width fields
    (let ((usedwidth 0) (nstars 0))
      ;; For fields without width spec, add their actual width to usedwidth
      (cl-mapc
       (lambda (width string)
         (cond ((eq '* width) (cl-incf nstars))
               ((numberp width) (cl-incf usedwidth width))
               ((null width) (cl-incf usedwidth (string-width string)))))
       widths strings)
      (let* ((extrawidth (max 0 (- width usedwidth)))
             (starwidth (/ extrawidth nstars))
             (remainder (% extrawidth nstars))
             (starindex 0))
        (string-join
         (cl-mapcar
          (lambda (width string)
            (if (not (eq width '*))
                string
              (cl-incf starindex)
              (citar-format--string
               string
               :width (+ starwidth (if (<= starindex remainder) 1 0))
               :hide-elided hide-elided :ellipsis ellipsis)))
          widths strings))))))


;;; Parsing format strings


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
             (width (cond
                     ((or (null spec) (string-empty-p spec)) nil)
                     ((string-equal spec "*") '*)
                     (t (string-to-number spec)))))
        (when (< position begin)
          (push (substring format-string position begin) fieldspecs))
        (push (cons (nconc (when width `(:width ,width))
                           (when textprops `(:text-properties ,textprops)))
                    (split-string-and-unquote fieldnames))
              fieldspecs)
        (setq position end)))
    (when (< position (length format-string))
      (push (substring format-string position) fieldspecs))
    (nreverse fieldspecs)))


(defun citar-format--preformat-parse (format-string)
  "Parse and group FORMAT-STRING."
  (let (fieldgroups group (groupwidth 0))
    (cl-flet ((newgroup ()
                (when group
                  (push (cons (when groupwidth `(:width ,groupwidth))
                              (nreverse group))
                        fieldgroups)
                  (setq group nil))
                (setq groupwidth 0)))
      (dolist (fieldspec (citar-format--parse format-string))
        (let ((fieldwidth (cond
                           ((stringp fieldspec) (string-width fieldspec))
                           ((listp fieldspec) (plist-get (car fieldspec) :width)))))
          (cond
           ((eq fieldwidth '*)
            ;; *-width field; start a new group
            (newgroup)
            ;; Pre-format the field at unlimited width by setting :width to nil
            (cl-callf plist-put (car fieldspec) :width nil)
            ;; Add the field in its own pre-format group with :width *
            (push fieldspec group)
            (setq groupwidth '*)
            (newgroup))
           ((numberp fieldwidth)
            ;; Fixed-length field; start new group if needed
            (unless (numberp groupwidth)
              (newgroup))
            ;; Add field to group and increment group width
            (push fieldspec group)
            (cl-incf groupwidth fieldwidth))
           (t
            ;; Unknown-length field; start new group if needed
            (unless (null groupwidth)
              (newgroup))
            ;; Add field to group; group width is now unknown
            (push fieldspec group)
            (setq groupwidth nil)))))
      ;; Add any remaining fields to group
      (newgroup))
    (nreverse fieldgroups)))


(provide 'citar-format)
;;; citar-format.el ends here
