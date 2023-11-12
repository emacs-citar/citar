;;; citar-format-test.el --- Tests for citar-format.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'citar-format)

(ert-deftest citar-format-test--star-widths ()
  "Test `citar-format--star-widths'."

  (should (string-empty-p (citar-format--star-widths 80 nil)))

  ;; For single string, return the original string; not a copy
  (let ((strings '("foo")))
    (should (eq (car strings) (citar-format--star-widths 80 strings))))

  (let ((strings '("foo" "bar" "baz")))
    (should (equal "foobaz" (citar-format--star-widths 0 strings)))
    (should (equal "foobabaz" (citar-format--star-widths 2 strings)))
    (should (equal "foob…baz" (citar-format--star-widths 2 strings nil "…")))
    (should (equal "foobarbaz" (citar-format--star-widths 3 strings)))
    (should (equal "foobar baz" (citar-format--star-widths 4 strings)))

    ;; When hide-elided is t, the actual string contents should be equal
    (cl-loop for w from 0 to 3
             do (should (equal "foobarbaz" (citar-format--star-widths w strings t))))
    ;; ...unless the allocated width is greater than the string length
    (should (equal "foobar baz" (citar-format--star-widths 4 strings)))

    ;; When hide-elided is t, the hidden text should have the 'display ""
    ;; property. N.B. equal-including-properties is slightly broken; see
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=6581
    (should (ert-equal-including-properties #("foobarbaz" 5 6 (display ""))
                                            (citar-format--star-widths 2 strings t)))

    ;; Test with ellipsis
    (should (ert-equal-including-properties #("foobarbaz" 4 6 (display "…"))
                                            (citar-format--star-widths 2 strings t "…"))))

  (let ((strings '("foo" "bar" "baz" "qux")))
    (should (equal "foobaz" (citar-format--star-widths 0 strings)))
    (should (equal "foobbaz" (citar-format--star-widths 1 strings)))
    (should (equal "foobbazq" (citar-format--star-widths 2 strings)))
    (should (equal "foobabazq" (citar-format--star-widths 3 strings)))
    (should (equal "foobabazqu" (citar-format--star-widths 4 strings)))

    ;; Test with ellipsis
    (should (equal "foob…baz…" (citar-format--star-widths 3 strings nil "…")))
    (should (ert-equal-including-properties
             #("foobarbazqux" 4 6 (display "…") 9 12 (display "…"))
             (citar-format--star-widths 3 strings t "…"))))

  (should (equal '((nil "author" "editor") " " (nil "year"))
                 (citar-format--parse "${author editor} ${year}")))
  (should (equal '((nil "author" "editor") " " ((:width 4) "year"))
                 (citar-format--parse "${author editor} ${year:4}")))
  (should (equal '(((:width *) "author" "editor") " " ((:width 4) "year"))
                 (citar-format--parse "${author editor:*} ${year:4}")))
  (should (equal '(((:width * :transform (citar--shorten-names)) "author" "editor") " " ((:width 4) "year"))
                 (citar-format--parse "${author editor: * % sn} ${year:4}"))))

(provide 'citar-format-test)
;;; citar-format-test.el ends here
