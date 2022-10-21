;;; citar-file-test.el --- Tests for citar-file.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'seq)
(require 'citar)

(ert-deftest citar-file-test--parser-default ()

  (should-not (citar-file--parser-default " "))
  (should (equal '("foo") (delete-dups (citar-file--parser-default "foo"))))
  (should (equal '("foo" "bar") (delete-dups (citar-file--parser-default "foo;bar"))))
  (should (equal '("foo" "bar") (delete-dups (citar-file--parser-default " foo ; bar ; "))))
  (should (equal '("foo:bar" "baz") (delete-dups (citar-file--parser-default "foo:bar;baz"))))
  (should (equal '("foo:bar" "baz") (delete-dups (citar-file--parser-default "foo:bar;;baz"))))

  ;; Test escaped delimiters
  (should (equal '("foo;bar" "foo\\;bar") (delete-dups (citar-file--parser-default "foo\\;bar"))))
  (should (equal '("foo" "bar\\") (delete-dups (citar-file--parser-default "foo;bar\\"))))
  (should (equal '("foo;bar" "foo\\;bar" "baz")
                 (delete-dups (citar-file--parser-default "foo\\;bar;baz")))))

(ert-deftest citar-file-test--parser-triplet ()

  (should-not (citar-file--parser-triplet "foo.pdf"))

  (should (equal '("foo.pdf") (delete-dups (citar-file--parser-triplet ":foo.pdf:PDF"))))
  (should (equal '("foo.pdf:PDF,:bar.pdf" "foo.pdf" "bar.pdf")
                 (delete-dups (citar-file--parser-triplet ":foo.pdf:PDF,:bar.pdf:PDF"))))

  ;; Don't trim spaces in triplet parser since file is delimited by :
  (should (equal '(" foo.pdf :PDF, : bar.pdf " " foo.pdf " " bar.pdf ")
                 (delete-dups (citar-file--parser-triplet ": foo.pdf :PDF, : bar.pdf :PDF"))))

  ;; Test escaped delimiters
  (should (equal '("title.pdf")
                 (delete-dups (citar-file--parser-triplet "Title\\: Subtitle:title.pdf:application/pdf"))))
  (should (equal '("C:\\title.pdf" "C\\:\\\\title.pdf")
                 (delete-dups (citar-file--parser-triplet "Title\\: Subtitle:C\\:\\\\title.pdf:PDF"))))

  ;; Calibre doesn't escape any special characters in filenames, so try that
  (should (equal '("C:title.pdf" "C:\\title.pdf")
                 (delete-dups (citar-file--parser-triplet "Title\\: Subtitle:C:\\title.pdf:PDF")))))

(ert-deftest citar-file-test--parse-file-field ()

  (let* ((citar-file-variable "file")
         (citekey "foo")
         (fieldvalue "foo.pdf")
         (dirs '("/home/user/library/"))
         (citar-file-parser-functions (list #'citar-file--parser-default))
         lastmessage)

    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq lastmessage (apply #'format-message format-string args))))
              ((symbol-function 'current-message)
               (lambda ()
                 (prog1 lastmessage (setq lastmessage nil))))
              ;; Pretend that all .pdf files under /home/user/library/ exist:
              ((symbol-function 'file-exists-p)
               (lambda (filename)
                 (and (equal "pdf" (file-name-extension filename))
                      (member (file-name-directory filename) dirs)))))

      (should-not (citar-file--parse-file-field " " dirs citekey))
      (should (string=
               (current-message)
               (format-message "Empty `%s' field: %s" citar-file-variable citekey)))

      (let ((citar-file-parser-functions nil))
        (should-not (citar-file--parse-file-field fieldvalue dirs citekey))
        (should (string=
                 (current-message)
                 (format-message
                  "Could not parse `%s' field of `%s'; check `citar-file-parser-functions': %s"
                  citar-file-variable citekey fieldvalue))))

      (should-not (citar-file--parse-file-field "foo.html" dirs citekey))
      (should (string=
               (current-message)
               (format-message
                (concat "None of the files for `%s' exist; check `citar-library-paths' and "
                        "`citar-file-parser-functions': %S")
                citekey '("foo.html"))))

      (let ((citar-library-file-extensions '("html")))
        (should-not (citar-file--parse-file-field fieldvalue dirs citekey))
        (should (string=
                 (current-message)
                 (format-message
                  "No files for `%s' with `citar-library-file-extensions': %S"
                  citekey '("/home/user/library/foo.pdf")))))

      (let ((citar-library-file-extensions nil))
        (should (equal (citar-file--parse-file-field fieldvalue dirs citekey)
                        '("/home/user/library/foo.pdf"))))

      (let ((citar-library-file-extensions '("pdf" "html")))
        (should (equal (citar-file--parse-file-field fieldvalue dirs citekey)
                        '("/home/user/library/foo.pdf")))))))

(provide 'citar-file-test)
;;; citar-file-test.el ends here
