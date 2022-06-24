;;; citar-file-test.el --- Tests for citar-file.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'seq)
(require 'citar-file)

(ert-deftest citar-format-test--parsing ()

  ;; Test the default parser, which splits strings by both : and ;
  (should (equal '("") (delete-dups (citar-file--parser-default " "))))
  (should (equal '("foo") (delete-dups (citar-file--parser-default "foo"))))
  (should (equal '("foo" "bar" "foo;bar") (delete-dups (citar-file--parser-default "foo;bar"))))
  (should (equal '("foo" "bar" "foo ; bar") (delete-dups (citar-file--parser-default " foo ; bar "))))
  (should (equal '("foo : bar" "foo" "bar") (delete-dups (citar-file--parser-default " foo : bar "))))
  (should (equal '("foo:bar" "baz" "foo" "bar;baz") (delete-dups (citar-file--parser-default "foo:bar;baz"))))

  ;; Test escaped delimiters
  (should (equal '("foo\\;bar") (delete-dups (citar-file--parser-default "foo\\;bar"))))
  (should (equal '("foo" "bar\\" "foo;bar\\") (delete-dups (citar-file--parser-default "foo;bar\\"))))
  (should (equal '("foo\\;bar" "baz" "foo\\;bar;baz")
                 (delete-dups (citar-file--parser-default "foo\\;bar;baz"))))

  ;; Test triplet parser
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

(provide 'citar-file-test)
;;; citar-file-test.el ends here
