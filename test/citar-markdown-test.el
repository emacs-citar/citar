;;; citar-markdown-test.el --- Tests for citar-markdown.el  -*- lexical-binding: t; -*-

;;; Commentary: Specification for markdown citations is from pandoc
;;; https://pandoc.org/MANUAL.html#citation-syntax and
;;; https://github.com/jgm/pandoc/blob/main/src/Text/Pandoc/Parsing/Citations.hs
;;; 
;;; See also pandoc's test-cases
;;; https://github.com/jgm/pandoc/blob/main/test/markdown-citations.txt

;;; Code:


(require 'ert)
(require 'citar)

(ert-deftest citar-markdown-test-citation-key-format ()
  (should (equal '("alpha1234")
           (with-temp-buffer
             (insert "  @alpha1234  ")
             (citar-markdown-list-keys))))

  (should (equal '("_startwithunderscore")
           (with-temp-buffer
             (insert "  @_startwithunderscore  ")
             (citar-markdown-list-keys))))

  (should (equal '("contain_0000_underscore")
           (with-temp-buffer
             (insert "  @contain_0000_underscore  ")
             (citar-markdown-list-keys))))
  
  (should (equal '("no_trail_fullstop")
           (with-temp-buffer
             (insert "  @no_trail_fullstop. ")
             (citar-markdown-list-keys))))
    
  (should (equal '("contain~nonalpha")
           (with-temp-buffer
             (insert "  @contain~nonalpha ")
             (citar-markdown-list-keys))))

  (should (equal '("nocontain")
           (with-temp-buffer
             (insert "  @nocontain--nonalpha ")
             (citar-markdown-list-keys))))

  (should (equal '("brace###delim")
           (with-temp-buffer
             (insert "  @{brace###delim} ")
             (citar-markdown-list-keys))))
  )
  
