;;; citar-file-test.el --- Tests for citar.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'citar)
(require 'map)

(ert-deftest citar-test--check-notes-sources ()
  ;; This should run without signalling an error
  (should-not (ignore (map-do #'citar--check-notes-source citar-notes-sources))))

(provide 'citar-test)
;;; citar-test.el ends here
