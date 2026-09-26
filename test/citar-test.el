;;; citar-file-test.el --- Tests for citar.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'citar)
(require 'map)

(ert-deftest citar-test--check-notes-sources ()
  ;; This should run without signalling an error
  (should-not (ignore (map-do #'citar--check-notes-source citar-notes-sources))))

(ert-deftest citar-test--bibliography-files-global-only ()
  "Existing global bibliography is returned."
  (let ((tmp (make-temp-file "citar-test-global" nil ".bib")))
    (unwind-protect
         (let ((citar-bibliography (list tmp))
               (citar-major-mode-functions
                '((t . ((local-bib-files . ignore))))))
           (should (member (file-truename tmp)
                           (citar--bibliography-files))))
      (delete-file tmp))))

(ert-deftest citar-test--bibliography-files-missing-global-errors ()
  "Missing global bibliography signals a `user-error'."
  (let ((citar-bibliography '("/tmp/citar-test-nonexistent-glob.bib"))
        (citar-major-mode-functions
         '((t . ((local-bib-files . ignore))))))
    (should-error (citar--bibliography-files) :type 'user-error)))

(ert-deftest citar-test--bibliography-files-missing-local-filtered ()
  "Missing local bibliography is silently filtered."
  (let ((tmp (make-temp-file "citar-test-glob" nil ".bib"))
        (missing "/tmp/citar-test-nonexistent-local.bib"))
    (unwind-protect
         (let* ((citar-bibliography (list tmp))
                (citar-major-mode-functions
                 `((t . ((local-bib-files . ,(lambda () (list missing)))))))
                (result (citar--bibliography-files)))
           (should (member (file-truename tmp) result))
           (should-not (member missing result))
           (should-not (member (file-truename missing) result)))
      (delete-file tmp))))

(ert-deftest citar-test--bibliography-files-existing-local-included ()
  "Existing local bibliography is included."
  (let ((tmp-glob (make-temp-file "citar-test-glob" nil ".bib"))
        (tmp-local (make-temp-file "citar-test-local" nil ".bib")))
    (unwind-protect
         (let* ((citar-bibliography (list tmp-glob))
                (citar-major-mode-functions
                 `((t . ((local-bib-files . ,(lambda () (list tmp-local)))))))
                (result (citar--bibliography-files)))
           (should (member (file-truename tmp-glob) result))
           (should (member (file-truename tmp-local) result)))
      (delete-file tmp-glob)
      (delete-file tmp-local))))

(provide 'citar-test)
;;; citar-test.el ends here
