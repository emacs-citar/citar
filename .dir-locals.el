
((emacs-lisp-mode
  (fill-column . 110)
  (indent-tabs-mode . nil)
  (elisp-lint-ignored-validators . ("byte-compile" "package-lint"))
  (elisp-lint-indent-specs . ((describe . 1)
                              (it . 1)
                              (thread-first . 0)
                              (cl-flet . 1)
                              (sentence-end-double-space . nil)
                              (cl-flet* . 1)))))
