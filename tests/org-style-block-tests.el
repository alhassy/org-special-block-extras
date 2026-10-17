;;; org-style-block-tests.el --- Tests for the style block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-export-test "a style block's CSS is a <style> element, taken verbatim" [style]
  :input "
          #+begin_style
          h2 { color: teal; }
          #+end_style
          Prose."
  :equals "<style>
           h2 { color: teal; }
           </style>
           <p>
           Prose.</p>
           ")

(define-org-export-test "a style block exports to nothing in LaTeX" [style]
  :input "
          #+begin_style
          h2 { color: teal; }
          #+end_style
          Prose."
  :to latex
  :equals "
           Prose.
           ")

(provide 'org-style-block-tests)
;;; org-style-block-tests.el ends here
