;;; org-defblock-tests.el --- Tests for org-defblock core infrastructure  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "A freshly defined block appears in org--supported-blocks."
  (let ((org--supported-blocks nil))
    (org-defblock test-basic-reg () "")
    (should (member 'test-basic-reg org--supported-blocks))))

(deftest "org-defblock creates an org-block/NAME function."
  (org-defblock test-handler-fn () "handler body"
    (upcase contents))
  (should (fboundp 'org-block/test-handler-fn)))

(define-org-call-test "Block handler output contains the HTML markup and the content."
  :call (using (org-defblock test-html-export () ""
                 (format "<b>%s</b>" contents))
          (org-block/test-html-export 'html "hello" nil))
  :returns "#+begin_export html
<b>
#+end_export
hello
#+begin_export html
</b>
#+end_export")

(define-org-call-test "Block handler output contains the LaTeX markup and the content."
  :call (using (org-defblock test-latex-export () ""
                 (format "\\textbf{%s}" contents))
          (org-block/test-latex-export 'latex "world" nil))
  :returns "#+begin_export latex
\\textbf{
#+end_export
world
#+begin_export latex
}
#+end_export")

(define-org-call-test "Keyword args bind correctly inside the block body."
  :call (using (org-defblock test-kwdargs (main-arg "" color "red") ""
                 (format "%s:%s" color contents))
          (org-block/test-kwdargs 'html "text" nil :color "blue"))
  :returns "#+begin_export html
blue:
#+end_export
text
#+begin_export html

#+end_export")

(define-org-call-test "Main arg uses its default when omitted."
  :call (using (org-defblock test-main-default (label "DEFAULT") ""
                 label)
          (org-block/test-main-default 'html "" nil))
  :returns "#+begin_export html
DEFAULT
#+end_export")

(deftest "org-defblock-only does NOT create an org-link/NAME function."
  (org-defblock-only test-only-block () ""
    contents)
  (should (fboundp 'org-block/test-only-block))
  (should-not (fboundp 'org-link/test-only-block)))

(deftest "org-undefblock removes a block from org--supported-blocks."
  (let ((org--supported-blocks nil))
    (org-defblock test-undefblock-target () "" contents)
    (should (member 'test-undefblock-target org--supported-blocks))
    (org-undefblock test-undefblock-target)
    (should-not (member 'test-undefblock-target org--supported-blocks))))

(deftest "org-set-block-header-args overrides default when no local value is passed."
  (org-defblock test-header-args-block (main-arg "" color "black") ""
    color)
  (let ((org--header-args nil))
    (org-set-block-header-args test-header-args-block :color "purple")
    (let ((result (org-block/test-header-args-block 'html "" nil)))
      (should (string-match-p "purple" result)))))

(org-deftest "Parser extracts name, main-arg, and contents from a temp buffer."
  :text "#+begin_myblock hello\nsome content\n#+end_myblock\n"
  (let ((blk (org-special-block-after-point "myblock")))
    (should (equal (org-special-block-name blk) "myblock"))
    (should (equal (org-special-block-main-arg blk) "hello"))
    (should (string-match-p "some content" (org-special-block-contents blk)))))

(deftest "org-thread-blockcall with no forms returns the body unchanged."
  (should (equal "hello" (org-thread-blockcall "hello"))))

;;; A block's body must yield a string

(deftest "a block whose body yields nil signals an error naming the block, rather than exporting the text nil"
  (using (org-defblock test-yields-nil (&optional x) "Yield nil, by mistake." (when x "hi"))
    (let ((err (should-error (org-export-string "#+begin_test-yields-nil\nbody\n#+end_test-yields-nil" 'html))))
      (should (s-contains-p "test-yields-nil" (cadr err)))
      (should (s-contains-p "(result satisfies (stringp it) = nil : symbol)" (cadr err))))))

(deftest "a block handler rejects a non-symbol backend"
  (using (org-defblock test-backend-check () "Echo." contents)
    (should-error (org-block/test-backend-check "html" "body"))))

;;; A block's #+name:

(org-deftest "a block's #+name: is its anchor"
  :text "#+name: fig\n#+begin_myblock\nbody\n#+end_myblock\n#+begin_myblock\nbody\n#+end_myblock"
  (should (equal "fig" (org-special-block-anchor (org-special-block-after-point "myblock"))))
  (search-forward "body")
  (should-not (org-special-block-anchor (org-special-block-after-point "myblock"))))

(define-org-export-test "a named block yielding Org is labelled by its #+name: and #+caption:, so [[NAME]] reaches it"
  :input "See [[fig]].

          #+name: fig
          #+caption: A figure
          #+begin_pic
          #+end_pic"
  :to latex
  :using (org-defblock pic ()
           "An image, as Org."
           (org-parse "[[file:x.png]]"))
  :equals "See \\ref{fig:org-id-1}.

\\begin{figure}[htbp]
\\centering
\\includegraphics[width=.9\\linewidth]{x.png}
\\caption{\\label{fig:org-id-1}A figure}
\\end{figure}
")

(provide 'org-defblock-tests)
;;; org-defblock-tests.el ends here
