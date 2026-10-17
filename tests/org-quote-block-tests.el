;;; org-quote-block-tests.el --- Tests for cited quotations  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "a block's Org contents within LaTeX braces hug them, sans the spaces of their newlines; issue #27" [quote]
  (should (s-contains-p "\\blockcquote[20-22]{zermelo1908}{%
\\textbf{This} is a \\emph{try}.
\\ifhmode\\unskip\\fi}"
                        (org-export-string "
#+begin_quote zermelo1908 :pages 20-22
*This* is a /try/.
#+end_quote" 'latex)))
  ;; Outside braces, a newline is a space the handler may well want.
  (org-defblock takeaway () "A takeaway: CONTENTS." (format "Takeaway: %s" contents))
  (should (s-contains-p "Takeaway: \nRemarkable."
                        (org-export-string "
#+begin_takeaway
Remarkable.
#+end_takeaway" 'latex))))

(deftest "a quote given a key is csquotes' \\blockcquote in LaTeX, as the issue spells it, :key and all; issue #24" [quote]
  (dolist (header '(":key zermelo1908 :pages 20-22" "zermelo1908 :pages 20-22"))
    (should (s-contains-p "\\blockcquote[20-22]{zermelo1908}{%\nThis is a \\emph{blockquote}.\n\\ifhmode\\unskip\\fi}"
                          (org-export-string (format "#+begin_quote %s\n\n    This is a /blockquote/.\n\n#+end_quote" header)
                                             'latex))))
  ;; Sans pages, sans the optional argument.
  (should (s-contains-p "\\blockcquote{zermelo1908}{"
                        (org-export-string "#+begin_quote zermelo1908\nQuoted.\n#+end_quote" 'latex))))

(deftest "a quote given a key is a blockquote attributed to its key, at its pages, in HTML and Markdown" [quote]
  (require 'ox-md)
  (let ((input "#+begin_quote zermelo1908 :pages 20-22\nQuoted.\n#+end_quote"))
    (should (s-contains-p "<footer>&mdash; <cite>zermelo1908</cite>, 20-22</footer></blockquote>"
                          (org-export-string input 'html)))
    (should (s-contains-p "> Quoted.\n> \n> &mdash; *zermelo1908*, 20-22" (org-export-string input 'md)))))

(deftest "a plain quote, sans arguments, stays Org's own, in every backend, and warns of nothing" [quote]
  (require 'ox-md)
  (let ((input "
- item
  #+begin_quote
  To be, or not to be.
  #+end_quote
- next")
        warnings)
    (dolist (backend '(latex html md))
      (let ((ours (org-export-string input backend))
            (orgs (unwind-protect
                      (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) (push args warnings))))
                        (org-special-block-extras-mode -1)
                        (org-export-string-as input backend t))
                    (org-special-block-extras-mode 1))))
        (should (equal ours orgs))))
    (should-not warnings)))

(deftest "a quote: link is an inline quotation, cited when keyed; a section tagged :quote: is Org's own quote of its text" [quote]
  (let ((latex (org-export-string "
Hamlet says [[quote:][to be]]; Zermelo writes [[quote:zermelo1908][set theory is …]].

* Credo :quote:
Less is more." 'latex)))
    (should (s-contains-p "Hamlet says ``to be''; Zermelo writes \\textcquote{zermelo1908}{set theory is …}." latex))
    (should (string-match-p "\\\\begin{quote}\nLess is more\\.\n\\\\end{quote}" latex))))

(deftest "a cited quote, or quote: link, loads csquotes in LaTeX, unless the document already does" [quote]
  (cl-flet ((csquotes (input) (s-count-matches "{csquotes}" (org-export-string-as input 'latex))))
    (should (= 1 (csquotes "#+begin_quote zermelo1908 :pages 20-22\nQuoted.\n#+end_quote")))
    (should (= 1 (csquotes "As Zermelo has it, [[quote:zermelo1908][set theory]] is foundational.")))
    ;; The document's own, options and all, is left be; and a document sans cited quotes gets nothing.
    (should (= 1 (csquotes "#+latex_header: \\usepackage[autostyle]{csquotes}\n#+begin_quote zermelo1908\nQuoted.\n#+end_quote")))
    (should (= 0 (csquotes "#+begin_quote\nPlain.\n#+end_quote")))))

(provide 'org-quote-block-tests)
;;; org-quote-block-tests.el ends here
