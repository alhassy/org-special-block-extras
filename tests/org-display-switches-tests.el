;;; org-display-switches-tests.el --- How a block is shown: :filename, :copy, :tab, :highlight  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-export-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; :filename

(deftest "a block's :filename puts its file name atop it, along each backend" [switches filename]
  (should (string-match-p "<div class=\"osbe-filename\"[^>]*><code>main.c</code></div>\n<div class=\"org-src-container\">"
                          (org-export-string "#+begin_src C :filename main.c :copy nil\nint x;\n#+end_src")))
  (should (string-match-p "\\\\texttt{main.c}}\\\\par\\\\nopagebreak\n\\\\begin{verbatim}"
                          (org-export-string "#+begin_src C :filename main.c\nint x;\n#+end_src" 'latex)))
  (should (string-match-p "^`main.c`"
                          (org-export-string "#+begin_src C :filename main.c\nint x;\n#+end_src" 'md))))

(deftest "a src block's :filename t names the file it tangles to; none is an error" [switches filename]
  (should (string-match-p "<code>init.el</code>"
                          (org-export-string "#+begin_src emacs-lisp :tangle ~/.emacs.d/init.el :filename t\n(x)\n#+end_src")))
  (should (string-match-p "<code>init.el</code>"
                          (org-export-string "#+PROPERTY: header-args:emacs-lisp :tangle init.el\n#+begin_src emacs-lisp :filename t\n(x)\n#+end_src")))
  (should-error (org-export-string "#+begin_src emacs-lisp :filename t\n(x)\n#+end_src") :type 'user-error))

(deftest "a src block whose code is not exported has no :filename, nor :copy" [switches filename copy]
  (let ((org-confirm-babel-evaluate nil))
    (should-not (string-match-p "osbe-filename\\|osbe-copy"
                                (org-export-string "#+begin_src emacs-lisp :filename x.el :exports results\n(+ 1 2)\n#+end_src")))))

;;; :copy

(deftest "every src and example block has a copy button in HTML, by `org-babel-default-header-args:src'" [switches copy]
  (should (string-match-p "<div class=\"osbe-copy\"[^>]*>\n<button class=\"osbe-copy-button\""
                          (org-export-string "#+begin_src haskell\nf = 1\n#+end_src")))
  (should (string-match-p "osbe-copy-button" (org-export-string "#+begin_example\nf = 1\n#+end_example")))
  (should-not (string-match-p "osbe-copy" (org-export-string "#+begin_src haskell\nf = 1\n#+end_src" 'latex)))
  (should-not (string-match-p "osbe-copy" (org-export-string "#+begin_src haskell\nf = 1\n#+end_src" 'md))))

(deftest "a copy button is turned off by :copy nil, for a block, a language, or every src block of a file" [switches copy]
  (should-not (string-match-p "osbe-copy" (org-export-string "#+begin_src haskell :copy nil\nf = 1\n#+end_src")))
  (should-not (string-match-p "osbe-copy" (org-export-string "#+PROPERTY: header-args:haskell :copy nil\n#+begin_src haskell\nf = 1\n#+end_src")))
  (should-not (string-match-p "osbe-copy" (org-export-string "#+PROPERTY: header-args:src :copy nil\n#+begin_src haskell\nf = 1\n#+end_src")))
  (should (string-match-p "osbe-copy" (org-export-string "#+begin_red :copy t\nHi\n#+end_red"))))

;;; :tab

(deftest "adjacent :tab blocks are each a titled tab, which the page's script gathers into one tabset" [switches tab]
  (let ((html (org-export-string "#+begin_src haskell :tab t :copy nil\nf = 1\n#+end_src\n#+begin_src python :tab Python3 :copy nil\nf = 1\n#+end_src")))
    (should (equal 2 (s-count-matches "<div class=\"osbe-tab\">" html)))
    (should (string-match-p "<div class=\"osbe-tab-title\"><strong>haskell</strong></div>" html))
    (should (string-match-p "<div class=\"osbe-tab-title\"><strong>Python3</strong></div>" html))
    (should (string-match-p "window.osbeTabs" html))))

(deftest "a :tab block, along LaTeX, is beneath its title in bold" [switches tab]
  (should (string-match-p "\\\\textbf{Haskell}"
                          (org-export-string "#+begin_src haskell :tab Haskell\nf = 1\n#+end_src" 'latex))))

;;; :highlight

(deftest "`org--highlighted-lines' reads numbers, ranges, and coderef labels" [switches highlight]
  (should (equal (org--highlighted-lines "2,4-6") '(2 4 5 6)))
  (should (equal (org--highlighted-lines 3) '(3)))
  (should (equal (org--highlighted-lines '(1 loop)) '(1 "loop"))))

(deftest "a block's :highlight lights each line it names, by number or by coderef label" [switches highlight]
  (let* ((org-html-htmlize-output-type nil)
         (html (org-export-string "#+begin_src C -n :highlight (1 loop) :copy nil
int tot = 1;
for (int i = 0; i != 10; i++)   (ref:loop)
   tot *= i;
#+end_src"))
         (lines (split-string html "\n")))
    (should (string-match-p "<pre style=\"position: relative;\" class=\"src src-C\">" html))
    (should (equal 2 (s-count-matches "osbe-highlight" html)))
    (should-not (string-match-p "osbe-highlight" (--first (string-match-p "tot \\*= i" it) lines)))
    (should (string-match-p "osbe-highlight" (--first (string-match-p (regexp-quote "i++") it) lines)))))

(deftest "a page with a [[(name)]] link has Org's own script that lights its coderef's line on hover" [switches highlight]
  (let ((org-html-head-include-scripts nil))
    (with-temp-buffer
      (insert "#+begin_src C\nint x;   (ref:x)\n#+end_src\nSee [[(x)]].")
      (org-mode)
      (let ((html (org-export-as 'html)))
        (should (string-match-p "function CodeHighlightOn" html))))))

;;; :log

(deftest "a block's :log t appends what it transpiles to, along the export's backend, to *Org Block Log*" [switches log]
  (when (get-buffer "*Org Block Log*") (kill-buffer "*Org Block Log*"))
  (let ((html (org-export-string "#+begin_box Lemma :log t\nHi *there*\n#+end_box")))
    (should (string-match-p "<h3>Lemma</h3>" html))
    (with-current-buffer "*Org Block Log*"
      (should (string-match-p "\\`#\\+begin_box ⟶ html, [0-9:]+\n<div style=[^\n]*<h3>Lemma</h3>\\(?:.\\|\n\\)*<b>there</b>"
                              (buffer-string))))))

(deftest "a block's :log FILE writes what it transpiles to into FILE" [switches log]
  (let ((file (make-temp-file "osbe-log")))
    (unwind-protect
        (progn (org-export-string (format "#+begin_red :log %s\nHi\n#+end_red" file) 'latex)
               (should (string-match-p "\\\\color{red}" (with-temp-buffer (insert-file-contents file) (buffer-string)))))
      (delete-file file))))

(org-deftest "C-c C-c within a :log block shows what it transpiles to, along each of `org-demo-backends'" [switches log]
  :text "
#+begin_box Lemma :log t
Hi
#+end_box"
  :at "Hi"
  (let (shown)
    (cl-letf (((symbol-function 'display-message-or-buffer) (lambda (text &rest _) (setq shown text))))
      (should (org--block-log-cc))
      (should (string-match-p "#\\+begin_box ⟶ html\n<div[^\n]*<h3>Lemma</h3>" shown))
      (should (string-match-p "#\\+begin_box ⟶ latex\n.*tcolorbox" shown)))))
