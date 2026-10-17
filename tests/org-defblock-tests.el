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
\\textbf{%
#+end_export
world
#+begin_export latex
\\ifhmode\\unskip\\fi}
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

(deftest "every def macro of ours indents as defun does; e.g., org-defblock, org-deflink, org-deftag, org-undefblock"
  (let ((home (file-name-directory (file-truename (symbol-file 'org-defblock 'defun)))) ours)
    (mapatoms (lambda (symbol)
                (-when-let (file (and (macrop symbol) (string-match-p "\\`org-\\(?:un\\)?def" (symbol-name symbol))
                                      (symbol-file symbol 'defun)))
                  ;; Our package's own sources, wherever it is checked out; not the tests' fixtures, as `org-deftest'.
                  (when (and (string-prefix-p home (file-truename file)) (not (string-match-p "/tests/" file)))
                    (push symbol ours)))))
    (should (memq 'org-defblock ours))
    (should (memq 'org-deflink ours))
    (should (equal (--remove (eq (function-get it 'lisp-indent-function) 'defun) ours) nil))))

(org-deftest "Parser extracts name, main-arg, and contents from a temp buffer."
  :text "#+begin_myblock hello\nsome content\n#+end_myblock\n"
  (let ((blk (org-special-block-after-point "myblock")))
    (should (equal (org-special-block-name blk) "myblock"))
    (should (equal (org-special-block-main-arg blk) "hello"))
    (should (string-match-p "some content" (org-special-block-contents blk)))))

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

;;; How a block looks in Org buffers: :fontify

(defun org-defblock-tests--displayed ()
  "The current buffer's text as Emacs displays it: each string-valued `display' property in place of the text it covers."
  (let ((shown "") (pos (point-min)))
    (while (< pos (point-max))
      (let ((display (get-text-property pos 'display))
            (next (next-single-property-change pos 'display nil (point-max))))
        (setq shown (concat shown (if (stringp display) display (buffer-substring-no-properties pos next)))
              pos next)))
    shown))

(org-deftest "a block's :fontify shows its delimiters as given, and its contents in its face; other blocks are left be" [fontify]
  :text "
#+begin_proof By induction
Trivially so.
#+end_proof
#+begin_proofs
Not a proof block.
#+end_proofs"
  (using (org-defblock proof (&optional title :fontify [:begin "Proof." :end "⏹" :face 'italic])
           "A proof, titled TITLE." (format "%s: %s" title contents))
    (font-lock-ensure)
    (should (equal (org-defblock-tests--displayed) "
Proof. By induction
Trivially so.
⏹
#+begin_proofs
Not a proof block.
#+end_proofs"))
    (search-forward "Trivially")
    (should (memq 'italic (ensure-list (get-text-property (point) 'face))))
    (search-forward "Not a proof")
    (should-not (memq 'italic (ensure-list (get-text-property (point) 'face))))))

(deftest "undefining a block forgets its :fontify look" [fontify]
  (using (org-defblock proof (:fontify [:end "⏹"]) "A proof." contents)
    (should (assoc "proof" org--block-fontifications)))
  (should-not (assoc "proof" org--block-fontifications)))

(deftest "a :fontify display edited away is cleared as Org refontifies" [fontify]
  (using (org-defblock proof (:fontify [:end "⏹"]) "A proof." contents)
    (with-org-test-buffer (:text "
#+begin_proof
Done.
#+end_proof")
      (font-lock-ensure)
      (search-forward "#+end_proof")
      (replace-match "#+end_story")
      (font-lock-ensure)
      (should (equal (org-defblock-tests--displayed) "
#+begin_proof
Done.
#+end_story")))))



(deftest "with the mode off, exporting one of our blocks warns that it exports as Org's own; issue #59" [mode]
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning) (lambda (_type message &rest _) (push message warnings))))
      (unwind-protect
          (progn
            (org-special-block-extras-mode -1)
            ;; As reported: the link is ours, the block Org's own.  Org's exporter, since `org-export-string' would
            ;; turn the mode on again.
            (should (s-contains-p "\\begin{rremark}" (org-export-string-as "
#+begin_rremark Jasim
Tighten this sentence.
#+end_rremark" 'latex t)))
            (should (string-match-p "#\\+begin_rremark exports as Org's own special block" (car warnings))))
        (org-special-block-extras-mode 1))
      ;; With the mode on, the block is ours, and nothing is said.
      (setq warnings nil)
      (should (s-contains-p "\\color{red}" (org-export-string "
#+begin_rremark Jasim
Tighten this sentence.
#+end_rremark" 'latex)))
      (should-not warnings))))

(deftest "a block renders for a backend derived from LaTeX, as for LaTeX; and may single out one backend; issue #12" [derived]
  (require 'ox-md)
  (org-export-define-derived-backend 'osbe-test-mylatex 'latex)
  (org-export-define-derived-backend 'osbe-test-myhugo 'md)
  (using (org-defblock myblock ()
           "Wrap the contents in \\my-latex-command; or, in the Hugo-like backend, in a docsy alert."
           (cond ((org-export-derived-backend-p org-export-current-backend 'osbe-test-myhugo)
                  (format "{{%% alert %%}}%s{{%% /alert %%}}" raw-contents))
                 ((equal backend 'latex) (concat "\\my-latex-command\n" raw-contents "\n\\my-latex-command"))
                 (t raw-contents)))
    ;; As in the issue: the derived backend's blocks see `backend' as latex.
    (should (equal (org-export-string "#+begin_myblock\nsome content\n#+end_myblock" 'osbe-test-mylatex)
                   "\\my-latex-command\nsome content\n\\my-latex-command\n"))
    (should (s-contains-p "{{% alert %}}some content" (org-export-string "#+begin_myblock\nsome content\n#+end_myblock" 'osbe-test-myhugo)))))

(deftest "a block's main argument may also be given by its own name, as a keyword; issue #47" [main-arg]
  (using (org-defblock satz (name "" label "")
           "A labelled sentence, named NAME."
           (format "\\tag{%s}\\label{%s} %s" name label contents))
    (dolist (header '("myname :label mylabel" ":name myname :label mylabel"))
      (should (s-contains-p "\\tag{myname}\\label{mylabel}"
                            (org-export-string (format "#+begin_satz %s\nExample Sentence.\n#+end_satz" header) 'latex))))))

(deftest "org-defblock may override one of Org's own blocks, saying so, and hand some of them back to Org" [builtin]
  (let (messages)
    (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (using (org-defblock verse (&optional poet)
               "A verse, signed by POET; sans one, Org's own verse."
               (if (org-string-nw-p poet)
                   (format "\\begin{verse}\n%s\n\\end{verse}\n--- %s" contents poet)
                 (org-parse (org-builtin-block "verse" raw-contents))))
        (should (string-match-p "verse overrides Org's own #\\+begin_verse" (car messages)))
        (let ((latex (org-export-string "
#+begin_verse Rumi
Out beyond ideas of wrongdoing,
#+end_verse

#+begin_verse
Plain verse.
#+end_verse" 'latex)))
          ;; Ours, signed; and Org's own, handed back once, rather than rewritten forever.
          (should (s-contains-p "\\end{verse}\n--- Rumi" latex))
          (should (s-contains-p "\\begin{verse}\nPlain verse.\\\\\n\\end{verse}" latex)))))))

(deftest "a block's #+begin_ line written within a src or example block is its text, not a block to rewrite" [builtin]
  (dolist (outer '("src org" "example"))
    (let ((latex (org-export-string (format "
#+begin_%s
#+begin_tip
Inner.
#+end_tip
#+end_%s" outer (car (split-string outer))) 'latex)))
      (should (s-contains-p "#+begin_tip" latex))
      (should-not (s-contains-p "tcolorbox" latex)))))

(deftest "a block's :latex-head joins the preamble once, when the export uses it, unless the preamble has it" [latex-head]
  (using (org-defblock thm (&optional title :latex-head ("amsthm" "\\newtheorem{theorem}{Theorem}"))
           "A theorem."
           (if (org-string-nw-p title)
               (format "\\begin{theorem}[%s]%s\\end{theorem}" title contents)
             (org-parse (org-builtin-block "center" raw-contents))))
    (cl-flet ((amsthm (input) (s-count-matches "usepackage\\(?:\\[[^]]*\\]\\)?{amsthm}" (org-export-string-as input 'latex))))
      (should (= 1 (amsthm "#+begin_thm Pythagoras\nIt holds.\n#+end_thm")))
      ;; As a link, too; and the document's own, options and all, is left be.
      (should (= 1 (amsthm "See [[thm:Euclid][infinitely many primes]].")))
      (should (= 1 (amsthm "#+latex_header: \\usepackage[thmmarks]{amsthm}\n#+begin_thm Pythagoras\nIt holds.\n#+end_thm")))
      ;; Any other line is added once too, however many theorems there are; unless the preamble has it.
      (cl-flet ((newtheorem (input) (s-count-matches "newtheorem{theorem}" (org-export-string-as input 'latex))))
        (should (= 1 (newtheorem "#+begin_thm A\nOne.\n#+end_thm\n#+begin_thm B\nTwo.\n#+end_thm")))
        (should (= 1 (newtheorem "#+latex_header: \\newtheorem{theorem}{Theorem}\n#+begin_thm A\nOne.\n#+end_thm"))))
      ;; Sans the block, or handed back to Org, nothing is loaded.
      (should (= 0 (amsthm "No theorems here.")))
      (should (= 0 (amsthm "#+begin_thm\nHanded back.\n#+end_thm"))))))

(provide 'org-defblock-tests)
;;; org-defblock-tests.el ends here
