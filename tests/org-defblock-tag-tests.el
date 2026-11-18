;;; org-defblock-tag-tests.el --- `org-defblock''s tags: a section tagged :NAME: exports as a NAME block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; Where the block's arguments come from

(define-org-export-test "a tagged section exports as a block, the tag's underscore items forming the main argument"
  :input "* Why bother? :memo_Spoiler_alert:
          Body text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Why bother?}
\\label{sec:org-id-1}
[Spoiler alert/teal: 
Body text.
]
")

(define-org-export-test "a tagged section's properties supply the block's keyword arguments"
  :input "* Why bother? :memo_Heads_up:
          :PROPERTIES:
          :COLOR: pink
          :END:
          Body text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Why bother?}
\\label{sec:org-id-1}
[Heads up/pink: 
Body text.
]
")

(define-org-export-test "a bare tag reads the main argument from the property of the same name, else a title from the heading"
  :input "* With a property :memo:
          :PROPERTIES:
          :TITLE: From the drawer
          :END:
          One.
          * Without :memo:
          Two."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{With a property}
\\label{sec:org-id-1}
[From the drawer/teal: 
One.
]
\\section{Without}
\\label{sec:org-id-2}
[Without/teal: 
Two.
]
")

;; Concretely, a `:graph:' section's drawer reads `:RANKDIR: LR', with no `rankdir' argument declared anywhere.
(define-org-export-test "a block that allows other keys receives every property of its section, bar Org's own"
  :input "* Plot :memo:
          :PROPERTIES:
          :RANKDIR: LR
          :NODE.SHAPE: box
          :CUSTOM_ID: plot
          :END:
          Body."
  :to latex
  :using (org-defblock memo (&optional title &key &allow-other-keys :tag t &context args)
           "Show the header's keyword arguments."
           (format "%S" args))
  :equals "\\section{Plot}
\\label{sec:org-id-1}
(:node.shape \"box\" :rankdir \"LR\")
")

(define-org-export-test "a block that refuses other keys receives only the properties it declares"
  :input "* Plot :memo:
          :PROPERTIES:
          :RANKDIR: LR
          :COLOR: red
          :END:
          Body."
  :to latex
  :using (org-defblock memo (&optional title &key color :tag t &context args)
           "Show the header's keyword arguments."
           (format "%S" args))
  :equals "\\section{Plot}
\\label{sec:org-id-1}
(:color \"red\")
")

;;; Which text is wrapped, and which sections and tags are touched

(define-org-export-test "a bare tag uses the main argument's default when that argument is not a title"
  :input "* Cheers :cheer:
          Hip hip."
  :to latex
  :using (org-defblock cheer (&optional (reps 2) :tag t &context contents)
           "Repeat CONTENTS REPS times." (format "[%s×: %s]" reps contents))
  :equals "\\section{Cheers}
\\label{sec:org-id-1}
[2×: 
Hip hip.
]
")

(define-org-export-test "a tagged section keeps its heading and other tags, and wraps only its own text"
  :input "* Outer :draft:memo:
          Outer text.
          ** Inner
          Inner text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Outer\\hfill{}\\textsc{draft}}
\\label{sec:org-id-1}
[Outer/teal: 
Outer text.
]
\\subsection{Inner}
\\label{sec:org-id-2}
Inner text.
")

(define-org-export-test "untagged sections export as usual"
  :input "* Plain
          Plain text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Plain}
\\label{sec:org-id-1}
Plain text.
")

(define-org-export-test "with :tag nil, a block leaves sections bearing its name alone"
  :input "* Just a tag :memo:
          Body text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag nil &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Just a tag\\hfill{}\\textsc{memo}}
\\label{sec:org-id-1}
Body text.
")

(define-org-call-test "org--defblock-parse-args passes :tag through, as any other option"
  :call (org--defblock-parse-args '(&optional (title "Memo") &key (color "teal") :tag t))
  :returns (:main (title "Memo") :keys ((color "teal")) :other-keys nil :context nil :options (:tag t)))

(define-org-call-test "org--defblock-parse-args accepts :tag right after &optional, without &key"
  :call (org--defblock-parse-args '(&optional (title "Memo") :tag t))
  :returns (:main (title "Memo") :keys nil :other-keys nil :context nil :options (:tag t)))

(deftest "a tagged section keeps its property drawer, so properties such as CUSTOM_ID still apply"
  (using (org-defblock memo (&optional (title "Memo") &context contents) "Show TITLE." (format "[%s: %s]" title contents))
    (let ((html (org-export-string "* Heading :memo:\n:PROPERTIES:\n:CUSTOM_ID: here\n:END:\nBody." 'html)))
      (should (s-contains-p "id=\"here\"" html))
      (should-not (s-contains-p ":CUSTOM_ID:" html)))))

;;; What becomes of a tagged section's subsections: `:tag [:subsections MODE]'

(define-org-export-test "with :subsections ignore, as with :tag t, subsections stay outside the block"
  :input "* Outer :memo:
          Outer text.
          ** Inner
          Inner text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag [:subsections ignore] &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Outer}
\\label{sec:org-id-1}
[Outer/teal:
Outer text.
]
\\subsection{Inner}
\\label{sec:org-id-2}
Inner text.
")

(deftest "with :subsections error, a tagged section with a subsection is refused, naming both"
  (using (org-defblock memo (&optional (title "Memo") :tag [:subsections error] &context contents)
           "Show TITLE." (format "[%s: %s]" title contents))
    (let ((err (should-error (org-export-string "* Outer :memo:\nOuter text.\n** Inner\nInner text." 'latex)
                             :type 'user-error)))
      (should (s-contains-p "“Outer”" (cadr err)))
      (should (s-contains-p "“Inner”" (cadr err)))
      (should (s-contains-p "org-defblock memo" (cadr err))))))

(define-org-export-test "with :subsections error, a tagged section without subsections exports as usual, siblings and all"
  :input "* Lonely :memo:
          Text.
          * Sibling
          Sibling text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag [:subsections error] &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Lonely}
\\label{sec:org-id-1}
[Lonely/teal:
Text.
]
\\section{Sibling}
\\label{sec:org-id-2}
Sibling text.
")

(define-org-export-test "with :subsections recurse, each subsection's text, at any depth, gets a block with the same arguments"
  :input "* Outer :memo_Big:
          Outer text.
          ** Inner
          Inner text.
          *** Innermost
          :PROPERTIES:
          :CUSTOM_ID: deep
          :END:
          Deep text.
          * After
          After text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag [:subsections recurse] &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Outer}
\\label{sec:org-id-1}
[Big/teal:
Outer text.
]
\\subsection{Inner}
\\label{sec:org-id-2}
[Big/teal:
Inner text.
]
\\subsubsection{Innermost}
\\label{sec:org-id-3}
[Big/teal:
Deep text.
]
\\section{After}
\\label{sec:org-id-4}
After text.
")

(define-org-export-test "with :subsections recurse, a subsection bearing the tag itself is wrapped once, with its own arguments"
  :input "* Outer :memo_Big:
          Outer text.
          ** Own :memo_Small:
          Own text.
          ** Plain
          Plain text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag [:subsections recurse] &context contents)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Outer}
\\label{sec:org-id-1}
[Big/teal:
Outer text.
]
\\subsection{Own}
\\label{sec:org-id-2}
[Small/teal:
Own text.
]
\\subsection{Plain}
\\label{sec:org-id-3}
[Big/teal:
Plain text.
]
")

(define-org-call-test "org--defblock-tag-subsections reads :tag t as ignore"
  :call (org--defblock-tag-subsections 'memo t)
  :returns ignore)

(define-org-call-test "org--defblock-tag-subsections reads the mode from [:subsections MODE]"
  :call (org--defblock-tag-subsections 'memo [:subsections recurse])
  :returns recurse)

(deftest "org-defblock refuses, as it expands, an unknown :subsections mode or :tag key"
  (should-error (macroexpand '(org-defblock memo (&optional title :tag [:subsections sideways]) title)))
  (should-error (macroexpand '(org-defblock memo (&optional title :tag [:sections error]) title))))

;;; Lifecycle

(deftest "org-defblock with :tag t defines org--NAME-tag, and org-undefblock removes it"
  (eval '(org-defblock memo-lifecycle (&optional (title "Memo") :tag t) "Show TITLE." title))
  (should (fboundp 'org--memo-lifecycle-tag))
  (should (memq #'org--memo-lifecycle-tag org-export-before-parsing-functions))
  (eval '(org-undefblock memo-lifecycle))
  (should-not (fboundp 'org--memo-lifecycle-tag))
  (should-not (memq #'org--memo-lifecycle-tag org-export-before-parsing-functions)))

(deftest "tags run before special blocks are rewritten, whatever the order they were hooked in"
  (using (org-defblock memo (&optional (title "Memo") :tag t &context contents) "Show TITLE." (format "[%s: %s]" title contents))
    ;; Re-enabling the mode re-adds the block rewriter after the tag was hooked.
    (org-special-block-extras-mode -1)
    (org-special-block-extras-mode 1)
    (should (s-contains-p "[Heading: " (org-export-string "* Heading :memo:\nBody." 'latex)))))

(deftest "disabling the mode removes every hook and advice that enabling it installs"
  (unwind-protect
      (progn
        (org-special-block-extras-mode -1)
        (should-not (memq #'org--rewrite-special-blocks-by-handlers org-export-before-parsing-functions))
        (should-not (memq #'org--html-export-style-setup org-export-before-processing-functions))
        (should-not (advice-member-p #'osbe--fontify-blocks-as-org 'org-fontify-meta-lines-and-blocks))
        (should-not (memq #'org--docs-prepare org-export-before-parsing-functions))
        (org-special-block-extras-mode 1)
        (should (memq #'org--rewrite-special-blocks-by-handlers org-export-before-parsing-functions))
        (should (memq #'org--html-export-style-setup org-export-before-processing-functions))
        (should (advice-member-p #'osbe--fontify-blocks-as-org 'org-fontify-meta-lines-and-blocks))
        (should (memq #'org--docs-prepare org-export-before-parsing-functions)))
    (org-special-block-extras-mode 1)))

(deftest "a :parallel: section sets each subheading as a column, as #+columnbreak: does; their count is the default"
  (should (s-contains-p (string-trim (org-export-string-as "
#+begin_parallel 2 :bar green
*Lisp*
Code is data.
#+columnbreak:
*Haskell*
Types are propositions.
#+end_parallel" 'latex t))
                        (org-export-string-as "
* Lisp or Haskell? :parallel:
:PROPERTIES:
:BAR: green
:END:
** Lisp
:PROPERTIES:
:CUSTOM_ID: lisp
:END:
Code is data.
** Haskell
Types are propositions." 'latex t))))

(deftest "a :parallel: section's own text sits above its columns, and a sub-subheading is a bold paragraph in its column"
  (let ((latex (org-export-string-as "
* Three creeds :parallel:
Each, a slogan.
** Lisp
Code is data.
*** Macros
Code that writes code.
** Haskell
Types are propositions.
** Prolog
Programs are proofs." 'latex t)))
    (should (s-contains-p "Each, a slogan.\n\n\\par" latex))
    (should (s-contains-p "\\begin{multicols}{3}" latex))
    (should (s-contains-p "Code is data.\n\n\\textbf{Macros}\nCode that writes code." latex))
    (should (= 2 (s-count-matches "columnbreak" latex)))
    (should-not (s-contains-p "subsection" latex))))

(deftest "details, box, spoiler, solution, and parallel are also tags"
  (dolist (name '(details box spoiler solution parallel))
    (should (fboundp (intern (format "org--%s-tag" name))))))

(define-org-export-test "a section tagged :details_Hint: exports as a details block titled Hint"
  :input "* Stuck? :details_Hint:
          Try induction."
  :to html
  :equals "<div id=\"table-of-contents\" role=\"doc-toc\">
<h2>Table of Contents</h2>
<div id=\"text-table-of-contents\" role=\"doc-toc\">
<ul>
<li><a href=\"#org-id-1\">1. Stuck?</a></li>
</ul>
</div>
</div>
<div id=\"outline-container-org-id-1\" class=\"outline-2\">
<h2 id=\"org-id-1\"><span class=\"section-number-2\">1.</span> Stuck?</h2>
<div class=\"outline-text-2\" id=\"text-1\">
<details class=\"code-details\" style=\"padding: 1em; background-color: #e5f5e5; border-radius: 15px;
color: hsl(157 75% 20%); font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">
<summary><strong><font face=\"Courier\" size=\"3\" color=\"green\">Hint</font></strong></summary>
<p>
Try induction.
</p>

</details>
</div>
</div>
")

(ert-deftest org-defblock-tag-renders-for-derived-backends ()
  "A tagged section exports as its block under a backend derived from html, not as an ignored export block."
  (org-export-define-derived-backend 'org-test-derived-html 'html)
  (org-defblock shout (&optional (title "Shout") :tag t &context contents)
    "Wrap CONTENTS in a <shout> element."
    (format "<shout>%s</shout>" contents))
  (unwind-protect
      (should (string-match-p "<shout>" (org-export-string-as "* Hi :shout:\nBody" 'org-test-derived-html t)))
    (org-undefblock shout)))

(provide 'org-defblock-tag-tests)
;;; org-defblock-tag-tests.el ends here
