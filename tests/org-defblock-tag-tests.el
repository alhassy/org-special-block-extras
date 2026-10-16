;;; org-defblock-tag-tests.el --- `org-defblock''s tags: a section tagged :NAME: exports as a NAME block  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; Where the block's arguments come from

(define-org-export-test "a tagged section exports as a block, the tag's underscore items forming the main argument"
  :input "* Why bother? :memo_Spoiler_alert:
          Body text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t)
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
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Why bother?}
\\label{sec:org-id-1}
[Heads up/pink: 
Body text.
]
")

(define-org-export-test "a bare tag reads the main argument from the property of the same name, else uses defaults"
  :input "* With a property :memo:
          :PROPERTIES:
          :TITLE: From the drawer
          :END:
          One.
          * Without :memo:
          Two."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{With a property}
\\label{sec:org-id-1}
[From the drawer/teal: 
One.
]
\\section{Without}
\\label{sec:org-id-2}
[Memo/teal: 
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
  :using (org-defblock memo (&optional title &key &allow-other-keys :tag t)
           "Show the header's keyword arguments."
           (format "%S" org--handler-args))
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
  :using (org-defblock memo (&optional title &key color :tag t)
           "Show the header's keyword arguments."
           (format "%S" org--handler-args))
  :equals "\\section{Plot}
\\label{sec:org-id-1}
(:color \"red\")
")

;;; Which text is wrapped, and which sections and tags are touched

(define-org-export-test "a tagged section keeps its heading and other tags, and wraps only its own text"
  :input "* Outer :draft:memo:
          Outer text.
          ** Inner
          Inner text."
  :to latex
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Outer\\hfill{}\\textsc{draft}}
\\label{sec:org-id-1}
[Memo/teal: 
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
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag t)
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
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag nil)
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Just a tag\\hfill{}\\textsc{memo}}
\\label{sec:org-id-1}
Body text.
")

(define-org-call-test "org--defblock-desugar-args passes :tag through, as any other option"
  :call (org--defblock-desugar-args '(&optional (title "Memo") &key (color "teal") :tag t))
  :returns (title "Memo" color "teal" :tag t :allow-other-keys nil))

(define-org-call-test "org--defblock-desugar-args accepts :tag right after &optional, without &key"
  :call (org--defblock-desugar-args '(&optional (title "Memo") :tag t))
  :returns (title "Memo" :tag t :allow-other-keys nil))

(deftest "a tagged section keeps its property drawer, so properties such as CUSTOM_ID still apply"
  (using (org-defblock memo (&optional (title "Memo")) "Show TITLE." (format "[%s: %s]" title contents))
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
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag [:subsections ignore])
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Outer}
\\label{sec:org-id-1}
[Memo/teal:
Outer text.
]
\\subsection{Inner}
\\label{sec:org-id-2}
Inner text.
")

(deftest "with :subsections error, a tagged section with a subsection is refused, naming both"
  (using (org-defblock memo (&optional (title "Memo") :tag [:subsections error])
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
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag [:subsections error])
           "Show TITLE and COLOR around CONTENTS."
           (format "[%s/%s: %s]" title color contents))
  :equals "\\section{Lonely}
\\label{sec:org-id-1}
[Memo/teal:
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
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag [:subsections recurse])
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
  :using (org-defblock memo (&optional (title "Memo") &key (color "teal") :tag [:subsections recurse])
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

(deftest "org-defblock with :tag t defines org-deftag/NAME, and org-undefblock removes it"
  (eval '(org-defblock memo-lifecycle (&optional (title "Memo") :tag t) "Show TITLE." title))
  (should (fboundp 'org-deftag/memo-lifecycle))
  (should (memq #'org-deftag/memo-lifecycle org-export-before-parsing-functions))
  (eval '(org-undefblock memo-lifecycle))
  (should-not (fboundp 'org-deftag/memo-lifecycle))
  (should-not (memq #'org-deftag/memo-lifecycle org-export-before-parsing-functions)))

(deftest "tags run before special blocks are rewritten, whatever the order they were hooked in"
  (using (org-defblock memo (&optional (title "Memo") :tag t) "Show TITLE." (format "[%s: %s]" title contents))
    ;; Re-enabling the mode re-adds the block rewriter after the tag was hooked.
    (org-special-block-extras-mode -1)
    (org-special-block-extras-mode 1)
    (should (s-contains-p "[Memo: " (org-export-string "* Heading :memo:\nBody." 'latex)))))

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

(deftest "details, box, spoiler, solution, and parallel are also tags"
  (dolist (name '(details box spoiler solution parallel))
    (should (fboundp (intern (format "org-deftag/%s" name))))))

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
  (org-defblock shout (&optional (title "Shout") :tag t)
    "Wrap CONTENTS in a <shout> element."
    (format "<shout>%s</shout>" contents))
  (unwind-protect
      (should (string-match-p "<shout>" (org-export-string-as "* Hi :shout:\nBody" 'org-test-derived-html t)))
    (org-undefblock shout)))

(provide 'org-defblock-tag-tests)
;;; org-defblock-tag-tests.el ends here
