;;; org-block-switches-tests.el --- Switches every block understands: :noexport, :when, :let, :ignore, :boxed, :folded, :C-c_C-c  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-export-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;;; Export switches

(define-org-export-test "a block marked :noexport t exports to nothing" [switches]
  :input "
          #+begin_red :noexport t
          A draft thought.
          #+end_red
          Published prose."
  :equals "<p>
           Published prose.</p>
           ")

(define-org-export-test "a block whose :when form is nil exports to nothing" [switches]
  :input "
          #+begin_red :when (eq backend 'latex)
          Only in print.
          #+end_red"
  :equals "")

(define-org-export-test "a block whose :when form is non-nil exports as usual; the form sees `backend'" [switches]
  :input "
          #+begin_red :when (eq backend 'latex)
          Only in print.
          #+end_red"
  :to latex
  :equals "\\begingroup\\color{red}
           Only in print.
           \\endgroup\\,
           ")

(define-org-export-test "a block marked :ignore t exports its contents as plain Org" [switches]
  :input "
          #+begin_red :ignore t
          Read /me/ plainly.
          #+end_red"
  :equals "<p>
           Read <i>me</i> plainly.</p>
           ")

(define-org-export-test "a block's :let replaces each whole-word variable by its value; later values see earlier ones" [switches]
  :input "
          #+begin_box Exercise :let (N 6 ANSWER (* N 7))
          What is N × 7? It is ANSWER; whereas Nx, and the word answer, are untouched.
          #+end_box"
  :equals "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
<h3>Exercise</h3>
<p>
What is 6 × 7? It is 42; whereas Nx, and the word answer, are untouched.
</p>
</div>
")

(define-org-export-test "a block's :let replaces operators too, such as -> amid p->q, since a variable's punctuation ends need no word boundary" [switches]
  :input "
          #+begin_red :let (-> \"⇒\" == \"≡\")
          Both p -> q == ¬p ∨ q and p->q==¬p∨q read as implications.
          #+end_red"
  :equals "<span style=\"color:red;\">
<p>
Both p ⇒ q ≡ ¬p ∨ q and p⇒q≡¬p∨q read as implications.
</p>
</span>
")

(deftest "a block's :let takes Babel's VAR=VALUE pairs too, up to the next :KEY; later values see earlier ones" [switches]
  (let ((html (org-export-string "
#+begin_box Exercise :let N=6 WHO=\"Jasim\" ANSWER=(* N 7) :folded \"Peek\"
WHO says N × 7 is ANSWER.
#+end_box")))
    (should (string-match-p "Jasim says 6 × 7 is 42." html))
    (should (string-match-p "Peek" html))))

(deftest "a heading's :let takes VAR=VALUE pairs too, as its drawer's string" [switches sections]
  (should (string-match-p "p ⇒ q, 3 times" (org-export-string "
* Logic
:PROPERTIES:
:let: ->=\"⇒\" N=3
:END:
p -> q, N times"))))

(deftest "a :let of VAR=VALUE pairs lacking a value is an error naming the switch" [switches]
  (should-error (org--block-let-pairs "N=3 WHO=") :type 'user-error)
  (should-error (org--block-let-pairs "N=3 stray") :type 'user-error))

(deftest "a block's :let NAME reads only a table's first two columns, so a third may hold notes" [switches]
  (should (string-match-p "p ⇒ q" (org-export-string "
#+begin_red :let logic
p -> q
#+end_red

#+name: logic
| Variable | Value | Note        |
|----------+-------+-------------|
| ->       | ⇒     | implication |"))))

(define-org-export-test "a block's :let NAME takes its variables from the table #+name: NAME, sans the header above its rule; even a :noexport: one" [switches]
  :input "
          #+begin_red :let logic
          p -> q == ¬p ∨ q
          #+end_red

          #+begin_blue :let logic
          q -> q == true
          #+end_blue

          * Shorthands, shared by the blocks above :noexport:
          #+name: logic
          | Variable | Value |
          |----------+-------|
          | ->       | ⇒     |
          | ==       | ≡     |"
  :equals "<span style=\"color:red;\">
<p>
p ⇒ q ≡ ¬p ∨ q
</p>
</span>

<span style=\"color:blue;\">
<p>
q ⇒ q ≡ true
</p>
</span>
")

(defvar org-block-switches-tests--logic '(-> "⇒")
  "Shorthands that every document may share, by :let org-block-switches-tests--logic.")

(define-org-export-test "a block's :let NAME, for a NAME that is no table, takes its variables from the Lisp variable NAME" [switches]
  :input "
          #+begin_red :let org-block-switches-tests--logic
          p -> q
          #+end_red"
  :equals "<span style=\"color:red;\">
<p>
p ⇒ q
</p>
</span>
")

(define-org-export-test "a block marked :boxed TITLE and :folded t is a box, folded away under the block's name" [switches]
  :input "
          #+begin_red :boxed \"Heads up\" :folded t
          Mind the gap.
          #+end_red"
  :equals "<details class=\"code-details\" style=\"padding: 1em; background-color: #e5f5e5; border-radius: 15px;
color: hsl(157 75% 20%); font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">
<summary><strong><font face=\"Courier\" size=\"3\" color=\"green\">Red</font></strong></summary>
<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
<h3>Heads up</h3>
<span style=\"color:red;\">
<p>
Mind the gap.
</p>
</span>
</div>

</details>
")

(define-org-export-test "a block marked :boxed t is a tcolorbox in LaTeX" [switches]
  :input "
          #+begin_red :boxed t
          Mind the gap.
          #+end_red"
  :to latex
  :equals "\\colorlet{osbe-box-back}{red!5!white}\\colorlet{osbe-box-frame}{red!75!black}
\\colorlet{osbe-box-title}{yellow!50!red}\\begin{tcolorbox}[title={},colback=osbe-box-back,
colframe=osbe-box-frame,colbacktitle=osbe-box-title,coltitle=red!25!black, fonttitle=\\bfseries,
subtitle style={boxrule=0.4pt, colback=yellow!50!red!25!white}]
\\begingroup\\color{red}
Mind the gap.
\\endgroup\\,
\\end{tcolorbox}
")

(define-org-export-test "a block that declares a keyword of a switch's name receives it, rather than the switch" [switches]
  :input "
          #+begin_drawer :folded closed
          Socks.
          #+end_drawer"
  :using (org-defblock drawer (&key folded &context contents) (format "Drawer is %s: %s" folded contents))
  :equals "Drawer is closed:
           <p>
           Socks.
           </p>
           ")

(define-org-export-test "switches are welcome even on a block that refuses unknown keywords" [switches]
  :input "
          #+begin_strict :boxed Strictly
          Rules.
          #+end_strict"
  :using (org-defblock strict (&key color &context contents) (format "STRICT %s" contents))
  :equals "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
<h3>Strictly</h3>
STRICT
<p>
Rules.
</p>

</div>
")

;;; Evaluating a switch's Lisp

(deftest "with `org-defblock-evaluate-switches' nil, a block's :when is an error rather than evaluated" [switches]
  (let ((org-defblock-evaluate-switches nil))
    (should-error (org-export-string "
#+begin_red :when t
Anything.
#+end_red")
                  :type 'user-error)))

(deftest "with `org-defblock-evaluate-switches' as query, each distinct form is asked about once" [switches]
  (let ((org-defblock-evaluate-switches 'query)
        (org--block-approved-forms nil)
        (asked 0))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (cl-incf asked) t)))
      (org-export-string "
#+begin_red :when (eq backend 'html)
One.
#+end_red
#+begin_blue :when (eq backend 'html)
Two.
#+end_blue")
      (should (equal asked 1)))))

;;; C-c C-c

(defvar org--block-cc-switch-test-result nil
  "Set by the :C-c_C-c form of a test's block.")

(org-deftest "C-c C-c on a block with a :C-c_C-c form evaluates it" [switches C-c_C-c]
  :text "
#+begin_quiz :C-c_C-c (setq org--block-cc-switch-test-result \"Score: 3/4\")
What is 6 × 7?
#+end_quiz"
  :at "What"
  (let ((org--block-cc-switch-test-result nil))
    (should (org--block-cc-dispatcher))
    (should (equal org--block-cc-switch-test-result "Score: 3/4"))))

(org-deftest "C-c C-c on a block whose :C-c_C-c names a function calls it" [switches C-c_C-c]
  :text "
#+begin_quiz :C-c_C-c org--block-cc-switch-test-check
What is 6 × 7?
#+end_quiz"
  :at "What"
  (let ((called nil))
    (cl-letf (((symbol-function 'org--block-cc-switch-test-check) (lambda () (setq called t))))
      (org--block-cc-dispatcher)
      (should called))))

(org-deftest "C-c C-c on a block with a registered handler runs the handler, not its :C-c_C-c switch" [switches C-c_C-c]
  :text "
#+begin_quiz :C-c_C-c (error \"The switch should not run\")
What is 6 × 7?
#+end_quiz"
  :at "What"
  (let ((org--block-cc-handlers (list (cons "quiz" (lambda () 'handled)))))
    (should (eq (org--block-cc-dispatcher) 'handled))))

(org-deftest "C-c C-c on a block without a :C-c_C-c switch leaves the key to Org" [switches C-c_C-c]
  :text "
#+begin_quiz
What is 6 × 7?
#+end_quiz"
  :at "What"
  (should-not (org--block-cc-dispatcher)))

;;; Switches on any block, of ours or not

(define-org-export-test "switches work on a special block no org-defblock declares: :let, then :boxed" [switches any-block]
  :input "
          #+begin_aside Mind :let (N 3) :boxed \"Exercise\"
          Pick N.
          #+end_aside"
  :equals "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
<h3>Exercise</h3>
<div class=\"aside\" id=\"org-id-1\">
<p>
Pick 3.
</p>

</div>
</div>
")

(define-org-export-test "switches work on Org's own blocks: a quote :folded under a title" [switches any-block]
  :input "
          #+begin_quote :folded \"Wisdom\"
          Less is more.
          #+end_quote"
  :equals "<details class=\"code-details\" style=\"padding: 1em; background-color: #e5f5e5; border-radius: 15px;
color: hsl(157 75% 20%); font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\">
<summary><strong><font face=\"Courier\" size=\"3\" color=\"green\">Wisdom</font></strong></summary>
<blockquote>
<p>
Less is more.
</p>
</blockquote>

</details>
")

(define-org-export-test "switches work on src blocks: :boxed, the code still exported by Org" [switches any-block]
  :input "
          #+begin_src emacs-lisp :boxed t :exports code :copy nil
          (+ 1 2)
          #+end_src"
  :equals "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
<h3></h3>
<div class=\"org-src-container\">
<pre class=\"src src-emacs-lisp\"><code>(+ 1 2)
</code></pre>
</div>
</div>
")

(define-org-export-test "a special block no org-defblock declares exports to nothing when its :when form is nil" [switches any-block]
  :input "
          #+begin_aside :when (eq backend 'latex)
          Print only.
          #+end_aside"
  :equals "")

(deftest "`org--header-sans-switches' splits a header into its switches and the rest, values read as Lisp" [switches]
  (should (equal (org--header-sans-switches "emacs-lisp :folded \"See it\" :exports code" #'org--block-switch-p)
                 '((:folded "See it") . "emacs-lisp :exports code")))
  (should (equal (org--header-sans-switches "Title :when (eq backend 'html) :boxed :n 2" #'org--block-switch-p)
                 '((:when (eq backend 'html) :boxed t) . "Title :n 2"))))

(deftest "`org-defswitch' makes a switch every block understands, placed among the others by its :depth" [switches]
  (let ((org-block-switches org-block-switches))
    (org-defswitch :shout (_loud? &key render contents)
      "Upper-case the block's rendering, when LOUD?."
      (upcase (funcall render contents)))
    (should (string-match-p "HELLO" (org-export-string "#+begin_src text :shout t\nhello\n#+end_src")))
    ;; Within :let, at depth 0: it shouts the substitution; within :folded, too, which titles the details.
    (should (string-match-p "Peek.*</summary>\\(?:.\\|\n\\)*JASIM"
                            (org-export-string "#+begin_example :let (WHO \"jasim\") :folded \"Peek\" :shout t\nWHO\n#+end_example"))))
  (should-not (org--block-switch-p :shout)))

(deftest "`org-defswitch' refuses a key not on offer as the switch expands, rather than binding it to nil" [switches]
  (should (string-match-p ":contnets.*not on offer"
                          (cadr (should-error (macroexpand '(org-defswitch :typo (_ &key contnets) "Typo." contnets)))))))

(deftest "`org-defswitch' hands a switch only the keys it names; a `let' around it keeps its own `name'" [switches]
  (let ((org-block-switches org-block-switches)
        (name "Jasim"))
    (org-defswitch :greet (_ &key ((:name block)) render contents)
      "Greet NAME atop the block, naming it too."
      (format "Hi %s, from %s\n%s" name block (funcall render contents)))
    (should (string-match-p "Hi Jasim, from example"
                            (org-export-string "#+begin_example :greet t\nhello\n#+end_example")))))

(deftest "switches apply before Babel: :let changes the code a src block runs, and :noexport keeps it from running" [switches babel]
  (let ((org-confirm-babel-evaluate nil))
    (should (string-match-p "<pre class=\"example\">\\s-*9\\s-*</pre>" (org-export-string "
#+begin_src emacs-lisp :let (N 3) :exports results
(* N N)
#+end_src")))
    (should (equal "" (org-export-string "
#+begin_src emacs-lisp :noexport t :exports results
(error \"Never run\")
#+end_src")))))

;;; :verbatim

(deftest "a block marked :verbatim t is shown as the Org source it is, sans its switches" [switches verbatim]
  ;; Sans htmlize, whose colors vary by machine.
  (let ((org-html-htmlize-output-type nil))
    (should (equal (org-export-string "
#+begin_tip Mind :verbatim t :folded nil
Hi *there*.
#+end_tip")
                   "<div class=\"org-src-container\">
<pre class=\"src src-org\"><code>#+begin_tip Mind
Hi *there*.
#+end_tip
</code></pre>
</div>
"))))

;;; Switches on sections, by property drawer

(deftest "a heading's property value reads as Lisp when wholly Lisp, else stays a string" [switches sections]
  (should (equal (org--heading-switch-value " (eq backend 'html) ") '(eq backend 'html)))
  (should (equal (org--heading-switch-value "") t))
  (should (equal (org--heading-switch-value "t") t))
  (should (equal (org--heading-switch-value "Peek at it") "Peek at it")))

(deftest "a heading whose :when form is nil axes its whole subtree, heading and subsections included" [switches sections]
  (let ((html (org-export-string "
* Kept
Before.
* Print only
:PROPERTIES:
:when: (eq backend 'latex)
:END:
Only in print.
** Deeper
Also only in print.
* After
Still here.")))
    (should (string-match-p "Before" html))
    (should (string-match-p "Still here" html))
    (should-not (string-match-p "Print only\\|Only in print\\|Deeper" html))))

(deftest "a heading whose :when form is non-nil exports as usual; the form sees `backend', and the key's case is moot" [switches sections]
  (let ((html (org-export-string "
* Web only
:PROPERTIES:
:WHEN: (string= backend \"html\")
:END:
Only online.")))
    (should (string-match-p "Web only" html))
    (should (string-match-p "Only online" html))))

(deftest "a heading marked :noexport t axes its subtree" [switches sections]
  (should-not (string-match-p "draft" (org-export-string "
* Notes to self
:PROPERTIES:
:noexport: t
:END:
A draft thought."))))

(deftest "a heading's :let substitutes throughout its subtree: its text, its blocks, and its subsections" [switches sections]
  (let ((html (org-export-string "
* Logic
:PROPERTIES:
:let: (-> \"⇒\")
:END:
p -> q
#+begin_red
q -> r
#+end_red
** Transitivity
p -> r")))
    (should (string-match-p "p ⇒ q" html))
    (should (string-match-p "q ⇒ r" html))
    (should (string-match-p "p ⇒ r" html))
    (should-not (string-match-p "->" html))))

(deftest "a heading's :folded folds its section away, titled by the value, or by the heading for t" [switches sections]
  (let ((html (org-export-string "
* Answer
:PROPERTIES:
:folded: Peek at it
:END:
It is 42.")))
    (should (string-match-p "<h2[^>]*>.*Answer</h2>" html))
    (should (string-match-p "<details.*\n.*Peek at it" html))
    (should (string-match-p "It is 42.\n</p>\n\n</details>" html)))
  (should (string-match-p "<summary>.*Answer.*</summary>" (org-export-string "
* Answer
:PROPERTIES:
:folded: t
:END:
It is 42."))))

(deftest "a heading's :boxed and :folded refuse its subsections, which no block can hold" [switches sections]
  (should-error (org-export-string "
* Answer
:PROPERTIES:
:boxed: t
:END:
It is 42.
** Why
Douglas Adams.")
                :type 'user-error))

(deftest "a heading's :verbatim shows its section as written, heading and subsections included, sans its switches" [switches sections]
  (let ((org-html-htmlize-output-type nil))
    (should (equal (org-export-string "
#+PROPERTY: header-args:src :copy nil
* Answer
:PROPERTIES:
:verbatim: t
:END:
It is *42*.
** Why
Douglas Adams.")
                   "<div class=\"org-src-container\">
<pre class=\"src src-org\"><code>* Answer
It is *42*.
** Why
Douglas Adams.
</code></pre>
</div>
"))))

(deftest "a heading's :verbatim and :folded fold away its source, subsections and all, titled by the heading" [switches sections]
  (let ((html (org-export-string "
* Answer
:PROPERTIES:
:verbatim: t
:folded: t
:END:
It is 42.
** Why
Douglas Adams.")))
    (should (string-match-p "<summary>.*Answer.*</summary>" html))
    (should (string-match-p "\\*\\* Why" html))))

(deftest "a heading's :ignore bypasses the section's own behaviour: its tags that an org-deftag defines" [switches sections]
  (should (string-match-p "<details" (org-export-string "
* Stuck? :details_Hint:
Try induction.")))
  (let ((html (org-export-string "
* Stuck? :details_Hint:
:PROPERTIES:
:ignore: t
:END:
Try induction.")))
    (should-not (string-match-p "<details\\|details_Hint" html))
    (should (string-match-p "Stuck?" html))
    (should (string-match-p "Try induction." html))))

(deftest "a heading's :backend keeps its subtree only along those backends; else axes it, heading included" [switches sections]
  (let ((section "
* Web only
:PROPERTIES:
:backend: html
:END:
Hover for more.
** Details
Tooltips.
* Everywhere
Prose."))
    (should (string-match-p "Hover for more.*\n\\(.*\n\\)*.*Tooltips" (org-export-string section)))
    (let ((latex (org-export-string section 'latex)))
      (should-not (string-match-p "Web only\\|Hover\\|Details\\|Tooltips" latex))
      (should (string-match-p "Everywhere" latex)))))

(deftest "a heading's :tags, which #+exclude_tags: names, axes its subtree; Org's own TAGS property never shadows it" [switches sections]
  (let ((html (org-export-string "
#+exclude_tags: solution
* Exercise :easy:
Show that 1 + 1 = 2.
* Answer :easy:
:PROPERTIES:
:tags: solution
:END:
By definition.")))
    (should (string-match-p "Show that" html))
    (should-not (string-match-p "Answer\\|By definition" html)))
  (let ((html (org-export-string "
* Answer :easy:
:PROPERTIES:
:tags: solution
:END:
By definition.")))
    (should (string-match-p "Answer.*easy" html))
    (should (string-match-p "By definition" html))))

(deftest "a heading's :within, :filename, and :copy wrap its text, as they would a block's" [switches sections]
  (let ((html (org-export-string "
* Aside
:PROPERTIES:
:within: (box \"Note\")
:END:
Easily missed.")))
    (should (string-match-p "<h3>Note</h3>[^<]*<p>\nEasily missed." html)))
  (let ((html (org-export-string "
* Setup
:PROPERTIES:
:filename: init.el
:copy: t
:END:
Paste this.")))
    (should (string-match-p "osbe-filename.*<code>init.el</code>" html))
    (should (string-match-p "osbe-copy-button" html))))

(deftest "adjacent headings' :tab make one tabset of their sections, as adjacent blocks' do" [switches sections]
  (let ((html (org-export-string "
* Install
:PROPERTIES:
:tab: MELPA
:END:
M-x package-install.
* Install by hand
:PROPERTIES:
:tab: Git
:END:
git clone.")))
    (should (string-match-p "osbe-tab" html))
    (should (string-match-p "MELPA" html))
    (should (string-match-p "Git" html))))

(deftest "a heading's :log logs its section's rendering, as a block's" [switches sections]
  (when (get-buffer "*Org Block Log*") (kill-buffer "*Org Block Log*"))
  (org-export-string "
* Answer
:PROPERTIES:
:log: t
:END:
It is *42*.")
  (should (string-match-p "It is <b>42</b>" (with-current-buffer "*Org Block Log*" (buffer-string)))))

(org-deftest "C-c C-c within a section runs its :C-c_C-c switch, or an ancestor's, where Org's own has nothing to do" [switches sections]
  :text "
* Quiz
:PROPERTIES:
:C-c_C-c: (setq org-block-switches-tests--graded t)
:END:
** Question
What is 1 + 1?"
  :at "What is"
  (dlet ((org-block-switches-tests--graded nil) (org-defblock-evaluate-switches t))
    (org-ctrl-c-ctrl-c)
    (should org-block-switches-tests--graded)))

(provide 'org-block-switches-tests)
;;; org-block-switches-tests.el ends here

;;; :backend, :tags, :within, :permalink

(define-org-export-test "a block's :backend names the backends it exports along; others get nothing" [switches backend]
  :input "
          #+begin_red :backend latex
          Only in print.
          #+end_red
          #+begin_blue :backend (html latex)
          Everywhere we care for.
          #+end_blue"
  :equals "<span style=\"color:blue;\">
<p>
Everywhere we care for.
</p>
</span>
")

(deftest "a block's :backend matches a backend that derives from it, so md is html, and asks nothing before it runs" [switches backend]
  (let ((org-defblock-evaluate-switches nil))
    (should (string-match-p "Web" (org-export-string "#+begin_red :backend html\nWeb\n#+end_red" 'md)))
    (should-not (string-match-p "Markdown" (org-export-string "#+begin_red :backend md\nMarkdown\n#+end_red" 'html)))))

(define-org-export-test "a block whose :tags meet #+exclude_tags is not exported, as a section so tagged is not" [switches tags]
  :input "
          #+exclude_tags: solution draft
          #+begin_box Exercise
          What is 6 × 7?
          #+end_box
          #+begin_box Answer :tags solution
          42
          #+end_box
          #+begin_red :tags (aside draft)
          Rethink this.
          #+end_red
          #+begin_blue :tags \"aside hint\"
          Think of a towel.
          #+end_blue"
  :equals "<div style=\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
<h3>Exercise</h3>
<p>
What is 6 × 7?
</p>
</div>


<span style=\"color:blue;\">
<p>
Think of a towel.
</p>
</span>
")

(deftest "sans #+exclude_tags, a block's :tags meet Org's `org-export-exclude-tags', noexport by default" [switches tags]
  (should-not (string-match-p "Hidden" (org-export-string "#+begin_red :tags noexport\nHidden\n#+end_red")))
  (should (string-match-p "Shown" (org-export-string "#+begin_red :tags solution\nShown\n#+end_red"))))

(deftest "`org--wrap-specs' reads one block, or a list of them, outermost first" [switches within]
  (should (equal (org--wrap-specs 'box) '((box))))
  (should (equal (org--wrap-specs '(box "T" :shadow t)) '((box "T" :shadow t))))
  (should (equal (org--wrap-specs '((details "Hint") box)) '((details "Hint") (box)))))

(deftest "a block's :within (details T) is its :folded T, and a list of blocks is its :folded and :boxed" [switches within]
  (let ((org-src-preserve-indentation t))
    (should (equal (org-export-string "#+begin_red :within (details \"Hint\")\nHi\n#+end_red")
                   (org-export-string "#+begin_red :folded \"Hint\"\nHi\n#+end_red")))
    (should (equal (org-export-string "#+begin_quote :within ((details \"Hint\") (box \"Lemma\"))\nHi\n#+end_quote")
                   (org-export-string "#+begin_quote :folded \"Hint\" :boxed \"Lemma\"\nHi\n#+end_quote")))))

(define-org-export-test "a block's :within passes each block its arguments; one no org-defblock declares is left to Org" [switches within]
  :input "
          #+begin_red :within ((box \"Aside\" :background-color pink) center)
          Hi
          #+end_red"
  :equals "<div style=\"padding: 1em;background-color: #FFCCFF;border-radius: 15px;font-size: 0.9em;\">
<h3>Aside</h3>
<div class=\"org-center\">
<span style=\"color:red;\">
<p>
Hi
</p>
</span>
</div>
</div>
")

(deftest "a block's :permalink ID puts <<ID>> atop it; for t, its #+name:, else its title; [[ID]] links to it" [switches permalink]
  (let ((html (org-export-string "#+begin_box Exercise :permalink t\nHi\n#+end_box\nSee [[Exercise]].")))
    (should (string-match-p "<a class=\"anchor\" id=\"Exercise\" href=\"#Exercise\"" html))
    (should (string-match-p "<a href=\"#Exercise\">" html)))
  (should (string-match-p "id=\"fact\" href=\"#fact\""
                          (org-export-string "#+name: fact\n#+begin_box Factorial :permalink t\nHi\n#+end_box")))
  (should (string-match-p "id=\"here\" href=\"#here\""
                          (org-export-string "#+begin_src emacs-lisp :permalink here\n(+ 1 2)\n#+end_src")))
  (should-error (org-export-string "#+begin_src emacs-lisp :permalink t\n(+ 1 2)\n#+end_src") :type 'user-error))

(deftest "a section's :permalink puts its 🔗 beside the heading, by CUSTOM_ID; the table of contents shows the title alone" [switches permalink]
  (let ((html (org-export-string "* Lemmas\n:PROPERTIES:\n:permalink: t\n:END:\nBody.")))
    (should (string-match-p "<h2 id=\"Lemmas\">.*Lemmas <a class=\"anchor\" href=\"#Lemmas\"" html))
    (should (string-match-p "<li><a href=\"#Lemmas\">1\\. Lemmas</a></li>" html))))

;;; Rewriting a block once, its #+name: with it

(deftest "a src block's inherited switch applies once, though switches are applied both before and after #+INCLUDE:" [switches]
  (should (equal 1 (s-count-matches "<h3>Hi</h3>"
                                    (org-export-string "* H
:PROPERTIES:
:header-args:emacs-lisp: :boxed Hi
:END:
#+begin_src emacs-lisp
(+ 1 2)
#+end_src")))))

(deftest "a rewritten src block keeps its #+name:, so Babel still finds it; :noexport takes the name along" [switches]
  (let ((org-confirm-babel-evaluate nil))
    (should (string-match-p "<pre class=\"src src-emacs-lisp\" id="
                            (org-export-string "#+name: three\n#+begin_src emacs-lisp :boxed t\n(+ 1 2)\n#+end_src")))
    (should (string-match-p "<pre class=\"example\"[^>]*>\n3\n</pre>"
                            (org-export-string "#+name: three\n#+begin_src emacs-lisp :permalink here :exports none\n(+ 1 2)\n#+end_src\n\n#+call: three()")))
    (should-not (string-match-p "id=\"lost\""
                                (org-export-string "#+name: lost\n#+begin_red :noexport t\nHi\n#+end_red\n\nNext.")))))

(deftest "a src block's language keeps its own header arguments: Babel's :wrap wraps its results, not the block" [switches within]
  (let ((org-confirm-babel-evaluate nil))
    (should (string-match-p "<div class=\"aside\"[^>]*>\n<p>\n3"
                            (org-export-string "#+begin_src emacs-lisp :wrap aside :exports results\n(+ 1 2)\n#+end_src")))
    (should (org--babel-declares-p "emacs-lisp" :wrap))
    (should-not (org--babel-declares-p "emacs-lisp" :folded))))
