(setq needed-libraries
      '(s cl-lib dash org seq quelpa))

(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg needed-libraries)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defmacro deftest (desc &rest body)
  `(ert-deftest ,(intern
;; Convert all non-letters to ‘_’
;; A = 65, z = 122
(concat (seq-map (lambda (c) (if (<= 65 c 122) c ?_))
         desc))) () ,@body))
;; without the s-replace, “M-x ert” crashes when it comes to selecting the test to run.


;; https://github.com/Wilfred/propcheck
(quelpa '(propcheck :fetcher github :repo "Wilfred/propcheck"))
(require 'propcheck)
(when nil ;; example use
  (let ((propcheck-seed (propcheck-seed)))
    (propcheck-generate-string nil)))


;; An attempt to make multiline strings less ugly
(require 's)
(defun unindent (s)
"Allow multiline strings, ignoring any initial indentation (as in Ruby).

The first line of S must be an empty line.

For instance,

(unindent \"
     Hello
       and then some\")

Returns the string:

Hello
  and then some

Notice that the initial indentation has been stripped uniformally
across all lines: The second line begins 2 characters indentated
from the first."
  (let ((indentation (length (car (s-match "\\( \\)+" (cadr (s-split "\n" s)))))))
    (s-chop-prefix "\n"
                   (replace-regexp-in-string (format "^ \\{%s\\}" indentation) "" s))))


(defalias '§ #'unindent)
(§ "
   Hello
       and then some")


(load-file "org-special-block-extras.el")
(org-special-block-extras-short-names)

(cl-defun ⟰  (input &optional (backend 'html))
  "Transform INPUT according to BACKEND for Org preprocessing.

INPUT is a multi-line string; we enclose it in ‘indent’.

Since org-special-block-extras--support-special-blocks-with-args
surrounds things in an export block, which is irrelevant for testing,
we strip that out."
  (s-chop-prefix
   "#+begin_export html \n"
   (s-chop-suffix
    "\n#+end_export"
    (with-temp-buffer
      (insert (unindent input))
      (org-special-block-extras--support-special-blocks-with-args backend)
      (buffer-string)))))

(deftest "pp-list works as desired"
  (should (equal "1 2 3 4 5"
                 (org-special-block-extras--pp-list '(1 2 3 4 5))))
  (should (equal "1"
                 (org-special-block-extras--pp-list '(1))))
  (should (equal ""
                 (org-special-block-extras--pp-list nil))))

;; Using propcheck, we run this test on /arbitrary/ buffer contents.
(deftest "No supported blocks means buffer is unchanged"
  :tags '(core)
  (let* (org-special-block-extras--supported-blocks
         (propcheck-seed (propcheck-seed))
         (buf (propcheck-generate-string nil)))
    (should (equal buf
                   (with-temp-buffer
                     (insert buf)
                     (org-special-block-extras--support-special-blocks-with-args 'html)
                     (buffer-string))))))

(deftest "Constant blocks preserve indentation/enumeration"
  :expected-result :failed
  :tags '(core)
  (defblock go nil nil "doc" "hello") ;; Constantly “hello”
  (should (equal
"
  1. item one
  2. item two
     #+begin_export html
hello
#+end_export
  3. item three"
    (with-temp-buffer
      (insert
  "
  1. item one
  2. item two
     #+begin_go
  world
  #+end_go
  3. item three")
      (org-special-block-extras--support-special-blocks-with-args 'html)
      (buffer-string)))))

(deftest "Constant blocks export to LaTex preserves indentation/enumeration"
(should (equal
"\\begin{enumerate}
\\item item one
\\item item two
hello
\\item item three
\\end{enumerate}
"
(org-export-string-as
  "
  1. item one
  2. item two
     #+begin_go
  world
  #+end_go
  3. item three"
  'latex :body-only-please))))


(deftest "Constant blocks export to HTML preserves indentation/enumeration"
(should (equal
"<ol class=\"org-ol\">
<li>item one</li>
<li><p>
item two
</p>
hello</li>
<li>item three</li>
</ol>
"
(org-export-string-as
  "
  1. item one
  2. item two
     #+begin_go
  world
  #+end_go
  3. item three"
  'html :body-only-please))))


(deftest "Identity blocks preserve indentation/enumeration"
    :expected-result :failed
  :tags '(core)
  (defblock id nil nil "doc" contents)
  (should (equal
"
  1. item one
  2. item two
     #+begin_export html

     #+end_export
     world

     #+begin_export html

     #+end_export
  3. item three"
    (with-temp-buffer
      (insert
  "
  1. item one
  2. item two
     #+begin_id
  world
  #+end_id
  3. item three")
      (org-special-block-extras--support-special-blocks-with-args 'html)
      (buffer-string)))))

(deftest "Identity blocks export to LaTex preserves indentation/enumeration"
    :expected-result :failed
(defblock id nil nil "doc" contents)
(should (equal
"\\begin{enumerate}
\\item item one
\\item item two

world
\\item item three
\\end{enumerate}
"
(org-export-string-as
  "
  1. item one
  2. item two
     #+begin_id
  world
  #+end_id
  3. item three"
 'latex :body-only-please))))

(deftest "Identity blocks export to HTML preserves indentation/enumeration"
    :expected-result :failed
(defblock id nil nil "doc" contents)
(should (equal

"<ol class=\"org-ol\">
<li>item one</li>
<li><p>
item two
</p>

<p>
world
</p></li>
<li>item three</li>
</ol>
"
(org-export-string-as
  "
  1. item one
  2. item two
     #+begin_id
  world
  #+end_id
  3. item three"
 'html :body-only-please))))

(ert-deftest remark-blocks-work ()
  (-let [rmrk (⟰ "#+begin_remark
                   Here is some meta-commentary...
                  #+end_remark")]

    ;; The user's remark is enclosed in the default starting signal.
    (should (s-matches? (rx (seq (* anything) "[Editor Remark:"
                                 (* anything) "Here is some meta-commentary..."
                                 (* anything)  "]"))
                        rmrk))))
;; The other features of remark blocks should be tested;
;; but this is not a pressing, nor interesting, concern.

(ert-deftest details-blocks ()
  (-let [deets (⟰ "#+begin_details TITLE-RIGHT-HERE
                   My aside...
                   #+end_details")]

    ;; The result is a <details> tag containing the user's title & text.
    (s-matches? (rx (seq "<details"
                         (* anything)
                         "TITLE-RIGHT-HERE"
                         (* anything)
                         "My aside..."
                         (* anything)
                         "</details>"))
                deets)))

(ert-deftest box-blocks ()
  (-let [box (⟰ "#+begin_box Pay Attention!
                 This is the key insight...
                 #+end_box")]

    ;; We have an HTML box enclosing the user's title (in <h3) and text
    (s-matches? (rx (seq "<div style=\"padding: 1em; background-color: #CCFFCC;border-radius: 15px;"
                         (* anything)
                         "<h3>Pay Attention!</h3>"
                         (* anything)
                         "This is the key insight..."
                         (* anything)))
                box)))

(ert-deftest parallel-blocks ()
  "Parallel blocks work as expected"
  (-let [par  (⟰ "#+begin_parallel 2 :bar yes-or-any-other-text
                  X

                  #+columnbreak:

                  Y

                  Z
                  #+end_parallel")]

    ;; The result is 2 columns with a solid rule between them
    ;; and it contains the user's text along with the “#+columnbreak”.
    (s-matches? (rx (seq "<div style=\"column-rule-style: solid;column-count: 2;\">"
                         (* anything)
                         "X"
                         (* anything)
                         "<p><br>" ;; “#+columnbreak” above
                         (* anything)
                         "Y"
                         (* anything)
                         "Z"
                         (* anything)))
                par)))

(ert-deftest red-colors-block-work ()
  (-let [red-text (⟰ "#+begin_red
                      My cool thoughts...
                      #+end_red")]

    ;; We have an HTML span styled red that contains the user's text
    (should (s-matches? (rx (seq "<span style=\"color:red;\">"
                                 (* anything)
                                 "My cool thoughts..."
                                 (* anything)
                                 "</span>")) red-text))))

(ert-deftest color-blocks-work ()
  (-let [pink-txt (⟰ "#+begin_color pink
                      My cool thoughts...
                      #+end_color")]

    ;; We have an HTML span styled with the user's color and it contains the user's text
    (should (s-matches? (rx (seq "<span style=\"color:pink;\">"
                                 (* anything)
                                 "My cool thoughts..."
                                 (* anything)
                                 "</span>")) pink-txt))))

(ert-deftest kbd-link ()
  (should (equal
    (org-export-string-as
     "kbd:C-u_80_-"
     'html
     :body-only)
    (unindent "<p>
              <kbd> C-u 80 </kbd>_-</p>
              "))))

(ert-deftest link-here-works ()
  (-let [link-here
         (org-export-string-as
          "link-here:example-location (Click the icon and see the URL has changed!)"
          'html
          :body-only)]
    ;; We have an anchor with the given it, and the default SVG chain icon.
    (should
     (s-matches? (rx (* anything)
                     "<a class=\"anchor\" aria-hidden=\"true\" id=\"example-location\" href=\"#example-location\"><svg"
                     (* anything)
                     "</svg></a> (Click the icon and see the URL has changed!)"
                     (* anything))
                 link-here))))

;; The following tests only check that numerous usage styles work, but otherwise are uninsightful.

(ert-deftest badges/lmgtfy/1 ()
  (should (equal
     (org-export-string-as
  "badge:Let_me_google_that|for_you!|orange|https://lmgtfy.app/?q=badge+shields.io&iie=1|Elixir"
  'html :body-only)
     "<p>
<a href=\"https://lmgtfy.app/?q=badge+shields.io&iie=1\"><img src=\"https://img.shields.io/badge/Let_me_google_that-for_you%21-orange?logo=Elixir\"></a></p>
")))

(ert-deftest badges/lmgtfy/2 ()
  (should (equal
     (org-export-string-as
  "[[badge: Let me google that | for you! | orange |
   https://lmgtfy.app/?q=badge+shields.io&iie=1|Elixir]]"
  'html :body-only)
"<p>
<a href=\" https://lmgtfy.app/?q=badge+shields.io&iie=1\"><img src=\"https://img.shields.io/badge/%20Let%20me%20google%20that%20-%20for%20you%21%20- orange ?logo=Elixir\"></a></p>
")))

(ert-deftest badges/lmgtfy/3 ()
  (should (equal
     (org-export-string-as
      "badge:Let_me_*not*_google_that|for_you"
      'html :body-only)
"<p>
<img src=\"https://img.shields.io/badge/Let_me_%2Anot%2A_google_that-for_you-nil?logo=nil\"></p>
")))

(ert-deftest badges/with-all-options ()
  (should (equal
     (org-export-string-as
      "badge:key|value|informational|here|Elixir"
      'html :body-only)
"<p>
<a id=\"key\" href=\"#key\"><img src=\"https://img.shields.io/badge/key-value-informational?logo=Elixir\"></a></p>
")))


(ert-deftest badges/with-spaces-and-% ()
  (should (equal
     (org-export-string-as
      "badge:example_with_spaces,_-,_and_%|points_right_here|orange|here"
      'html :body-only)
"<p>
<a id=\"example_with_spaces,_-,_and_%\" href=\"#example_with_spaces,_-,_and_%\"><img src=\"https://img.shields.io/badge/example_with_spaces%2C_--%2C_and_%25-points_right_here-orange?logo=nil\"></a></p>
")))


(ert-deftest badges/no-color-given ()
  (should (equal
     (org-export-string-as
      "badge:key|value"
      'html :body-only)
"<p>
<img src=\"https://img.shields.io/badge/key-value-nil?logo=nil\"></p>
")))

(ert-deftest badges/empty-value ()
  (should (equal
     (org-export-string-as
      "badge:key"
      'html :body-only)
"<p>
<img src=\"https://img.shields.io/badge/key--nil?logo=nil\"></p>
")))

(ert-deftest badges/empty-key-and-empty-value ()
  (should (equal
     (org-export-string-as
      "badge:||green"
      'html :body-only)
"<p>
<img src=\"https://img.shields.io/badge/--green?logo=nil\"></p>
")))

(ert-deftest badges/just-value ()
  (should (equal
     (org-export-string-as
      "badge:|value"
      'html :body-only)
"<p>
<img src=\"https://img.shields.io/badge/-value-nil?logo=nil\"></p>
")))

(ert-deftest badges/totally-empty ()
  (should (equal
     (org-export-string-as
      "[[badge:]]"
      'html :body-only)
"<p>
<img src=\"https://img.shields.io/badge/--nil?logo=nil\"></p>
")))

; (ert "badges/*")

(ert-deftest docs/lisp ()
    (-let [apply-docs (org-export-string-as
                       "doc:apply"
                       'html :body-only-please)]
      ;; We get a tooltip whose title is the Lisp docs of APPLY.
      (should (s-matches? (rx (* anything)
                              "<abbr class=\"tooltip\" title=\""
                              (literal (s-replace "\n" "<br>" (documentation #'apply)))
                              "\">apply</abbr>"
                              (* anything))
                          apply-docs))))


(deftest "docs/existential-angst"
  (-let [ex (s-collapse-whitespace
             (with-temp-buffer
               (org-mode)
               (load-file "./org-special-block-extras.el")
               (org-special-block-extras-mode)
               (insert (unindent
                      "#+begin_documentation Existential Angst :label \"ex-angst\"
                       A negative feeling arising from freedom and responsibility.

                       Also known as
                       1. /Existential Dread/, and
                       2. *Existential Anxiety*.

                       Perhaps a distraction, such as [[https://www.w3schools.com][visiting W3Schools]], may help ;-)

                       Then again, ~coding~ can be frustrating at times, maybe have
                       a slice of pie with maths by reading “$e^{i×π} + 1 = 0$” as a poem ;-)
                       #+end_documentation

                       We now use this as doc:ex-angst."))
               (org-export-as 'html nil nil :body-only-please)))]

    ;; The #+begin_documentation produces a new osbe--docs entry
    (should (org-special-block-extras--name&doc "ex-angst"))

    ;; Upon export, the #+begin_documentation is /not/ present.
    ;; We have the text outside that block only.
    ;; Along, with a htmlified tooltip of the new entry.
    (s-matches? (rx " <p> We now use this as <abbr class=\"tooltip\" title=\""
                    (literal (org-special-block-extras--poor-mans-html-org-export
                              (cadr (org-special-block-extras--name&doc "ex-angst"))))
                    "\">Existential Angst</abbr>.</p> "
                    )
                ex)))

(ert-deftest margin-links-work ()
  (should (equal
           (s-collapse-whitespace
            (org-export-string-as
             (unindent
              "/Allah[[margin:][The God of Abraham; known as Elohim
               in the Bible]] does not burden a soul beyond what it can bear./
               --- Quran 2:286")
             'html
             :body-only))
           (s-collapse-whitespace "<p> <i>Allah<abbr
           class=\"tooltip\" title=\"The God of Abraham; known as
           Elohim<br>in the Bible\">°</abbr>&emsp13; does not
           burden a soul beyond what it can bear.</i> &#x2014;
           Quran 2:286</p> "))))

(deftest "Calculation blocks work"
  (-let [calc (⟰ "#+begin_calc :hint-format \"\\left\\{ %s\\right.\"
                  +     x
                  +     y -- Explanation of why $x \;=\; y$
                    Actually, let me explain:
                    * x
                    * x′ -- hint 1
                    * y  -- hint 2

                    No words can appear (in the export) *after* a nested calculation, for now.
                  + [≤] z
                    --
                    Explanation of why $y \;\leq\; z$

                    -- explain it more, this is ignored from export ;-)
                  #+end_calc")]

    ;; It's an align environment enclosed in $$ brackets.
    (should
     (s-matches? (rx (seq "$$\\begin{align*}" (* anything) "\\end{align*}$$"))
                 calc))

    ;; The calculation has 4 proof steps.
    ;;
    ;; The number of steps in a calculation is the number of items in each nesting, minus 1 (at each nesting).
    ;; Above we have depth 2 with 3 items in each depth, for a total of (3-1) + (3-1) = 2 + 2 = 4.
    (should (= 4 (s-count-matches (rx (seq "\\" (* whitespace) (any "=" "≤"))) calc)))

    ;; Of our 4 steps, 3 of them are equalities and one is an inclusion.
    (should (= 3 (s-count-matches (rx "= \\;\\;") calc)))
    (should (= 1 (s-count-matches (rx "≤ \\;\\;") calc)))

    ;; All of the hints actually appear in the calculational proof
    (mapc (lambda (hint) (should (s-contains? hint calc)))
          '("Explanation of why $x \;=\; y$"
            "Actually, let me explain:"
            "hint 1"
            "hint 2"
            "Explanation of why $y \;\leq\; z$"))))
