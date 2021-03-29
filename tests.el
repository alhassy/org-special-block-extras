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
  :expected-result :failed
  (should
   (equal
    (⟰
       "#+begin_box TITLE-RIGHT-HERE
        X

        Y
        #+end_box")
    "#+begin_export html
<div style=\"padding: 1em; background-color: #CCFFCC;border-radius: 15px; font-size: 0.9em; box-shadow: 0.05em 0.1em 5px 0.01em #00000057;\"><h3>TITLE-RIGHT-HERE</h3>
#+end_export
X

Y

#+begin_export html
</div>
#+end_export")))

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
