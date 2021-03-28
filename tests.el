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

INPUT is a multi-line string; we enclose it in ‘indent’."
  (with-temp-buffer
    (insert (unindent input))
    (org-special-block-extras--support-special-blocks-with-args backend)
    (buffer-string)))

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

(when nil


(deftest "Constant blocks preserve indentation/enumeration"
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

) ;; when nil ... TODO: Fix these tests.

(when nil


(ert-deftest remark-blocks/1 ()
  (should (equal
    (⟰
     "#+begin_remark
       XXX
      #+end_remark")
    (unindent
      "#+begin_export html
      <p style=\"color: black;\"><span style=\"border-width:1px;border-style:solid;padding:5px\"><strong>[Editor Remark:</strong></span>
      #+end_export
      XXX

      #+begin_export html
        <span style=\"border-width:1px;border-style:solid;padding:5px\"><strong>]</strong></span></p>
      #+end_export"))))

(ert-deftest remark-blocks/2 ()
  (should (equal
    (⟰
     "#+begin_remark EDITOR
       XXX
      #+end_remark")
    (unindent
     "#+begin_export html
     <p style=\"color: black;\"><span style=\"border-width:1px;border-style:solid;padding:5px\"><strong>[EDITOR:</strong></span>
     #+end_export
     XXX

     #+begin_export html
       <span style=\"border-width:1px;border-style:solid;padding:5px\"><strong>]</strong></span></p>
     #+end_export"))))

)

(when nil

(ert-deftest details-blocks ()
  "Parallel blocks work as expected"
  (should
   (equal
    (⟰
       "#+begin_details TITLE-RIGHT-HERE
        X

        Y
        #+end_details")

"#+begin_export html
<details class=\"code-details\"
                 style =\"padding: 1em;
                          background-color: #e5f5e5;
                          /* background-color: pink; */
                          border-radius: 15px;
                          color: hsl(157 75%);
                          font-size: 0.9em;
                          box-shadow: 0.05em 0.1em 5px 0.01em  #00000057;\">
                  <summary>
                    <strong>
                      <font face=\"Courier\" size=\"3\" color=\"green\">
                         TITLE-RIGHT-HERE
                      </font>
                    </strong>
                  </summary>

#+end_export
X

Y

#+begin_export html

               </details>
#+end_export")))

)

(when nil


(ert-deftest box-blocks ()
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



)

(when nil

(ert-deftest parallel-blocks ()
  "Parallel blocks work as expected"
  (should
   (equal
    (unindent
      "#+begin_export html
      <div style=\"column-rule-style: solid;column-count: 2;\">
      #+end_export
      X

      @@html:<p><br>@@

      Y

      Z

      #+begin_export html
      </div>
      #+end_export")
    (with-temp-buffer
      (insert
       (unindent
       "#+begin_parallel 2 :bar yes-or-any-other-text
        X

        #+columnbreak:

        Y

        Z
        #+end_parallel"))
      (org-special-block-extras--support-special-blocks-with-args 'html)
      (buffer-string)))))

)

(when nil


(ert-deftest kbd-link ()
  (should (equal
    (org-export-string-as
     "kbd:C-u_80_-"
     'html
     :body-only)
    (unindent "<p>
              <kbd> C-u 80 </kbd>_-</p>
              "))))

)
