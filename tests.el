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

(deftest "Constant blocks export to LaTex preserves indentation/enumeration")
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
