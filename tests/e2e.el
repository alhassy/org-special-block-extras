;;; e2e.el --- Simple End-to-End Testing for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (c) 2025 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1") (lf "1.0") (dad-joke "1.4") (seq "2.0") (lolcat "0"))
;; Keywords: end-to-end, testing, yaml, org-mode
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a streamlined approach to writing and updating
;; end-to-end tests using YAML as a data-driven format. Each test specifies
;; input data and expected output, making it easy to maintain and review.
;;
;; Workflow:
;; 1. Write YAML test files specifying `input' (with no `expectations').
;; 2. Run `M-x e2e-update-tests` (or `e2e-update-this-test`) to generate
;;    the `expectations' based on current implementation.
;; 3. Run `M-x e2e-run-tests` to verify tests pass.
;; 4. Review changes via Magit and commit:
;;    M-x magit, to see how the tests changed, commit if happy.
;; 5. Optionally, (ert-delete-all-tests)
;;
;; Tests are easy to maintain and update, encouraging comprehensive test coverage.
;;
;;
;; [Tell me more]
;; “end to end” tests are data driven tests (e.g., using JSON or YAML) that make API calls
;; and verify the responses. One writes a test's input, then updates its expectations using
;; the provided M-xe2e-update-tests method, then verifies the changes are acceptable in Magit.
;;
;; Read a YAML file that defines `input' and `expectations'.
;; Run `compute-values' on `input' and check the result is the same as `expectations'
;; Recursively read all YAML files in a specified directory.
;;
;;
;; Why?
;;
;; I think some of the unit tests I've written capture important
;; facts of the blocks I've defined, however the downside of
;; writing them is that they require thought.
;;
;; For example, nearly all of my tests are against the HTML
;; backend, even though org-special-block-extras is about
;; supporting multiple backends. Moreover, escaped strings aren't pretty.
;; Finally, since writing unit tests has friction, I don't even test
;; all options/keywords of my blocks.
;;
;; As such, I'd like to expedite the test writing process, and e2e.el
;; allows me to capture a snapshot of what a block produces and
;; I can then decide if a future change impacts the snapshot in
;; an acceptable fashion or not.
;;
;; When tests are easy to write, I'm more likely to write more of them.

;;; Code:

(use-package yaml)
(use-package yaml-mode)
(use-package ert)

(defun e2e--read-yaml-file (file)
  "Read and parse a YAML FILE, returning its contents as a hash table."
  (with-temp-buffer
    (insert-file-contents file)
    (yaml-parse-string (buffer-string))))

(ert-deftest e2e--read-yaml-file/test ()
  "Test reading a simple YAML file into a hash table."
  (let ((tmpfile (make-temp-file "e2e-test-" nil ".yaml" "key: value")))
    (unwind-protect
        (let ((hash (e2e--read-yaml-file tmpfile)))
          (should (equal (gethash 'key hash) "value")))
      (delete-file tmpfile))))


(defun e2e-run-this-test ()
  "Run the ERT test corresponding to the current YAML file."
  (interactive)
  (-let [file (f-relative (buffer-file-name))]
    (e2e--make-ert-test-for-yaml-file file)
    (ert (concat "e2e/" (file-name-base file)))))

(defun e2e--make-ert-test-for-yaml-file (file)
  "ERT-compatible test for a YAML FILE based on 'input' and 'expectations'."
  ;; Create a separate ert test for each YAML file
  (let* ((data (e2e--read-yaml-file file))
         (input (gethash 'input data))
         (no-prettier (gethash 'no-prettier data))
         (expected-results (gethash 'expectations data))
         (actual-results (compute-values input no-prettier)))

    (eval `(ert-deftest ,(intern (format "e2e/%s" (file-name-base file))) ()
	         :expected-result ,(if (map-elt data 'fails) :failed :passed) 
             ;; Iterate over each key in expected-results and create assertions
             (map-every-p (lambda (key expected-value)
                            (let ((actual-value (map-elt ,actual-results key 'not-found)))

                              (ert-info (`(lambda ()
                                            (when ,(and (stringp actual-value) (stringp expected-value))
                                              (insert-button "Show Diff"
                                                             'action (lambda (button)
                                                                       (e2e--show-string-diff ,expected-value ,actual-value))
                                                             'follow-link t
                                                             'help-echo "Click to see the diff."))
                                            (format " Test failed in file “%s” for key “%s”" ,,file (quote ,key))))
                                (should (equal actual-value expected-value)))))
                          ,expected-results)
	         
	         ;; TODO: Also assert that we can create a standalone PDF, unless there's a latex-backend-not-maintained key.
	         (when nil ;; unless (map-elt data 'latex-backend-not-maintained)
	           (let* ((file.tex (concat (f-base file) ".tex"))
		              (required-latex-imports (map-elt data 'required-latex-imports))
		              (latex (map-elt (map-elt data 'expectations) 'latex)))
		         (with-temp-file file.tex
		           (insert
		            (format "\\documentclass{standalone} %s \\begin{document} %s \\end{document}"
			                (or required-latex-imports "")
			                latex)))
		         (cl-assert  (s-contains-p (format "Output written on %s.pdf" (f-base file))
				                           (shell-command-to-string (format "pdflatex -shell-escape -halt-on-error %s; rm -f %s" file.tex file.tex)))))	     
	           )))))



(defun e2e--show-string-diff (string1 string2)
  "Show the difference between STRING1 and STRING2 using `diff` in Emacs."
  (interactive "sEnter first string: \nsEnter second string: ")
  (let ((temp-buffer1 (get-buffer-create "*Diff String 1*"))
        (temp-buffer2 (get-buffer-create "*Diff String 2*")))
    ;; Fill the first buffer with string1
    (with-current-buffer temp-buffer1
      (erase-buffer)
      (insert string1))
    ;; Fill the second buffer with string2
    (with-current-buffer temp-buffer2
      (erase-buffer)
      (insert string2))
    ;; Call the diff command on the two buffers
    (diff temp-buffer1 temp-buffer2)))


;; ( e2e--show-string-diff "hello" "hilla")

(cl-defun e2e-run-tests (&optional (directory "."))
  "Recursively define and run E2E tests for all .yaml files in DIRECTORY."  
  (interactive)
  (cl-loop for file in (directory-files-recursively directory "\\.yaml\\'")
           do (e2e--make-ert-test-for-yaml-file file))
  ;; Run all tests matching regex
  (ert "e2e/.*"))

(cl-defun e2e-update-tests ()
  "Update all YAML files in the current directory using their current input."  
  (interactive)
  ;; update all tests
  (cl-loop for file in (directory-files-recursively "." "\\.yaml\\'")
	       do (e2e-update-test file)))

(cl-defun e2e-update-this-test ()
  "Update the FILE by computing fresh `expectations` for its `input`."  
  (interactive)
  (e2e-update-test (buffer-file-name)))


(cl-defun e2e-update-test (file)
  (let* ((yaml (e2e--read-yaml-file file))
         (input (map-elt yaml 'input))
         (no-prettier (map-elt yaml 'no-prettier))
         ;; Creating an alist and not a hash-table so that ordering matters, for yaml encoding
         (actual (-concat (list (cons 'input input))
 			              (map-remove (lambda (k v) (member k '(input expectations))) yaml)
			              (list (cons 'expectations (map-into (compute-values input no-prettier) 'alist))))))
    (with-temp-file file
      ;; (insert (yaml-encode actual)) ;; Nope, it does not honour new lines
      (insert (my/prettify 'yaml (e2e--yaml-encode-alist actual))))))



;;
(defun my/prettify (language snippet)
  "Using this instead of `formal-all' so I can get formatting even if errors are present."
  (if (equal language 'latex)
      (shell-command-to-string (format "latexindent <<EOF\n%s\nEOF" snippet))
    (if (equal language 'html) ;; brew install tidy-html5
        (shell-command-to-string (format "tidy -quiet -indent --wrap 80 --show-warnings no  --show-body-only yes --indent-attributes yes --vertical-space yes --sort-attributes alpha<<EOF\n%s\nEOF" snippet))
      (shell-command-to-string (format "npx prettier --parser %s <<EOF\n%s\nEOF" language snippet)))))
;;
(my/prettify 'html "<html><body><div><h1>Hi</h1><p>This is ugly <b>HTML</b></p><ul><li>One<li>Two</ul></div></body></html>")
;; ⇒
"<html>
  <body>
    <div>
      <h1>Hi</h1>
      <p>This is ugly <b>HTML</b></p>
      <ul>
        <li>One</li>
        <li>Two</li>
      </ul>
    </div>
  </body>
</html>
"
;;
(when nil How to use prettier cli with snippet via here-docs:

      npx prettier --parser typescript <<EOF
      function greet(name:string){console.log("Hi, " + name);}
      greet("Musa");
      EOF

      )
;;
;;
(my/prettify 'latex "
\\begin{enumerate}
   \\item f
\\item g
     \\item h
  \\end{enumerate}")
;; ⇒
"
\\begin{enumerate}
	\\item f
	\\item g
	\\item h
\\end{enumerate}
"
;;
;; brew install tidy-html5
(should (equal (my/prettify 'html "    <p>It can be useful to draw attention to some important text by enclosing it in
    a <abbr class=\"tooltip\" title=
    \"&lt;br&gt;&lt;br&gt;(fn ARG0 ARG &amp;rest ARGS)\">box</abbr>.</p>
    <div style=
    \"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
      <h3>Uses of callout boxes</h3>
      <p>Such boxes often callout tips, warnings, cautionary info or emphasises
      core information.</p>
    </div>")
               "<p>It can be useful to draw attention to some important text by enclosing it in
a <abbr class=\"tooltip\"
      title=\"&lt;br&gt;&lt;br&gt;(fn ARG0 ARG &amp;rest ARGS)\">box</abbr>.</p>

<div style=
\"padding: 1em;background-color: #CCFFCC;border-radius: 15px;font-size: 0.9em;\">
  <h3>Uses of callout boxes</h3>

  <p>Such boxes often callout tips, warnings, cautionary info or emphasises
  core information.</p>
</div>
"))

(defun e2e--yaml-encode-alist (alist &optional indent-level)
  "Encode an ALIST as a YAML-like string with multiline support.
Newlines within values are formatted using `|-' style.
Ensures a blank line before the `expectations' key if present.
INDENT-LEVEL specifies the current indentation level, defaulting to 0."
  (let ((indent-level (or indent-level 0))
        (first-key t))  ;; Track if it’s the first key
    (mapconcat
     (lambda (pair)
       (let* ((key (car pair))
              (value (cdr pair))
              (indent (make-string (* 2 indent-level) ? ))
              (sub-indent (make-string (* 2 (1+ indent-level)) ? ))
              ;; Insert a blank line before `expectations`
              (separator (if (and (eq key 'expectations) (not first-key))
                             "\n\n" "\n")))
         (setq first-key nil)  ;; No longer the first key after first iteration
         (concat separator
                 indent (symbol-name key) ": "
                 (cond
                  ;; Multiline strings: use `|-` style with additional indentation
                  ((and (stringp value) (string-match-p "\n" value))
                   (concat "|-\n"
                           sub-indent
                           (replace-regexp-in-string "\n" (concat "\n" sub-indent) value)))
                  ;; Nested alists: recursively call e2e--yaml-encode-alist
                  ((and (listp value) (listp (car value)))
                   (concat "\n" (e2e--yaml-encode-alist value (1+ indent-level))))
                  ;; Other values: simply output the value
                  (t
                   (prin1-to-string value))))))
     alist
     "")))

(ert-deftest e2e--yaml-encode-alist/multiline ()
  "Multiline strings should be encoded using |- style."
  (let* ((alist '((input . "hello\nworld")))
         (yaml (e2e--yaml-encode-alist alist)))
    (should (string-match-p "input: |-\\(\n\\s-*hello\\)" yaml))))

(ert-deftest compute-values/html-latex ()
  "Ensure compute-values returns HTML and LaTeX outputs."
  (let ((result (compute-values "*Hi*" nil)))
    (should (gethash 'html result))
    (should (gethash 'latex result))))



(defun compute-values (input no-prettier)
  "Computes new output given INPUT; `no-prettier' means no prettier/auto-format of result is done."
  (defun hs-hide-all () t) ;; HACK. PROBLEM: “hiding all blocks”, is this hide-show-mode?
  (map-into
   ;; Temporarily redefine gensym, so tests are deterministic
   (let ((seed 0))
     (cl-letf (((symbol-function 'gensym)
                (lambda () (format "g%s" (cl-incf seed)))))
       (list
        (cons 'html
	          (with-temp-buffer
	            (org-special-block-extras-mode)
                (insert (org-export-string-as (format "\n%s\n" input) 'html :body-only-please))
                (setq _X (buffer-string))
	            (when nil unless no-prettier
		              (-let [format-all-formatters '(("HTML" prettier))]
		                ;; TODO: When all my osbe e2e tests pass, then I should remove this ignore-errors.
		                ;; If something doesn't format, then that means it's likely invalid and should error.
		                ;; ignore-errors for unclosed <p> tags and other silly html errors
		                (html-mode) (ignore-errors (format-all-buffer))))	   
                (if no-prettier  (s-trim (buffer-string)) (my/prettify 'html  (s-trim (buffer-string))))))
        ;; Try to export, if it fails then just get the err msg.
        (cons 'latex
	          (condition-case err
	              (with-temp-buffer
		            (org-special-block-extras-mode)
		            (insert (org-export-str
                             ing-as (format "\n%s\n" input) 'latex :body-only-please))
		            (when nil unless no-prettier
		                  (-let [format-all-formatters '(("LaTeX" latexindent))] ;; !! brew install latexindent
		                    (latex-mode) (format-all-buffer)))
                    (if no-prettier  (s-trim (buffer-string)) (my/prettify 'latex (s-trim (buffer-string)))))
	            (error (format "🚫 The LaTex backend is intentionally unmaintained.\n🫠 Whoops, there seems to be an error: \n %S" err)))))))
   'hash-table))

(defun my/hash-get-or-compute (hash key compute-fn)
  "Retrieve or compute the value for KEY in HASH, and store it if computed."
  (or (gethash key hash)
      (let ((value (funcall compute-fn)))
        (puthash key value hash)
        value)))

;; 😲
(setq osbe-example-cache (make-hash-table :test 'equal))
(defvar osbe-example-cache (make-hash-table :test 'equal)
  "Cache to avoid time re-reading yaml files!")
;; MA: Consider using a single Org file as the source of truth, instead of multiple YAML files.
;; E.g., in the main org file, have some sections tagged :E2E: and those will be /tested/ every-time
;; I produce a new export ---as such, I never need to “remember” to run tests. Whenever I export, tests are run.
;; For now, I could consider just adding a hook to run tests whenever I produce an export!
(org-defblock osbe-example (file)
              "Render the given FILE as both Org source and rendered HTML result.

The source is the `input' key; the target is the `expectations.html' key.

Workflow: Write the `input' in an Org buffer, and once the export is to
my liking, then move the `input' to the relevant yaml file."
              (my/hash-get-or-compute
               osbe-example-cache
               file
               (lambda ()
                 (let* ((yaml (e2e--read-yaml-file file))
                        (input (map-elt yaml 'input))
                        (see (map-elt yaml 'see))
	                    (src (s-trim input))
	                    (tgt (map-elt (map-elt yaml 'expectations) 'html))
	                    ;; "teal" "brown" "gray" "purple" "lime" "green" "blue" "orange" "peach" "pink" "yellow" "custard" 
	                    (src.color (org-subtle-colors "lime"))
	                    (tgt.color (org-subtle-colors "peach")))
                   (cl-letf* (((symbol-function 'make-title) (lambda (it) (format "<h6 style=\"text-align:center; font-family: Lorna; padding: 0; margin: 0;\"> ﴾%s﴿ </h6>" it)))
	                          (src.title (make-title "What You Write"))
	                          (tgt.title (make-title "What You Get")))
                     (setq _X (format "<div><div style=\"padding: 1em;background-color: %s;border-radius: 15px;font-size: 0.9em;\"> %s <pre class=\"src src-org\">%s</pre></div> <div style=\"padding: 1em;background-color: %s;border-radius: 15px;font-size: 0.9em;\"> %s %s </div></div> <br> <details style=\"background-color: %s\"><summary style=\"text-align:center; font-family: Lorna; padding: 0; margin: 0; cursor: pointer;\">﴾How It’s Implemented﴿</summary> %s </details>"
	                                  src.color src.title src
	                                  tgt.color tgt.title tgt
	                                  (org-subtle-colors "custard")
	                                  (org-export-string-as (format "\n #+begin_src emacs-lisp \n %s \n#+end_src \n" (e2e--get-definition (or see (f-base file))))  'html :body-only-please))))))))

;; _X
;;
;; (org-export-string-as "hola" 'html t)
;; MA: Why is this empty?

;; (org-link/osbe-example "~/org-special-block-extras/tests/parallel.yaml" nil 'html)
;; (org-link/osbe-example "~/org-special-block-extras/tests/box.yaml" nil 'html)

(defun e2e--get-definition (block-name)
  (save-excursion
    (switch-to-buffer (find-file "~/org-special-block-extras/org-special-block-extras.el"))
    (or (progn (beginning-of-buffer) (search-forward (format "defblock %s" block-name nil t)))
        (progn (beginning-of-buffer) (search-forward (format "deflink %s" block-name nil t)))
        (progn (beginning-of-buffer) (search-forward (format "%s" block-name) nil t)))
    (-let [result (substring-no-properties (thing-at-point 'defun))]
      (bury-buffer)
      result)))
;; Example use:
;; (e2e--get-definition 'org-demo)
;; (e2e--get-definition 'org-make-badge)
(search-forward "TODOx" nil t)

;; TODO: Expose this in use-facing docs, then covert that prose into a yaml test using the workflow documented in osbe-example link type.
(org-defblock src (language "emacs-lisp" folded nil title "Details")
	          "yup"
	          (-let [org--supported-blocks '(details)] ;; to avoid infinite recursive calls for `src'
		        (-let [discloure (if folded "details" "box")]
		          (org-export-string-as
		           (format "\n#+begin_%s %s\n#+begin_src %s \n %s \n#+end_src\n#+end_%s\n" discloure title language (s-replace "+begin_box" "⊹begin_box" raw-contents) discloure)
		           'html
		           :body-only-please))))
;;
;; Example use
;;
;;     #+begin_src emacs-lisp -r -n :title Implementation :folded t
;;     (cl-defun speak ()
;;       (interactive)
;;       (message-box "Hello, world"))  
;;     #+end_src
;;       


