;; Emacs Lisp End-to-End Testing
;;
;; “end to end” tests are data driven tests (e.g., using JSON or YAML) that make API calls
;; and verify the responses. One writes a test's input, then updates its expectations using
;; the provided M-xe2e-update-tests method, then verifies the changes are acceptable in Magit.
;;
;; Read a YAML file that defines `input' and `expectations'.
;; Run `compute-values' on `input' and check the result is the same as `expectations'
;; Recursively read all YAML files in a specified directory.
;;
;; Sequence:
;; 1. Write a YAML test with no `expectations'
;; 2. (e2e-update-this-test)  ;; Or: (e2e-update-tests)
;; 3. (e2e-run-tests)
;; 4. M-x magit, to see how the tests changed, commit if happy.
;; 5. Optionally, (ert-delete-all-tests)
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


(use-package yaml)
(use-package yaml-mode)
(use-package format-all)
(use-package ert)



(defun e2e--read-yaml-file (file)
  "Read and parse a YAML FILE, returning its contents as a hash table."
  (with-temp-buffer
    (insert-file-contents file)
    (yaml-parse-string (buffer-string))))


(defun e2e-run-this-test ()
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
		 (assert (s-contains-p (format "Output written on %s.pdf" (f-base file))
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


(cl-defun e2e-run-tests (&optional (directory "."))
  (interactive)
  "Run all e2e tests recursively in DIRECTORY."
  (loop for file in (directory-files-recursively directory "\\.yaml\\'")
        do (e2e--make-ert-test-for-yaml-file file))
  ;; Run all tests matching regex
  (ert "e2e/.*"))



(cl-defun e2e-update-tests ()
  (interactive)
  ;; update all tests
  (loop for file in (directory-files-recursively "." "\\.yaml\\'")
	do (e2e-update-test file)))

(cl-defun e2e-update-this-test ()
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
        (insert (e2e--yaml-encode-alist actual))
        (-let [format-all-formatters '(("YAML" prettier))]
	  (yaml-mode) (format-all-buffer)))))


	  
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



(defun compute-values (input no-prettier)
  "Computes new output given INPUT; `no-prettier' means no prettier/auto-format of result is done."
  (defun hs-hide-all () t) ;; HACK. PROBLEM: “hiding all blocks”, is this hide-show-mode?
  (map-into
   ;; Temporarily redefine gensym, so tests are deterministic
(let ((seed 0))
(cl-letf (((symbol-function 'gensym)
           (lambda () (format "g%s" (incf seed)))))
  (list
    (cons 'html
	  (with-temp-buffer
	    (org-special-block-extras-mode)
            (insert (org-export-string-as (format "\n%s\n" input) 'html :body-only-please))
	    (unless no-prettier
		 (-let [format-all-formatters '(("HTML" prettier))]
		   ;; TODO: When all my osbe e2e tests pass, then I should remove this ignore-errors.
		   ;; If something doesn't format, then that means it's likely invalid and should error.
		   ;; ignore-errors for unclosed <p> tags and other silly html errors
		   (html-mode) (ignore-errors (format-all-buffer))))	   
            (s-trim (buffer-string))))
    ;; Try to export, if it fails then just get the err msg.
    (cons 'latex
	  (condition-case err
	      (with-temp-buffer
		(org-special-block-extras-mode)
		(insert (org-export-string-as (format "\n%s\n" input) 'latex :body-only-please))
		(unless no-prettier
		  (-let [format-all-formatters '(("LaTeX" latexindent))] ;; !! brew install latexindent
		    (latex-mode) (format-all-buffer)))
		(s-trim (buffer-string)))
	    (error (format "🚫 The LaTex backend is intentionally unmaintained.\n🫠 Whoops, there seems to be an error: \n %S" err)))))))
'hash-table))


(defun my/hash-get-or-compute (hash key compute-fn)
  "Retrieve or compute the value for KEY in HASH, and store it if computed."
  (or (gethash key hash)
      (let ((value (funcall compute-fn)))
        (puthash key value hash)
        value)))

(defvar osbe-example-cache (make-hash-table :test 'equal)
  "Cache to avoid time re-reading yaml files!")
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
	 (src (s-trim input))
	 (tgt (map-elt (map-elt yaml 'expectations) 'html))
	 ;; "teal" "brown" "gray" "purple" "lime" "green" "blue" "orange" "peach" "pink" "yellow" "custard" 
	 (src.color (org-subtle-colors "lime"))
	 (tgt.color (org-subtle-colors "peach")))
    (cl-letf* (((symbol-function 'make-title) (lambda (it) (format "<h6 style=\"text-align:center; font-family: Lorna; padding: 0; margin: 0;\"> ﴾%s﴿ </h6>" it)))
	      (src.title (make-title "What You Write"))
	      (tgt.title (make-title "What You Get")))
    (format "<div><div style=\"padding: 1em;background-color: %s;border-radius: 15px;font-size: 0.9em;\"> %s <pre class=\"src src-org\">%s</pre></div> <div style=\"padding: 1em;background-color: %s;border-radius: 15px;font-size: 0.9em;\"> %s %s </div></div> <br> <details style=\"background-color: %s\"><summary style=\"text-align:center; font-family: Lorna; padding: 0; margin: 0; cursor: pointer;\">﴾How It’s Implemented﴿</summary> %s </details>"
	    src.color src.title src
	    tgt.color tgt.title tgt
	    (org-subtle-colors "custard")
	    (org-export-string-as (format "\n #+begin_src emacs-lisp \n %s \n#+end_src \n" (e2e--get-definition (f-base file)))  'html :body-only-please)))))))


(defun e2e--get-definition (block-name)
  (save-excursion ;; TODO: Why isn't this working?
    (switch-to-buffer (find-buffer-visiting "~/org-special-block-extras/org-special-block-extras.el"))
    (beginning-of-buffer)
    (search-forward (format "defblock %s" block-name))
    (-let [result (substring-no-properties (thing-at-point 'defun))]
      (bury-buffer)
      result)))
;; Example use:
;; (e2e--get-definition 'org-demo)


;; TODO: Expose this in use-facing docs, then covert that prose into a yaml test using the workflow documented in osbe-example link type.
(org-defblock src (language "emacs-lisp" folded nil title "Details")
	      "yup"
	      (-let [org--supported-blocks '(details)] ;; to avoid infinite recursive calls for `src'
		(-let [discloure (if folded "details" "box")]
		  (org-export-string-as
		   (format "\n#+begin_%s %s\n#+begin_src %s \n %s \n#+end_src\n#+end_%s\n" discloure title language raw-contents discloure)
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


