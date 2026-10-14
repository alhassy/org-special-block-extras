;;; An osbe-example:NAME link renders tests/examples/NAME.el  -*- lexical-binding: t; -*-

;; Each tests/examples/NAME.el holds one `define-org-example-test' named NAME: Org source, and its
;; HTML and LaTeX snapshots.  In the manual, org-special-block-extras.org, the link osbe-example:NAME
;; renders that source beside its HTML, then folds away the `org-defblock' or `org-deflink' of NAME.
;; So the manual shows exactly what the tests check; `C-u C-x C-e' on a test refreshes both.
;;
;; The manual loads this file at startup; see tests/org-examples.el for the snapshot relation itself.

(require 'cl-lib)
(unless (fboundp 'org-defblock)
  (load-file (expand-file-name "../../org-special-block-extras.el" (file-name-directory (or load-file-name buffer-file-name)))))

(defconst osbe--examples-directory (file-name-directory (or load-file-name buffer-file-name))
  "The directory of this file, tests/examples/, wherein each example NAME lives in NAME.el.")

(defun osbe--example (name)
  "NAME ↦ plist (:input … :html … :latex …) of its `define-org-example-test' in tests/examples/NAME.el."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (format "%s.el" name) osbe--examples-directory))
    (cl-loop for form = (ignore-errors (read (current-buffer))) while form
             when (and (eq (car-safe form) 'define-org-example-test) (equal (cadr form) name))
             return (cddr form))))

(defun osbe--definition (name)
  "NAME ↦ source of its `org-defblock' or `org-deflink' form, from this repo's *.el files; else \"\"."
  (or (cl-loop for f in (directory-files (expand-file-name "../.." osbe--examples-directory) t "\\.el\\'")
               thereis (with-temp-buffer
                         (insert-file-contents f)
                         (emacs-lisp-mode)
                         (when (re-search-forward (format "(org-def\\(block\\|link\\) %s\\_>" (regexp-quote name)) nil t)
                           (goto-char (match-beginning 0))
                           (thing-at-point 'sexp t))))
      ""))

(org-defblock osbe-example (name)
  "Render example NAME, from tests/examples/NAME.el, as Org source beside its HTML export and implementation."
  (let* ((example (osbe--example name))
         (title (lambda (it) (format "<h6 style=\"text-align:center; font-family: Lorna; padding: 0; margin: 0;\"> ﴾%s﴿ </h6>" it)))
         (panel (lambda (colour body) (format "<div style=\"padding: 1em;background-color: %s;border-radius: 15px;font-size: 0.9em;\"> %s </div>" (org-subtle-colors colour) body))))
    (format "<div>%s %s</div> <br> <details style=\"background-color: %s\"><summary style=\"text-align:center; font-family: Lorna; padding: 0; margin: 0; cursor: pointer;\">﴾How It’s Implemented﴿</summary> %s </details>"
            (funcall panel "lime" (format "%s <pre class=\"src src-org\">%s</pre>" (funcall title "What You Write") (s-trim (plist-get example :input))))
            (funcall panel "peach" (format "%s %s" (funcall title "What You Get")
                                           ;; `:live' examples are exported now, not snapshotted; see tests/org-examples.el.
                                           (if (eq (plist-get example :html) :live)
                                               (org-export-string-as (plist-get example :input) 'html t)
                                             (plist-get example :html))))
            (org-subtle-colors "custard")
            (org-export-string-as (format "\n#+begin_src emacs-lisp\n%s\n#+end_src\n" (osbe--definition name)) 'html t))))
