;;; org-pdf-export-tests.el --- Every example compiles to a PDF, its blocks loading their own LaTeX packages  -*- lexical-binding: t; -*-

;; A document declares no LaTeX packages for our blocks: each block names those it needs by its `:latex-pkgs', and a
;; LaTeX export loads them, by `org--load-latex-packages'.  So this test declares none, and compiles every example of
;; the manual, as an article and as beamer slides.  See https://github.com/alhassy/org-special-block-extras/issues/49.
;; We compile with LuaLaTeX, which reads Unicode such as 𝒳 and ⇒ as is; pdflatex needs \newunicodechar instead.

(unless (fboundp 'deftest)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(add-to-list 'load-path (expand-file-name "~/org-special-block-extras/org-graph"))
(require 'org-graph-block) ; The graph example's block.

(defun org-pdf-test--examples ()
  "↦ (NAME . INPUT) for each `define-org-example-test' of tests/examples/*.el, by NAME."
  (cl-loop for file in (directory-files (expand-file-name "~/org-special-block-extras/tests/examples") t "\\.el\\'")
           append (with-temp-buffer
                    (insert-file-contents file)
                    (cl-loop for form = (ignore-errors (read (current-buffer))) while form
                             when (eq (car-safe form) 'define-org-example-test)
                             collect (cons (cadr form) (plist-get (cddr form) :input))))))

(defun org-pdf-test--compile (name export &optional heading-depth)
  "(NAME, EXPORT, HEADING-DEPTH) ↦ the first LaTeX error, else nil, of compiling every example to NAME.pdf via EXPORT,
such as `org-latex-export-to-latex'; with HEADING-DEPTH stars before each example's heading, and one more on its own."
  (let* ((org-defblock-evaluate-switches t)
         (dir (make-temp-file "osbe-pdf-" t))
         (org (expand-file-name (concat name ".org") dir))
         (stars (make-string (or heading-depth 1) ?*)))
    (with-temp-file org
      (insert "#+title: Every example\n#+latex_compiler: lualatex\n#+options: H:1\n")
      (pcase-dolist (`(,example . ,input) (org-pdf-test--examples))
        (insert (format "\n%s %s\n%s\n" stars example (replace-regexp-in-string "^\\*" (concat stars "*") input)))))
    (with-current-buffer (find-file-noselect org)
      (funcall export)
      (kill-buffer))
    (let ((default-directory dir))
      (call-process "latexmk" nil nil nil "-lualatex" "-interaction=nonstopmode" "-halt-on-error" (concat name ".tex"))
      (or (with-temp-buffer
            (insert-file-contents (concat name ".log"))
            ;; The LaTeX error itself, with the line after it; a bare non-zero status says little.
            (when (re-search-forward "^! .*\n.*" nil t) (match-string 0)))
          (unless (file-exists-p (concat name ".pdf")) "No PDF, though no LaTeX error either")))))

(deftest "every example exports to a PDF that compiles, declaring no LaTeX packages: its blocks load theirs; issue #49" [pdf]
  (skip-unless (and (executable-find "latexmk") (executable-find "lualatex")))
  (should-not (org-pdf-test--compile "every-example" #'org-latex-export-to-latex)))

(deftest "every example exports to beamer slides that compile, declaring no LaTeX packages either; issue #35" [pdf]
  (skip-unless (and (executable-find "latexmk") (executable-find "lualatex")))
  (require 'ox-beamer)
  ;; Each example is a frame, its own headings within it.
  (should-not (org-pdf-test--compile "every-example-slides" #'org-beamer-export-to-latex)))

(provide 'org-pdf-export-tests)
;;; org-pdf-export-tests.el ends here
