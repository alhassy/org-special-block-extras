;;; org-pdf-export-tests.el --- Every demo compiles to a PDF, its blocks loading their own LaTeX packages  -*- lexical-binding: t; -*-

;; A document declares no LaTeX packages for our blocks: each block names those it needs by its `:latex-head', and a
;; LaTeX export loads them, by `org--load-latex-head'.  So this test declares none, and compiles every demo of the manual
;; and of tests/demos.org, as an article and as beamer slides.  See https://github.com/alhassy/org-special-block-extras/issues/49.
;; We compile with LuaLaTeX, which reads Unicode such as 𝒳 and ⇒ as is; pdflatex needs \newunicodechar instead.

(unless (fboundp 'deftest)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(require 'org-graph-block) ; The graph example's block.

(defun org-pdf-test--examples ()
  "↦ (NAME . SOURCE) for each exported :demo of the manual and of tests/demos.org, NAME being its file and line."
  (cl-loop for file in '("org-special-block-extras.org" "tests/demos.org")
           append (with-temp-buffer
                    (insert-file-contents (expand-file-name file "~/org-special-block-extras"))
                    (let ((org-inhibit-startup t)) (org-mode))
                    (cl-loop for site in (reverse (org-demo--sites t))
                             unless (save-excursion (goto-char (car site)) (org-in-commented-heading-p))
                             collect (cons (format "%s, line %s" (file-name-nondirectory file) (line-number-at-pos (car site)))
                                           (org-demo--source site 'latex))))))

(defun org-pdf-test--compile (name export &optional heading-depth)
  "(NAME, EXPORT, HEADING-DEPTH) ↦ the first LaTeX error, else nil, of compiling every example to NAME.pdf via EXPORT,
such as `org-latex-export-to-latex'; with HEADING-DEPTH stars before each example's heading, and one more on its own."
  (let ((stars (make-string (or heading-depth 1) ?*)))
    (org-pdf-test--compile-org
     name export
     (concat "#+title: Every example\n#+latex_compiler: lualatex\n#+options: H:1\n"
             (mapconcat (pcase-lambda (`(,example . ,input))
                          (format "\n%s %s\n%s\n" stars example (replace-regexp-in-string "^\\*" (concat stars "*") input)))
                        (org-pdf-test--examples) "")))))

(defun org-pdf-test--compile-org (name export org-text)
  "(NAME, EXPORT, ORG-TEXT) ↦ the first LaTeX error, else nil, of compiling ORG-TEXT to NAME.pdf via EXPORT."
  (let* ((org-defblock-evaluate-switches t)
         (dir (make-temp-file "osbe-pdf-" t))
         (org (expand-file-name (concat name ".org") dir)))
    (with-temp-file org (insert org-text))
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

(deftest "margin notes compile within a box and side-by-side columns, as footnotes there, since the margin is out of reach" [pdf]
  (skip-unless (and (executable-find "latexmk") (executable-find "lualatex")))
  (should-not (org-pdf-test--compile-org "margin-notes-within" #'org-latex-export-to-latex "
#+latex_compiler: lualatex
#+begin_box
A [[tooltip:word][with a tip]], and maths with one:
#+begin_math
x[fn::the unknown] = 1
#+end_math
#+end_box
#+begin_parallel
A [[tooltip:word][with a tip]].
#+columnbreak:
#+begin_margin
A note.
#+end_margin
#+end_parallel")))

(provide 'org-pdf-export-tests)
;;; org-pdf-export-tests.el ends here
