;;; org-pdf-export-tests.el --- Every example compiles to a PDF, given just the LaTeX packages listed here  -*- lexical-binding: t; -*-

;; Org's default LaTeX header already loads amsmath, amssymb, ulem, hyperref, and graphicx; our blocks need only
;; the packages of `org-pdf-test--latex-headers' beside them.  So, to export to PDF, a document has these lines:
;;
;;   #+latex_header: \usepackage{xcolor}     % Colours: red:𝒳, highlight, margin, callouts; mixes like red!5!white
;;   #+latex_header: \usepackage{tcolorbox}  % box, details, solution, the callouts, and the :boxed and :folded switches
;;   #+latex_header: \usepackage{multicol}   % parallel
;;   #+latex_header: \usepackage{adjustbox}  % Only a margin block's #+resize:
;;
;; See https://github.com/alhassy/org-special-block-extras/issues/49.  We compile with LuaLaTeX, which reads Unicode
;; such as 𝒳 and ⇒ as is; pdflatex needs \newunicodechar for each such character instead.

(unless (fboundp 'deftest)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(add-to-list 'load-path (expand-file-name "~/org-special-block-extras/org-graph"))
(require 'org-graph-block) ; The graph example's block.

(defconst org-pdf-test--latex-headers
  '("\\usepackage{xcolor}     % Colours: red:𝒳, highlight, margin, callouts; mixes like red!5!white"
    "\\usepackage{tcolorbox}  % box, details, solution, the callouts, and the :boxed and :folded switches"
    "\\usepackage{multicol}   % parallel"
    "\\usepackage{adjustbox}  % Only a margin block's #+resize:")
  "The LaTeX packages our blocks need, beyond Org's default header; mimic these as #+latex_header: lines.")

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
      (dolist (header org-pdf-test--latex-headers) (insert "#+latex_header: " header "\n"))
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

(deftest "every example exports to a PDF that compiles, given just the LaTeX packages listed here; issue #49" [pdf]
  (skip-unless (and (executable-find "latexmk") (executable-find "lualatex")))
  (should-not (org-pdf-test--compile "every-example" #'org-latex-export-to-latex)))

(deftest "every example exports to beamer slides that compile, given the same LaTeX packages; issue #35" [pdf]
  (skip-unless (and (executable-find "latexmk") (executable-find "lualatex")))
  (require 'ox-beamer)
  ;; Each example is a frame, its own headings within it.
  (should-not (org-pdf-test--compile "every-example-slides" #'org-beamer-export-to-latex)))

(provide 'org-pdf-export-tests)
;;; org-pdf-export-tests.el ends here
