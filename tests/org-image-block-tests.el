;;; org-image-block-tests.el --- #+begin_image LANG: itself along its own backend, else an image  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-export-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(defmacro org-image-test--in-temp-directory (&rest body)
  "Runs BODY with `default-directory' a fresh temporary directory, axed afterwards."
  (declare (indent 0))
  `(let ((default-directory (file-name-as-directory (make-temp-file "osbe-image-test-" t))))
     (unwind-protect (progn ,@body) (delete-directory default-directory t))))

(defconst org-image-test--tikz "
#+name: arrow
#+begin_image latex
\\begin{tikzpicture}\\draw[->] (0,0) -- (2,1);\\end{tikzpicture}
#+end_image")

;;; Its own backend: itself, raw; no tools needed

(deftest "a latex image block, along LaTeX, is its LaTeX, raw" [image]
  (should (string-match-p "^\\\\begin{tikzpicture}\\\\draw\\[->\\] (0,0) -- (2,1);\\\\end{tikzpicture}$"
                          (org-export-string org-image-test--tikz 'latex))))

(deftest "an html image block, along HTML, is its HTML, raw" [image]
  (should (string-match-p "^<button>Click me</button>$"
                          (org-export-string "#+begin_image html\n<button>Click me</button>\n#+end_image"))))

;;; Naming the renderings

(deftest "`org--image-base' names renderings by :file, exactly; else by #+name:, else image, then the block's hash" [image]
  (let ((org-image-block-directory "images"))
    (should (string-match-p "\\`images/image-[0-9a-f]\\{10\\}\\'" (org--image-base 'latex "x" nil nil)))
    (should (string-match-p "\\`images/arrow-[0-9a-f]\\{10\\}\\'" (org--image-base 'latex "x" nil "arrow")))
    (should (equal "figs/tree" (org--image-base 'latex "x" '(:file "figs/tree.png") "arrow")))
    (should (equal "images/tree" (org--image-base 'latex "x" '(:file tree) "arrow")))
    (should-not (equal (org--image-base 'latex "x" nil nil) (org--image-base 'latex "y" nil nil)))
    (should (equal (org--image-base 'html "x" '(:file "a") nil) (org--image-base 'html "x" '(:file "a") nil)))))

;;; Unrendered, sans the tools

(deftest "an image block whose tools are lacking exports as its source, with a warning, rather than failing" [image]
  (org-image-test--in-temp-directory
    (let (warned)
      (cl-letf (((symbol-function 'org--image-render) (lambda (&rest _) (user-error "No pdflatex here")))
                ((symbol-function 'display-warning) (lambda (_ message &rest _) (setq warned message))))
        (should (string-match-p "<pre class=\"example\"[^>]*>\n\\\\begin{tikzpicture}"
                                (org-export-string org-image-test--tikz)))
        (should (string-match-p "shown as source, unrendered: No pdflatex here" warned))))))

;;; Rendering, where the tools are at hand

(deftest "a latex image block renders to an SVG, inlined in HTML; a PDF, in a Markdown-less LaTeX; a PNG, in Markdown" [image]
  (skip-unless (and (executable-find "pdflatex") (executable-find "pdftocairo")))
  (require 'ox-md)
  (org-image-test--in-temp-directory
    (should (string-match-p "<img class=\"osbe-image\" alt=\"arrow\" src=\"data:image/svg\\+xml;base64,"
                            (org-export-string org-image-test--tikz)))
    (should (string-match-p "<img alt=\"arrow\" src=\"images/arrow-[0-9a-f]\\{10\\}\\.png\">"
                            (org-export-string org-image-test--tikz 'md)))
    (should (equal 3 (length (directory-files "images" nil "\\`arrow-"))))))

(deftest "an edited image block is rendered afresh, its former renderings axed" [image]
  (skip-unless (and (executable-find "pdflatex") (executable-find "pdftocairo")))
  (org-image-test--in-temp-directory
    (org-export-string org-image-test--tikz)
    (let ((before (directory-files "images" nil "\\`arrow-")))
      (org-export-string (s-replace "(2,1)" "(1,2)" org-image-test--tikz))
      (let ((after (directory-files "images" nil "\\`arrow-")))
        (should (equal 3 (length after)))
        (should-not (-intersection before after))))))

(deftest "an html image block renders, by a headless Chrome, to a PDF that LaTeX includes" [image]
  (skip-unless (and org-image-block-browser (executable-find "pdfcrop") (executable-find "pdftocairo")))
  (org-image-test--in-temp-directory
    (should (string-match-p "\\\\includegraphics{images/image-[0-9a-f]\\{10\\}\\.pdf}"
                            (org-export-string "#+begin_image html\n<b>Bold</b>\n#+end_image" 'latex)))))

(org-deftest "C-c C-c within an image block renders it, and shows its PNG atop the block; again, removes it" [image]
  :text org-image-test--tikz
  :at "tikzpicture"
  (skip-unless (and (executable-find "pdflatex") (executable-find "pdftocairo")))
  (org-image-test--in-temp-directory
    (should (org--block-cc-dispatcher))
    (should (--some (overlay-get it 'org-image-preview) (overlays-in (point-min) (point-max))))
    (should (org--block-cc-dispatcher))
    (should-not (--some (overlay-get it 'org-image-preview) (overlays-in (point-min) (point-max))))))

;;; The preamble, compiler, :css, and :width reach the renderers

(deftest "a latex image is rendered with `org-image-block-latex-preamble', the document's #+latex_header: lines, then its own preamble; by its #+latex_compiler:" [image]
  (let (program tex)
    (cl-letf (((symbol-function 'org--image-run)
               (lambda (prog &rest args)
                 (setq program prog
                       tex (with-temp-buffer (insert-file-contents (expand-file-name (car (last args)))) (buffer-string)))
                 (write-region "" nil (expand-file-name "image.pdf")))))
      (with-temp-buffer
        (insert "#+latex_compiler: xelatex\n#+latex_header: \\newcommand{\\Ring}{\\mathcal{R}}\n")
        (org-mode)
        (let ((pdf (make-temp-file "osbe-image-" nil ".pdf")))
          (org--image-latex-pdf "\\usepackage{xcolor}" "$\\Ring$" nil pdf)
          (delete-file pdf))))
    (should (equal program "xelatex"))
    (should (string-match-p "\\`\\\\documentclass\\[border=2pt,varwidth\\]{standalone}\n\\\\usepackage{amsmath,amssymb}\n\\\\usepackage{tikz}\n\\\\newcommand{\\\\Ring}{\\\\mathcal{R}}\n\\\\usepackage{xcolor}\n\\\\begin{document}\n\\$\\\\Ring\\$" tex))))

(deftest "an html image is rendered with its :css, :width, and preamble in <head>, atop a transparent page sans margins" [image]
  (let (html)
    (cl-letf (((symbol-function 'org--image-run)
               (lambda (_prog &rest args)
                 (-when-let (page (--first (string-prefix-p "file://" it) args))
                   (setq html (with-temp-buffer (insert-file-contents (string-remove-prefix "file://" page)) (buffer-string))))))
              (org-image-block-browser "chrome"))
      (org--image-html-pdf "<script>let x;</script>" "<b>Hi</b>" '(:css "b { color: red }" :width "600px") (make-temp-file "osbe-image-" nil ".pdf")))
    (should (string-match-p "@page { margin: 0 }" html))
    (should (string-match-p "body { display: inline-block; padding: 4px; width: 600px }b { color: red }</style>" html))
    (should (string-match-p "</style>\n<script>let x;</script>\n</head><body>\n<b>Hi</b>\n</body>" html))))

(deftest "a latex image's hash covers the document's #+latex_header: lines, so a new header renders it afresh" [image]
  (let ((plain (with-temp-buffer (org-mode) (org--image-hash 'latex "x" nil)))
        (headed (with-temp-buffer (insert "#+latex_header: \\usepackage{xcolor}\n") (org-mode) (org--image-hash 'latex "x" nil))))
    (should-not (equal plain headed))))

;;; The preamble, above a line of dashes; :file; :as-image; :resolution; -shell-escape

(deftest "`org--image-split' parts a block's preamble from its body by its first line of five or more dashes" [image]
  (should (equal (org--image-split "\\usepackage{x}\n-----\nbody\n------\nmore")
                 '("\\usepackage{x}" . "body\n------\nmore")))
  (should (equal (org--image-split "a---b\n---\nc") '("" . "a---b\n---\nc"))))

(deftest "a latex image block, along LaTeX, puts its preamble in the document's preamble, and its body in place" [image]
  (let ((latex (with-temp-buffer
                 (insert "#+begin_image latex\n\\usepackage{smartdiagram}\n-----\n\\smartdiagram[bubble diagram]{A, B}\n#+end_image")
                 (org-mode)
                 (org-special-block-extras-mode)
                 (org-export-as 'latex))))
    (should (< (string-search "\\usepackage{smartdiagram}\n" latex)
               (string-search "\\begin{document}" latex)
               (string-search "\n\\smartdiagram[bubble diagram]{A, B}\n" latex)))
    (should-not (string-match-p "^-----" latex))))

(deftest "an html image block, along HTML, is its preamble then its body, raw" [image]
  (should (string-match-p "^<style>b { color: red }</style>\n<b>Hi</b>$"
                          (org-export-string "#+begin_image html\n<style>b { color: red }</style>\n-----\n<b>Hi</b>\n#+end_image"))))

(deftest "an image block's :as-image t is an image even along its own backend" [image]
  (org-image-test--in-temp-directory
    (cl-letf (((symbol-function 'org--image-render)
               (lambda (_language _source _args base)
                 (make-directory (file-name-directory base) t)
                 (write-region "" nil (concat base ".pdf")))))
      (should (string-match-p "^\\\\includegraphics{images/arrow-[0-9a-f]\\{10\\}\\.pdf}$"
                              (org-export-string (s-replace "#+begin_image latex" "#+begin_image latex :as-image t"
                                                            org-image-test--tikz)
                                                 'latex))))))

(deftest "an image block's :resolution renders its PNG so; :file names it exactly, axing no other renderings" [image]
  (org-image-test--in-temp-directory
    (let (calls)
      (make-directory "images")
      (write-region "" nil "images/tree-0123456789.png")
      (cl-letf (((symbol-function 'org--image-run) (lambda (&rest args) (push args calls)))
                ((symbol-function 'org--image-latex-pdf) #'ignore))
        (org--image-render 'latex "x" '(:file "tree" :resolution 300) "images/tree"))
      (should (member "300" (--first (member "-png" it) calls)))
      (should (file-exists-p "images/tree-0123456789.png")))))

(deftest "with `org-image-block-shell-escape', a latex image compiles with -shell-escape; else not" [image]
  (dolist (escape '(nil t))
    (let ((org-image-block-shell-escape escape) args)
      (cl-letf (((symbol-function 'org--image-run)
                 (lambda (_prog &rest given) (setq args given) (write-region "" nil (expand-file-name "image.pdf")))))
        (let ((pdf (make-temp-file "osbe-image-" nil ".pdf")))
          (with-temp-buffer (org-mode) (org--image-latex-pdf "" "x" nil pdf))
          (delete-file pdf)))
      (should (eq escape (and (member "-shell-escape" args) t))))))
