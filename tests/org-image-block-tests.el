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

(deftest "`org--image-base' names renderings by :file, else #+name:, else image; then the block's hash" [image]
  (let ((org-image-block-directory "images"))
    (should (string-match-p "\\`images/image-[0-9a-f]\\{10\\}\\'" (org--image-base 'latex "x" nil nil)))
    (should (string-match-p "\\`images/arrow-[0-9a-f]\\{10\\}\\'" (org--image-base 'latex "x" nil "arrow")))
    (should (string-match-p "\\`figs/tree-[0-9a-f]\\{10\\}\\'" (org--image-base 'latex "x" '(:file "figs/tree.png") "arrow")))
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
