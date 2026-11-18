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

;;; svg and dot ---prose first: an HTML export that opens with <svg is put in `image-mode' by Org, which batch Emacs lacks

(defconst org-image-test--svg "
A gold bar:
#+begin_image svg
<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"20\"><rect width=\"40\" height=\"20\" fill=\"gold\"/></svg>
#+end_image")

(deftest "an svg image block, along HTML, is its SVG, raw; it is native there, as LaTeX is in LaTeX" [image]
  (should (string-match-p "^<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"20\"><rect[^>]*/></svg>$"
                          (org-export-string org-image-test--svg)))
  (should (equal '(html) (plist-get (alist-get 'svg org-image-block-languages) :native)))
  (should-not (plist-get (alist-get 'dot org-image-block-languages) :native)))

(deftest "an svg image block, along LaTeX, is a PDF of itself, by rsvg-convert" [image]
  (skip-unless (and (executable-find "rsvg-convert") (executable-find "pdftocairo")))
  (org-image-test--in-temp-directory
    (should (string-match-p "^\\\\includegraphics{images/image-[0-9a-f]\\{10\\}\\.pdf}$"
                            (org-export-string org-image-test--svg 'latex)))))

(deftest "a dot image block is an image everywhere: an SVG, inlined, in HTML; a PDF, in LaTeX" [image]
  (skip-unless (and (executable-find "dot") (executable-find "pdftocairo")))
  (org-image-test--in-temp-directory
    (let ((dot "#+begin_image dot\ndigraph { Org -> HTML; Org -> LaTeX }\n#+end_image"))
      (should (string-match-p "<img class=\"osbe-image\" alt=\"Rendered Graphviz\" src=\"data:image/svg\\+xml;base64,"
                              (org-export-string dot)))
      (should (string-match-p "^\\\\includegraphics{images/image-[0-9a-f]\\{10\\}\\.pdf}$" (org-export-string dot 'latex))))))

(deftest "a dot image block's :layout chooses Graphviz's layout engine" [image]
  (let (args)
    (cl-letf (((symbol-function 'org--image-run) (lambda (prog &rest given) (when (equal prog "dot") (setq args given)))))
      (org--image-dot-pdf "" "graph { a -- b }" '(:layout neato) "out.pdf"))
    (should (equal (-take 4 args) '("-Kneato" "-Gbgcolor=transparent" "-Gpad=0.05" "-Tpdf")))))

(deftest "an svg, or dot, image block has no preamble; a line of dashes in one is an error" [image]
  (should-error (org--image-svg-pdf "<defs/>" "<svg/>" nil "out.pdf") :type 'user-error)
  (should-error (org--image-dot-pdf "rankdir=LR" "digraph {}" nil "out.pdf") :type 'user-error))

;;; plantuml and mermaid

(deftest "a plantuml image block is an image everywhere, its body put within @startuml … @enduml if need be" [image]
  (let (puml)
    (cl-letf (((symbol-function 'org--image-run)
               (lambda (prog &rest args)
                 (when (equal prog "plantuml") (setq puml (with-temp-buffer (insert-file-contents (car (last args))) (buffer-string)))))))
      (org--image-plantuml-pdf "" "Alice -> Bob: Hi" nil "out.pdf")
      (should (equal puml "@startuml\nAlice -> Bob: Hi\n@enduml"))
      (org--image-plantuml-pdf "" "@startmindmap\n* Org\n@endmindmap" nil "out.pdf")
      (should (equal puml "@startmindmap\n* Org\n@endmindmap"))))
  (should-not (plist-get (alist-get 'plantuml org-image-block-languages) :native)))

(deftest "a plantuml image block renders, by plantuml, to an SVG, inlined, in HTML" [image]
  (skip-unless (and (executable-find "plantuml") (executable-find "pdfcrop") (executable-find "pdftocairo")))
  (org-image-test--in-temp-directory
    (should (string-match-p "<img class=\"osbe-image\" alt=\"Rendered PlantUML\" src=\"data:image/svg\\+xml;base64,"
                            (org-export-string "#+begin_image plantuml\nAlice -> Bob: Hi\n#+end_image")))))

(defconst org-image-test--mermaid "
#+begin_image mermaid
graph LR
  Org --> HTML
#+end_image")

(deftest "a mermaid image block, along HTML, is drawn in the browser, by Mermaid from a CDN; no tool is needed" [image]
  (let ((html (org-export-string org-image-test--mermaid)))
    (should (string-match-p "^<pre class=\"mermaid\">\ngraph LR\n  Org --&gt; HTML\n</pre>$" html))
    (should (string-search "mermaid.esm.min.mjs" html))))

(deftest "a mermaid image block, along Markdown, is a fenced mermaid block, which GitHub draws" [image]
  (require 'ox-md)
  (should (string-match-p "^```mermaid\ngraph LR\n  Org --> HTML\n```$" (org-export-string org-image-test--mermaid 'md))))

(deftest "a mermaid image block, along LaTeX, is rendered by mmdc, the Mermaid CLI, then cropped" [image]
  (let (programs)
    (cl-letf (((symbol-function 'org--image-run) (lambda (prog &rest _) (push prog programs))))
      (org--image-mermaid-pdf "" "graph LR\n  a --> b" nil "out.pdf"))
    (should (equal (reverse programs) '("mmdc" "pdfcrop"))))
  (should-error (org--image-mermaid-raw 'mermaid "x" "graph LR" 'html) :type 'user-error))

;;; d2; and how to install a missing tool

(deftest "a d2 image block is rendered by d2, to SVG, then rsvg-convert to PDF; its :layout, :theme, and :sketch, d2's flags" [image]
  (let (calls)
    (cl-letf (((symbol-function 'org--image-run) (lambda (prog &rest args) (push (cons prog args) calls))))
      (org--image-d2-pdf "" "x -> y: hi" '(:layout elk :theme 200 :sketch t) "out.pdf"))
    (setq calls (reverse calls))
    (should (equal (mapcar #'car calls) '("d2" "rsvg-convert")))
    (should (equal (-take 4 (cdar calls)) '("--layout=elk" "--theme=200" "--sketch" "--pad=4")))
    (should (string-suffix-p "image.svg" (car (last (cdar calls))))))
  (should-not (plist-get (alist-get 'd2 org-image-block-languages) :native))
  (should-error (org--image-d2-pdf "vars: {}" "x -> y" nil "out.pdf") :type 'user-error))

(deftest "an image block whose tool is not installed says how to install it" [image]
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (should (string-match-p "needs ‘d2’, which is not installed; brew install d2"
                            (cadr (should-error (org--image-run "d2" "x.d2") :type 'user-error))))
    (should (string-match-p "needs ‘mmdc’, which is not installed; npm install -g @mermaid-js/mermaid-cli"
                            (cadr (should-error (org--image-run "mmdc") :type 'user-error))))))

;;; p5js: a sketch, running in HTML; a still of it elsewhere

(defconst org-image-test--p5js "
#+begin_image p5js
function setup() {
  createCanvas(200, 100);
  noStroke();
}

function draw() {
  background(250, 245, 235, 40);
  fill(230, 120, 60);
  circle(noise(frameCount * 0.01) * width, height / 2, 24);
}
#+end_image")

(deftest "a p5js image block, along HTML, is its sketch running in an <iframe> of its own, p5 from a CDN" [image]
  (let ((html (org-export-string org-image-test--p5js)))
    (should (string-match-p "^<iframe class=\"osbe-p5js\" style=\"border: 0\" srcdoc=\"&lt;!DOCTYPE html&gt;" html))
    (should (string-search "&lt;script src=&quot;https://cdn.jsdelivr.net/npm/p5@" html))
    (should (string-search "createCanvas(200, 100);" html))
    (should (string-search "fill(230, 120, 60);" html))
    (should-not (string-match-p "srcdoc=\"[^\"]*<script" html))))

(deftest "a p5js image block's preamble joins its page's <head>, after p5 itself" [image]
  (let ((page (org--image-p5js-page "<script src=\"p5.sound.min.js\"></script>" "function setup() {}")))
    (should (string-match-p "p5\\.min\\.js\"></script>\n.*\n<script src=\"p5\\.sound\\.min\\.js\"></script>\n<script>\nfunction setup() {}\n</script>"
                            page))))

(defun org-image-test--png-size (png)
  "Returns (WIDTH HEIGHT), in pixels, of PNG, read from its header's IHDR chunk: big-endian 4-byte integers, from byte 16."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally png nil 0 24)
    (mapcar (lambda (at) (--reduce-from (+ (* acc 256) it) 0 (append (buffer-substring (+ 1 at) (+ 5 at)) nil)))
            '(16 20))))

(deftest "a p5js image block's canvas size is read from its createCanvas, when that is in numbers" [image]
  (should (equal '(400 200) (org--image-p5js-declared-size "function setup() {\n  createCanvas(400, 200);\n}")))
  (should (equal '(300 300) (org--image-p5js-declared-size "createCanvas(300,300, WEBGL);")))
  (should-not (org--image-p5js-declared-size "createCanvas(windowWidth, 200);"))
  (should-not (org--image-p5js-declared-size "createCanvas(400, 200 * 2);")))

(deftest "a p5js image block renders, by a headless Chrome's screenshot, to a PNG its canvas's size, after draw() runs" [image]
  (skip-unless (and org-image-block-browser (executable-find "rsvg-convert") (executable-find "pdftocairo")))
  (org-image-test--in-temp-directory
    (should (string-match-p "^\\\\includegraphics{images/image-[0-9a-f]\\{10\\}\\.pdf}$"
                            (org-export-string org-image-test--p5js 'latex)))
    ;; 200×100 CSS pixels, at 96 per inch, make 300×150 at the default 144.
    (should (equal '(300 150) (org-image-test--png-size (car (directory-files "images" t "\\.png\\'")))))))

(deftest "a p5js image block that draws no canvas is an error saying so" [image]
  (skip-unless org-image-block-browser)
  (should-error (org--image-p5js-pdf "" "function setup() { noCanvas(); }" nil "out.pdf") :type 'user-error))

(deftest "C-c C-c on a p5js image block, in an Emacs with xwidgets, runs it live, rendering nothing; so, no Chrome" [image]
  (let (rendered)
    (with-temp-buffer
      (insert org-image-test--p5js)
      (org-mode)
      (search-backward "noStroke")
      (cl-letf (((symbol-function 'display-graphic-p) #'always)
                ((symbol-function 'make-xwidget) #'ignore)
                ((symbol-function 'org--image-p5js-live) (lambda (&rest _) 'a-running-sketch))
                ((symbol-function 'org--image-render) (lambda (&rest _) (setq rendered t))))
        (should (org--block-cc-dispatcher))
        (should (equal '(xwidget :xwidget a-running-sketch)
                       (--some (overlay-get it 'display) (overlays-in (point-min) (point-max)))))
        (should-not rendered)))))

;;; How an image block looks in Org buffers: as its language

(org-deftest "a p5js image block looks as JavaScript, its preamble as HTML; sans Org's emphasis and links" [image fontify]
  :text "
#+begin_image p5js
<script src=\"p5.sound.min.js\"></script>
-----
function draw() {
  circle(noise(frameCount *0.01* 2) * width, height / 2, 24); // [[x]] is no link
}
#+end_image"
  (font-lock-ensure)
  (cl-flet ((faces-at (word) (goto-char (point-min)) (search-forward word)
                      (ensure-list (get-text-property (match-beginning 0) 'face))))
    (should (memq 'font-lock-function-name-face (faces-at "script")))
    (should (memq 'font-lock-keyword-face (faces-at "function")))
    (should-not (memq 'bold (faces-at "0.01")))
    (should (memq 'font-lock-comment-face (faces-at "[[x]]")))
    (should-not (memq 'org-link (faces-at "[[x]]")))
    (should-not (get-text-property (match-beginning 0) 'mouse-face))))

(org-deftest "a latex image block, and one naming no language, look as LaTeX" [image fontify]
  :text "
#+begin_image latex
\\begin{tikzpicture}\\draw (0,0) -- (1,1);\\end{tikzpicture}
#+end_image

#+begin_image :file arrow
\\begin{tikzpicture}\\fill (0,0) circle (1pt);\\end{tikzpicture}
#+end_image"
  (font-lock-ensure)
  (search-forward "\\draw")
  (should (memq 'font-lock-keyword-face (ensure-list (get-text-property (1- (point)) 'face))))
  (search-forward "\\fill")
  (should (memq 'font-lock-keyword-face (ensure-list (get-text-property (1- (point)) 'face)))))

(org-deftest "a section tagged :image_html: shows its text as HTML, up to the next heading, sans Org's markup" [image fontify]
  :text "
* Our button :image_html:
:PROPERTIES:
:WIDTH: 50%
:END:
<button>Click *me*</button>
* Afterwards
<button> is prose here."
  (font-lock-ensure)
  (cl-flet ((faces-at (word) (goto-char (point-min)) (search-forward word)
                      (ensure-list (get-text-property (1- (point)) 'face))))
    (should (memq 'font-lock-function-name-face (faces-at "<button")))
    (should-not (memq 'bold (faces-at "*me")))
    (should-not (memq 'font-lock-function-name-face (faces-at "* Afterwards\n<button")))
    (should-not (memq 'font-lock-function-name-face (faces-at ":WIDTH")))))

(org-deftest "a section tagged :image: shows its text as LaTeX, the image block's default" [image fontify]
  :text "
* Arrow :image:
\\begin{tikzpicture}\\draw (0,0) -- (1,1);\\end{tikzpicture}"
  (font-lock-ensure)
  (search-forward "\\draw")
  (should (memq 'font-lock-keyword-face (ensure-list (get-text-property (1- (point)) 'face)))))

(org-deftest "an html image block looks as HTML" [image fontify]
  :text "
#+begin_image html
<button>Click me</button>
#+end_image"
  (font-lock-ensure)
  (search-forward "<button")
  (should (memq 'font-lock-function-name-face (ensure-list (get-text-property (1- (point)) 'face)))))
