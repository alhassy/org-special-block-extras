;;; org-image-block.el --- #+begin_image LANG: LaTeX, HTML, SVG, dot, PlantUML, Mermaid; itself, else an image  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A TikZ picture in a web page, or an HTML widget in a PDF; an SVG, or a Graphviz graph, in either:
;;
;;     #+begin_image latex                     #+begin_image html :css "button { padding: 1em }"
;;     \begin{tikzpicture}                     <button>Click me</button>
;;       \draw[->] (0,0) -- (2,1);             #+end_image
;;     \end{tikzpicture}
;;     #+end_image
;;
;; Along its own backend, a block is itself, raw; along any other, it is an image of itself: an SVG in
;; HTML, inlined so the page needs no image files; a PDF in LaTeX, so it stays vector; else a PNG.
;; Each language is made into a cropped PDF ---LaTeX by pdflatex and the standalone class, HTML by a
;; headless Chrome, SVG by rsvg-convert, dot by Graphviz, PlantUML by plantuml, Mermaid by mmdc--- whence
;; pdftocairo makes the SVG and PNG; see `org-image-block-languages'.  An SVG is itself in HTML; Mermaid,
;; in HTML and on GitHub, which draw it; dot and PlantUML are images everywhere.
;;
;; Above a line of five dashes is the block's own preamble: LaTeX's, before \begin{document}; or HTML's
;; <head>.  C-c C-c renders the images, and shows one atop the block; export uses them.  Their names
;; hold a hash of the block, so an edited block is rendered afresh ---unless :file names them exactly,
;; a figure to link to; one never rendered is rendered upon export, when the tools are at hand, else
;; shown as source with a warning.  :as-image t is an image along its own backend too.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'dash)
(require 's)
(require 'org)
(require 'ox)
(require 'ox-html)
(require 'org-defblock)
(require 'org-ctrl-c-ctrl-c-for-special-block)

(defcustom org-image-block-directory "images"
  "The directory, relative to the document, wherein #+begin_image blocks' renderings are written."
  :type 'directory
  :group 'org-special-block-extras)

(defcustom org-image-block-browser
  (or (executable-find "chromium") (executable-find "google-chrome") (executable-find "chrome")
      (-first #'file-executable-p '("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
                                    "/Applications/Chromium.app/Contents/MacOS/Chromium")))
  "The Chrome, or Chromium, executable that prints a #+begin_image html block to PDF, headless."
  :type '(choice file (const nil))
  :group 'org-special-block-extras)

(defcustom org-image-block-latex-preamble "\\usepackage{amsmath,amssymb}\n\\usepackage{tikz}"
  "The LaTeX loaded by every #+begin_image latex block, before the document's own #+latex_header: lines."
  :type 'string
  :group 'org-special-block-extras)

(defcustom org-image-block-png-resolution 144
  "The resolution, in pixels per inch, of the PNGs that #+begin_image blocks render to; 144 is crisp on HiDPI.
A block's own :resolution wins."
  :type 'integer
  :group 'org-special-block-extras)

(defcustom org-image-block-shell-escape nil
  "Whether #+begin_image latex blocks compile with -shell-escape; as minted, or gnuplottex, need.
Off by default: -shell-escape lets the LaTeX run any shell command, so a stranger's document could too."
  :type 'boolean
  :group 'org-special-block-extras)

(defvar org-image-block-languages
  '((latex    :render org--image-latex-pdf    :native (latex))
    (html     :render org--image-html-pdf     :native (html))
    (svg      :render org--image-svg-pdf      :native (html))
    (dot      :render org--image-dot-pdf)
    (plantuml :render org--image-plantuml-pdf)
    (mermaid  :render org--image-mermaid-pdf  :native (html md) :raw org--image-mermaid-raw))
  "Each (LANGUAGE :render RENDER :native BACKENDS :raw RAW) of #+begin_image: along one of BACKENDS ---md, Markdown,
being one apart from html--- the block is itself, by (RAW LANGUAGE PREAMBLE BODY BACKEND), else `org--image-raw';
along any other, RENDER writes its PREAMBLE and BODY, see `org--image-split', as a cropped PDF, by (RENDER PREAMBLE
BODY ARGS PDF), ARGS being the block's keyword arguments; whence the SVG and PNG are made.  A new language is one
entry.")

;;;; A block's preamble: what is above its first line of dashes

(defconst org--image-rule "^[ \t]*-\\{5,\\}[ \t]*$"
  "A line of five or more dashes, as Org's horizontal rule: it parts a #+begin_image block's preamble from its body.
Five, since a line of three, ---, is LaTeX's em-dash.")

(defun org--image-split (source)
  "Returns (PREAMBLE . BODY) of an image block's SOURCE, parted by its first line of dashes; else (\"\" . SOURCE).
  \\usepackage{smartdiagram}
  -----                              ⇒  (\"\\\\usepackage{smartdiagram}\" . \"\\\\smartdiagram[…]{…}\")
  \\smartdiagram[…]{…}
The preamble is LaTeX's, before \\begin{document}; or HTML's, within <head>, such as a <style> or <script>."
  (if (string-match org--image-rule source)
      (cons (string-trim (substring source 0 (match-beginning 0))) (string-trim (substring source (match-end 0))))
    (cons "" source)))

;;;; Rendering: SOURCE ⟶ cropped PDF ⟶ SVG, PNG

(defun org--image-run (program &rest args)
  "Runs PROGRAM on ARGS, in `default-directory'; an error, quoting its output's tail, should it fail."
  (unless (executable-find program) (user-error "#+begin_image needs ‘%s’, which is not installed" program))
  (with-temp-buffer
    (unless (eq 0 (apply #'call-process program nil t nil args))
      (user-error "‘%s’ failed on this #+begin_image block:\n%s" (file-name-nondirectory program)
                  (buffer-substring (max (point-min) (- (point-max) 1500)) (point-max))))))

(defun org--image-latex-headers ()
  "Returns the document's #+latex_header: and #+latex_header_extra: lines, as one string."
  (mapconcat (lambda (it) (string-join (cdr it) "\n"))
             (org-collect-keywords '("LATEX_HEADER" "LATEX_HEADER_EXTRA")) "\n"))

(defun org--image-latex-pdf (preamble body _args pdf)
  "Writes LaTeX BODY as PDF, cropped to it by the standalone class: `org-image-block-latex-preamble', the document's
#+latex_header: lines, then the block's PREAMBLE; by its #+latex_compiler:, else `org-latex-compiler'; with
-shell-escape, if `org-image-block-shell-escape'."
  (let* ((dir (make-temp-file "org-image-" t))
         (compiler (or (cadar (org-collect-keywords '("LATEX_COMPILER"))) org-latex-compiler "pdflatex"))
         ;; Read here, in the document, not within the .tex file's buffer below.
         (headers (org--image-latex-headers)))
    (with-temp-file (expand-file-name "image.tex" dir)
      (insert "\\documentclass[border=2pt,varwidth]{standalone}\n" org-image-block-latex-preamble "\n"
              headers "\n" preamble "\n\\begin{document}\n" body "\n\\end{document}\n"))
    (let ((default-directory (file-name-as-directory dir)))
      (apply #'org--image-run compiler
             `(,@(and org-image-block-shell-escape '("-shell-escape")) "-interaction=nonstopmode" "-halt-on-error"
               "image.tex")))
    (copy-file (expand-file-name "image.pdf" dir) pdf t)
    (delete-directory dir t)))

(defun org--image-html-pdf (preamble body args pdf)
  "Writes HTML BODY as PDF, by `org-image-block-browser', headless, cropped by pdfcrop; the block's PREAMBLE, and
ARGS' :css, join its <head>, and ARGS' :width, such as 600px, fixes its width.  The page is transparent, sans
margins, its colours printed as shown."
  (unless org-image-block-browser (user-error "#+begin_image html needs Chrome, or Chromium; see `org-image-block-browser'"))
  (let* ((dir (make-temp-file "org-image-" t))
         (html (expand-file-name "image.html" dir))
         (printed (expand-file-name "printed.pdf" dir)))
    (with-temp-file html
      (insert "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><style>"
              "@page { margin: 0 } * { -webkit-print-color-adjust: exact; print-color-adjust: exact }"
              " html, body { margin: 0; background: transparent }"
              (format " body { display: inline-block; padding: 4px; %s }"
                      (-if-let (width (plist-get args :width)) (format "width: %s" width) ""))
              (format "%s" (or (plist-get args :css) ""))
              "</style>\n" preamble "\n</head><body>\n" body "\n</body></html>"))
    (org--image-run org-image-block-browser "--headless=new" "--disable-gpu" "--no-pdf-header-footer"
                    (concat "--print-to-pdf=" printed) (concat "file://" html))
    (org--image-run "pdfcrop" "--margins" "0" printed pdf)
    (delete-directory dir t)))

(defun org--image-sans-preamble (language preamble)
  "Refuses a PREAMBLE for LANGUAGE, which has no place for one; e.g., an SVG's or a dot graph's whole is its body."
  (unless (string-empty-p preamble)
    (user-error "A #+begin_image %s block has no preamble; axe its line of dashes, -----, and what is above it"
                language)))

(defun org--image-svg-pdf (preamble body _args pdf)
  "Writes SVG BODY as PDF, by rsvg-convert, its page the SVG's own size; it has no PREAMBLE."
  (org--image-sans-preamble 'svg preamble)
  (let ((svg (make-temp-file "org-image-" nil ".svg" body)))
    (org--image-run "rsvg-convert" "--format" "pdf" "--output" pdf svg)
    (delete-file svg)))

(defun org--image-dot-pdf (preamble body args pdf)
  "Writes Graphviz BODY, such as digraph { a -> b }, as PDF, by dot, cropped by pdfcrop; ARGS' :layout,
such as neato or circo, chooses the layout engine.  It has no PREAMBLE."
  (org--image-sans-preamble 'dot preamble)
  (let ((dot (make-temp-file "org-image-" nil ".dot" body))
        (drawn (make-temp-file "org-image-" nil ".pdf")))
    ;; Transparent, as the other languages' renderings; the graph's own attributes override these defaults.  Graphviz's
    ;; PDF has a margin of its own, which pdfcrop trims.
    (apply #'org--image-run "dot" `(,@(-some->> (plist-get args :layout) (format "-K%s") list)
                                    "-Gbgcolor=transparent" "-Gpad=0.05" "-Tpdf" "-o" ,drawn ,dot))
    (org--image-run "pdfcrop" "--margins" "2" drawn pdf)
    (delete-file dot)
    (delete-file drawn)))

(defun org--image-plantuml-pdf (preamble body _args pdf)
  "Writes PlantUML BODY as PDF, by plantuml, cropped by pdfcrop; a BODY sans @startuml is put within @startuml and
@enduml, so Alice -> Bob: Hi alone suffices.  It has no PREAMBLE."
  (org--image-sans-preamble 'plantuml preamble)
  (let* ((dir (make-temp-file "org-image-" t))
         (puml (expand-file-name "image.puml" dir)))
    (write-region (if (string-match-p "^[ \t]*@start" body) body (format "@startuml\n%s\n@enduml" body)) nil puml)
    (org--image-run "plantuml" "-tpdf" puml)
    (org--image-run "pdfcrop" "--margins" "2" (expand-file-name "image.pdf" dir) pdf)
    (delete-directory dir t)))

(defun org--image-mermaid-pdf (preamble body _args pdf)
  "Writes Mermaid BODY as PDF, by mmdc, the Mermaid CLI, cropped by pdfcrop; it has no PREAMBLE, since Mermaid's
own front matter, between lines of ---, is a part of BODY.  Along HTML and Markdown, no tool is needed: see
`org--image-mermaid-raw'."
  (org--image-sans-preamble 'mermaid preamble)
  (let* ((dir (make-temp-file "org-image-" t))
         (mmd (expand-file-name "image.mmd" dir))
         (drawn (expand-file-name "drawn.pdf" dir)))
    (write-region body nil mmd)
    (org--image-run "mmdc" "--input" mmd "--output" drawn "--pdfFit" "--backgroundColor" "transparent")
    (org--image-run "pdfcrop" "--margins" "2" drawn pdf)
    (delete-directory dir t)))

(defconst org--mermaid-script
  (concat "<script type=\"module\">if (!window.osbeMermaid) { window.osbeMermaid = true;"
          " const { default: mermaid } = await import('https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs');"
          " mermaid.initialize({ startOnLoad: false }); await mermaid.run({ querySelector: 'pre.mermaid' }); }</script>")
  "The script that draws each <pre class=\"mermaid\"> of a page, once, by Mermaid from a CDN; so no tool is needed.")

(defun org--image-mermaid-raw (_language preamble body backend)
  "Returns Mermaid BODY as itself along BACKEND: in HTML, a <pre class=\"mermaid\"> that `org--mermaid-script' draws
in the browser; in Markdown, a fenced mermaid block, which GitHub draws.  It has no PREAMBLE."
  (org--image-sans-preamble 'mermaid preamble)
  (if (eq backend 'md) (format "```mermaid\n%s\n```" body)
    (format "<pre class=\"mermaid\">\n%s\n</pre>\n%s" (org-html-encode-plain-text body) org--mermaid-script)))

(defun org--image-hash (language source args)
  "Returns the hash that names the renderings of a LANGUAGE block of SOURCE and ARGS; for LaTeX, of its preamble too."
  (substring (secure-hash 'sha1 (prin1-to-string (list language source (org-plist-delete (copy-sequence args) :file)
                                                       (and (eq language 'latex)
                                                            (list org-image-block-latex-preamble (org--image-latex-headers))))))
             0 10))

(defun org--image-base (language source args anchor)
  "Returns the path, sans extension, of the renderings of a LANGUAGE block of SOURCE, ARGS, and #+name: ANCHOR;
within `org-image-block-directory', unless the name has a directory of its own:
+ :file tree  ⇒  images/tree, exactly; a figure we mean to link to, which an edit leaves stale till C-c C-c.
+ else ANCHOR, else “image”, then the block's hash  ⇒  images/arrow-1a2b3c4d5e, rendered afresh upon each edit."
  (let ((name (-if-let (file (plist-get args :file)) (file-name-sans-extension (format "%s" file))
                (format "%s-%s" (or anchor "image") (org--image-hash language source args)))))
    (if (file-name-directory name) name (concat (file-name-as-directory org-image-block-directory) name))))

(defun org--image-render (language source args base)
  "Renders a LANGUAGE block of SOURCE and ARGS as BASE.pdf, .svg, and .png; the PNG at ARGS' :resolution, else
`org-image-block-png-resolution'.  Sans :file, the renderings of its former selves ---BASE with another hash--- are
axed, so edits leave no stale files behind."
  (let ((render (plist-get (or (alist-get language org-image-block-languages)
                               (user-error "#+begin_image knows no %s; see `org-image-block-languages'" language))
                           :render))
        (pdf (expand-file-name (concat base ".pdf"))))
    (make-directory (file-name-directory pdf) t)
    (unless (plist-get args :file)
      (dolist (stale (file-expand-wildcards (concat (substring (expand-file-name base) 0 -10) "??????????.*")))
        (unless (string-prefix-p (expand-file-name base) stale) (delete-file stale))))
    (pcase-let ((`(,preamble . ,body) (org--image-split source)))
      (funcall render preamble body args pdf))
    (org--image-run "pdftocairo" "-svg" pdf (expand-file-name (concat base ".svg")))
    (org--image-run "pdftocairo" "-png" "-singlefile" "-r"
                    (format "%s" (or (plist-get args :resolution) org-image-block-png-resolution))
                    pdf (expand-file-name base))))

(defun org--image-file (language source args anchor extension)
  "Returns the EXTENSION rendering, such as \"svg\", of a LANGUAGE block of SOURCE, ARGS, and ANCHOR; rendered now, if
need be.  Should the tools be lacking, nil, with a warning, during export; else an error."
  (let* ((base (org--image-base language source args anchor))
         (file (concat base "." extension)))
    (if (file-exists-p file) file
      (condition-case err
          (progn (org--image-render language source args base) file)
        (user-error (if (not org-export-current-backend) (signal (car err) (cdr err))
                      (display-warning 'org-special-block-extras
                                       (format "A #+begin_image %s block is shown as source, unrendered: %s"
                                               language (cadr err)))
                      nil))))))

;;;; The block

(defun org--image-raw (language preamble body &optional _backend)
  "Returns a LANGUAGE block's PREAMBLE and BODY along its own backend, raw: in LaTeX, BODY, with PREAMBLE in the
document's preamble; in HTML, PREAMBLE then BODY, as a <style> or <script> works within <body> too."
  (if (eq language 'latex)
      (progn (unless (string-empty-p preamble) (cl-pushnew preamble org--latex-head-used :test #'equal))
             body)
    (string-trim (concat preamble "\n" body))))

(org-defblock image (&optional (language "latex") &key file css width layout alt resolution as-image
                     :latex-head ("graphicx") :manual "#images"
                     :hover "Above a line of five dashes, -----, is the block's preamble: LaTeX's, or HTML's <head>.
Naming its renderings, images/NAME.{svg,pdf,png}:
+ Sans :file, NAME is the #+name:, else image, then the block's hash: an edit renders it afresh, its
  former renderings axed; yet the name changes with each edit, so link to it from nowhere else.
+ :file tree names them exactly, a figure we mean to link to, from a README, say; yet an edit leaves
  it stale till C-c C-c renders it anew.
:as-image t is an image even along its own backend: its preamble, such as a heavy package, stays out
of the document's; yet a PDF picture's text is not searchable, nor in the document's font, and an
HTML one is not interactive.  :resolution 300 sharpens its PNG; `org-image-block-shell-escape' allows
minted and the like, at the price of letting the LaTeX run shell commands."
                     &context backend raw-contents anchor)
  "LaTeX, HTML, SVG, dot, PlantUML, or Mermaid LANGUAGE: itself along a backend that reads it; else an image of itself.
In HTML, an SVG, inlined; in LaTeX, a PDF; in Markdown, and elsewhere, a PNG.  C-c C-c renders them, else export
does, within `org-image-block-directory'; see `org--image-base'.  Above a line of dashes is the block's preamble,
see `org--image-split'.  For HTML, CSS styles it and WIDTH, such as 600px, fixes its width; ALT describes the image,
for screen readers; RESOLUTION, in pixels per inch, sharpens its PNG; AS-IMAGE t makes it an image along its own
backend too.  For dot, LAYOUT, such as neato, chooses the layout engine.  See `org-image-block-languages'."
  (let* ((language (intern (downcase (format "%s" language))))
         (source (string-trim raw-contents))
         (args (list :file file :css css :width width :layout layout :resolution resolution))
         (markdown? (org-export-markdown-p))
         (alt (or alt (format "%s" (or anchor (format "Rendered %s" (pcase language ('latex "LaTeX") ('dot "Graphviz") ('plantuml "PlantUML")
                                                             ('mermaid "Mermaid") (_ (upcase (symbol-name language)))))))))
         (extension (cond (markdown? "png") ((eq backend 'html) "svg") ((eq backend 'latex) "pdf") (t "png"))))
    (if (and (memq (if markdown? 'md backend) (plist-get (alist-get language org-image-block-languages) :native))
             (not as-image))
        (pcase-let ((`(,preamble . ,body) (org--image-split source)))
          (funcall (or (plist-get (alist-get language org-image-block-languages) :raw) #'org--image-raw)
                   language preamble body (if markdown? 'md backend)))
      (-if-let (path (org--image-file language source args anchor extension))
          (pcase extension
            ("svg" (format "<img class=\"osbe-image\" alt=\"%s\" src=\"data:image/svg+xml;base64,%s\">"
                           (org-html-encode-plain-text alt)
                           (with-temp-buffer (insert-file-contents-literally path)
                                             (base64-encode-region (point-min) (point-max) t) (buffer-string))))
            ("pdf" (format "\\includegraphics{%s}" (file-relative-name path)))
            (_ (if markdown? (format "<img alt=\"%s\" src=\"%s\">" (org-html-encode-plain-text alt) (file-relative-name path))
                 (org-parse (format "[[file:%s]]" (file-relative-name path))))))
        (org-parse (format "#+begin_example\n%s\n#+end_example" (org-escape-code-in-string raw-contents)))))))

(org-defblock-attribute image :example "
# In HTML, a crisp SVG of the diagram; in a PDF, the diagram itself, its package in the PDF's preamble.
#+begin_image latex
\\usepackage{smartdiagram}
-----
\\smartdiagram[bubble diagram]{Emacs, Org, \\LaTeX, HTML}
#+end_image")

;;;; C-c C-c ---render, and show the image atop the block

(defun org--image-preview (language source args anchor)
  "Renders the block at point, LANGUAGE SOURCE with ARGS and ANCHOR, afresh, and shows its PNG atop the block; if one
is shown already, remove it instead.  An edit to the block removes it too."
  (let* ((block (org-special-block-at-point))
         (beg (org-element-property :post-affiliated block))
         (end (save-excursion (goto-char (org-element-property :end block)) (skip-chars-backward " \t\n") (point)))
         (shown (--first (overlay-get it 'org-image-preview) (overlays-in beg end))))
    (if shown (delete-overlay shown)
      (let ((base (org--image-base language source args anchor))
            (overlay (make-overlay beg end)))
        (org--image-render language source args base)
        (overlay-put overlay 'org-image-preview t)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'modification-hooks (list (lambda (o after &rest _) (when after (delete-overlay o)))))
        (overlay-put overlay 'display (create-image (expand-file-name (concat base ".png")) 'png nil :ascent 'center))
        (message "Rendered %s.{svg,pdf,png}" (file-relative-name base))))
    t))

(org-def-action image (&optional (language "latex") &key file css width layout resolution)
  "Render the block as SVG, PDF, and PNG; show the PNG atop it; again, to remove it.  See `org--image-preview'."
  (org--image-preview (intern (downcase (format "%s" language))) (string-trim raw-contents)
                      (list :file file :css css :width width :layout layout :resolution resolution)
                      (org-element-property :name (org-special-block-at-point))))

(provide 'org-image-block)
;;; org-image-block.el ends here
