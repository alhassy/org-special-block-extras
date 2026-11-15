;;; org-image-block.el --- #+begin_image LANG: LaTeX or HTML, as itself along its own backend, else as an image  -*- lexical-binding: t; -*-

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

;; A TikZ picture in a web page, or an HTML widget in a PDF:
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
;; headless Chrome--- whence pdftocairo makes the SVG and PNG; see `org-image-block-languages'.
;;
;; C-c C-c renders the images, and shows one atop the block; export uses them.  Their names hold a hash
;; of the block, so an edited block is rendered afresh; one never rendered is rendered upon export, when
;; the tools are at hand, else shown as source with a warning.
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
  "The resolution, in pixels per inch, of the PNGs that #+begin_image blocks render to; 144 is crisp on HiDPI."
  :type 'integer
  :group 'org-special-block-extras)

(defvar org-image-block-languages
  '((latex . org--image-latex-pdf)
    (html . org--image-html-pdf))
  "Each (LANGUAGE . RENDER) of #+begin_image: RENDER writes the block, SOURCE, as a cropped PDF, by (RENDER SOURCE
ARGS PDF), ARGS being the block's keyword arguments; whence the SVG and PNG are made.  A new language is one entry.")

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

(defun org--image-latex-pdf (source _args pdf)
  "Writes LaTeX SOURCE as PDF, cropped to it by the standalone class: `org-image-block-latex-preamble', then the
document's #+latex_header: lines; by its #+latex_compiler:, else `org-latex-compiler'."
  (let* ((dir (make-temp-file "org-image-" t))
         (compiler (or (cadar (org-collect-keywords '("LATEX_COMPILER"))) org-latex-compiler "pdflatex"))
         (headers (org--image-latex-headers)))
    (with-temp-file (expand-file-name "image.tex" dir)
      (insert "\\documentclass[border=2pt,varwidth]{standalone}\n" org-image-block-latex-preamble "\n" headers
              "\n\\begin{document}\n" source "\n\\end{document}\n"))
    (let ((default-directory (file-name-as-directory dir)))
      (org--image-run compiler "-interaction=nonstopmode" "-halt-on-error" "image.tex"))
    (copy-file (expand-file-name "image.pdf" dir) pdf t)
    (delete-directory dir t)))

(defun org--image-html-pdf (source args pdf)
  "Writes HTML SOURCE as PDF, by `org-image-block-browser', headless, cropped by pdfcrop; ARGS' :css styles it, and
its :width, such as 600px, fixes its width.  The page is transparent, sans margins, its colours printed as shown."
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
              "</style></head><body>\n" source "\n</body></html>"))
    (org--image-run org-image-block-browser "--headless=new" "--disable-gpu" "--no-pdf-header-footer"
                    (concat "--print-to-pdf=" printed) (concat "file://" html))
    (org--image-run "pdfcrop" "--margins" "0" printed pdf)
    (delete-directory dir t)))

(defun org--image-hash (language source args)
  "Returns the hash that names the renderings of a LANGUAGE block of SOURCE and ARGS; for LaTeX, of its preamble too."
  (substring (secure-hash 'sha1 (prin1-to-string (list language source (org-plist-delete (copy-sequence args) :file)
                                                       (and (eq language 'latex)
                                                            (list org-image-block-latex-preamble (org--image-latex-headers))))))
             0 10))

(defun org--image-base (language source args anchor)
  "Returns the path, sans extension, of the renderings of a LANGUAGE block of SOURCE, ARGS, and #+name: ANCHOR:
its :file, else ANCHOR, else “image”, then the block's hash; within `org-image-block-directory', unless :file has a
directory of its own.  E.g., \"images/tree-1a2b3c4d5e\"."
  (let ((name (format "%s-%s" (file-name-sans-extension (format "%s" (or (plist-get args :file) anchor "image")))
                      (org--image-hash language source args))))
    (if (file-name-directory name) name (concat (file-name-as-directory org-image-block-directory) name))))

(defun org--image-render (language source args base)
  "Renders a LANGUAGE block of SOURCE and ARGS as BASE.pdf, .svg, and .png; axing the renderings of its former
selves, BASE sans its hash, so edits leave no stale files behind."
  (let ((render (or (alist-get language org-image-block-languages)
                    (user-error "#+begin_image knows no %s; see `org-image-block-languages'" language)))
        (pdf (expand-file-name (concat base ".pdf"))))
    (make-directory (file-name-directory pdf) t)
    (dolist (stale (file-expand-wildcards (concat (substring (expand-file-name base) 0 -10) "??????????.*")))
      (unless (string-prefix-p (expand-file-name base) stale) (delete-file stale)))
    (funcall render source args pdf)
    (org--image-run "pdftocairo" "-svg" pdf (expand-file-name (concat base ".svg")))
    (org--image-run "pdftocairo" "-png" "-singlefile" "-r" (format "%s" org-image-block-png-resolution)
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

(org-defblock image (&optional (language "latex") &key file css width alt
                     :latex-head ("graphicx") :manual "#images"
                     &context backend raw-contents anchor)
  "LaTeX, or HTML, LANGUAGE: itself, raw, along its own backend; else an image of itself.
In HTML, an SVG, inlined; in LaTeX, a PDF; in Markdown, and elsewhere, a PNG.  C-c C-c renders them, else export
does, as `org-image-block-directory' FILE, else the block's #+name:, then its hash.  For HTML, CSS styles it and
WIDTH, such as 600px, fixes its width; ALT describes the image, for screen readers."
  (let* ((language (intern (downcase (format "%s" language))))
         (source (string-trim raw-contents))
         (args (list :file file :css css :width width))
         (markdown? (org-export-markdown-p))
         (alt (or alt (format "%s" (or anchor (format "Rendered %s" (if (eq language 'latex) "LaTeX" (upcase (symbol-name language))))))))
         (extension (cond (markdown? "png") ((eq backend 'html) "svg") ((eq backend 'latex) "pdf") (t "png"))))
    (if (and (eq language backend) (not markdown?)) raw-contents
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
# In HTML, a crisp SVG of the picture; in a PDF, the picture itself.  C-c C-c shows it here.
#+begin_image latex
\\begin{tikzpicture}
  \\draw[->, thick] (0,0) -- (2,1) node[right] {$f(x)$};
\\end{tikzpicture}
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

(org-def-action image (&optional (language "latex") &key file css width)
  "Render the block as SVG, PDF, and PNG; show the PNG atop it; again, to remove it.  See `org--image-preview'."
  (org--image-preview (intern (downcase (format "%s" language))) (string-trim raw-contents) (list :file file :css css :width width)
                      (org-element-property :name (org-special-block-at-point))))

(provide 'org-image-block)
;;; org-image-block.el ends here
