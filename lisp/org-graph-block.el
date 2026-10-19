;;; org-graph-block.el --- #+begin_graph: Org lists → Graphviz dot  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, graphviz, dot, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides a #+begin_graph … #+end_graph special block that compiles
;; native Org syntax (nested lists + two-column tables) into Graphviz dot.
;;
;; Syntax overview:
;;
;;   #+macro: gold "#f9e2af"       ; Org's own macros: | color | {{{gold}}} |
;;   #+begin_graph Title :rankdir LR :node.shape box :bgcolor #1e1e2e :wrap 20
;;                                 ; title = graph label; :KEY graph attrs; :directed nil ⇒ undirected; labels fill to 20
;;                                 ; :notes inline ⇒ notes beneath labels; :unflatten 3 ⇒ leaves stagger over 3 ranks;
;;                                 ; :unknown-tags random-templates ⇒ an unknown :tag: gets a made-up template
;;                                 ; :smartdiagram constellation ⇒ LaTeX's smartdiagram typesets the list instead
;;   #+graph:                      ; global graph[ ] attrs
;;   | rankdir | TB      |
;;   | bgcolor | #1e1e2e |
;;
;;   #+node:                       ; default node[ ] attrs
;;   | shape | box |
;;
;;   #+edge:                       ; default edge[ ] attrs
;;   | color | #6c7086 |
;;
;;   #+rank: lam [[combinatory]]   ; { rank=same; lam; combinatory }
;;
;;   #+name: faint                 ; a style template, for :faint: tags and :inherit rows; also outside graphs
;;   | style | dotted |
;;
;;   - :faint: :: inferred         ; a legend entry: a sample of template faint, captioned inferred
;;
;;   + <<lam>> *lambda*             ; <<id>>; *bold* /italic/ _u_ +s+ =code= ~pre~ ⇒ HTML label
;;     | shape          | ellipse |  ; node attrs; values are quoted for you; | :id | lam | ≡ <<lam>>
;;     | :children-rank | same    |  ; { rank=same; <immediate children> }
;;     + church \\ ℕ, Bool, Pairs    ; nesting = edge lambda→church; \\ breaks the label's line
;;       | :edge.style | dotted |    ; arc attrs; a parallel arc is a second child item
;;     + lisp [[church][cross]]     ; [[TARGET]] or [[TARGET][LABEL]]: an extra arc
;;     + calls :: eval :faint:      ; arc lambda→eval labelled calls; :faint: splices that template
;;     + apply [fn::of a closure]   ; an inline footnote is the node's tooltip
;;
;;       Applies a /closure/.       ; so is a note: a later paragraph; `#+begin_note :edge' … #+end_note, the arc's
;;       #+node:                    ; an item's #+node:, #+edge:, and #+rank: reach its whole subtree
;;       | shape | box |
;;       | :edge[church].style | dotted |  ; attrs of this item's arc to church
;;       + [[combinatory]]          ; a lone link is a reference: an arc to a node declared elsewhere
;;       + [[combinatory]] :faint:  ; a reference's tags style its arc; "rec::port" reaches a record's port
;;     + [X] Org [[https://orgmode.org]]  ; a web or [[*Section]] link is the node's URL; [[file:x.png]] its image;
;;                                        ; [X] applies :checked:
;;   + combinatory
;;     1. S                         ; ordered children: one rank, in the order written
;;     2. K
;;   #+begin_cluster Tools :bgcolor grey  ; a box around its items; titled, with attributes, as #+begin_graph is
;;   + emacs
;;   #+end_cluster
;;   #+end_graph
;;
;; Pressing C-c C-c anywhere inside the block renders a PNG immediately; hovering over a node shows its tooltip,
;; and clicking it leads to its link's target, else to its item.
;; C-M-i completes, and eldoc explains, table keys and #+begin_graph keys; in any Org buffer's graph
;; blocks, and in the `org-graph-edit-mode' buffer C-c ' opens.  The attributes live in org-graph-attributes.el.
;; Use #+begin_graph :file foo.png, or a #+name: foo line above it, to fix the output filename.

;;; Code:

(require 'org-special-block-extras)
(require 'org-graph-dot)
(require 'org-graph-edit)
(require 'org-graph-smartdiagram)
(require 'xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-c C-c integration

(defun org-graph--insert-results (end-point payload)
  "Insert \"#+RESULTS:\\nPAYLOAD\" on the line after END-POINT, replacing the whole #+RESULTS: element already there.
#+RESULTS: is an affiliated keyword, so its element spans the result proper ---say, a multi-line src block--- too."
  (save-excursion
    (goto-char end-point)
    (forward-line 1)
    (when (looking-at-p "[ \t]*#\\+RESULTS:")
      ;; Its blank lines after belong to what follows, so they stay.
      (delete-region (point) (save-excursion (goto-char (org-element-property :end (org-element-at-point)))
                                             (skip-chars-backward " \t\n")
                                             (forward-line 1)
                                             (point))))
    (insert (format "#+RESULTS:\n%s\n" payload))))

(defun org-graph--run-dot (dot format outfile &optional map-file)
  "(DOT, FORMAT, OUTFILE) ↦ (EXIT-CODE . STDERR) of running `dot -TFORMAT' on DOT source, writing OUTFILE;
and, given MAP-FILE, its client-side image map there too, from the same layout."
  (let ((dotfile  (make-temp-file "org-graph-" nil ".dot"))
        (err-file (make-temp-file "org-graph-err-")))
    (unwind-protect
        (progn
          (write-region dot nil dotfile)
          (cons (apply #'call-process "dot" nil (list nil err-file) nil (concat "-T" format) dotfile "-o" outfile
                       (when map-file (list "-Tcmapx" "-o" map-file)))
                (with-temp-buffer
                  (insert-file-contents err-file)
                  (s-trim (buffer-string)))))
      (delete-file dotfile)
      (delete-file err-file))))

(defun org-graph--annotate-dot-error (err-str dot)
  "Enrich ERR-STR from dot with offending lines from DOT and debug hints.
Collects every “line N” reference in ERR-STR, extracts those lines, and
appends a structured debug footer."
  (let* ((src-lines  (split-string dot "\n"))
         (n-lines    (length src-lines))
         (offenders
          (--keep (let ((n (string-to-number (cadr it))))
                    (when (<= 1 n n-lines) (format "  line %d: %s" n (s-trim (nth (1- n) src-lines)))))
                  (s-match-strings-all "line \\([0-9]+\\)" err-str)))
         (debug-footer
          (string-join
           '("\nTo debug further:"
             "  • dot -v -Tpng <file>.dot -o /dev/null   (verbose layout trace)"
             "  • dot -V                                   (check Graphviz version)"
             "  • C-u C-c C-c on the block                (inspect generated dot source)")
           "\n")))
    (concat err-str
            (when offenders
              (concat "\n\nOffending dot lines:\n" (string-join offenders "\n")))
            debug-footer)))

(defun org-graph--show-dot (dot)
  "Display DOT source string in a dedicated *org-graph dot* buffer."
  (let ((buf (get-buffer-create "*org-graph dot*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert dot)
      (when (fboundp 'graphviz-dot-mode) (graphviz-dot-mode))
      (goto-char (point-min)))
    (display-buffer buf)))

(defun org--graph-expand-macros (raw)
  "RAW with each {{{NAME(ARGS)}}} ↦ its expansion by this buffer's #+macro: lines; e.g. {{{gold}}} ↦ \"#f9e2af\".
Textually, so that #+begin_cluster lines, which Org parses no objects in, expand too."
  (if (not (s-contains-p "{{{" raw))
      raw
    (require 'org-macro)
    (org-macro-initialize-templates)
    (let ((templates org-macro-templates))
      (replace-regexp-in-string
       "{{{\\([[:alpha:]][-[:alnum:]_]*\\)\\(?:(\\([^\0]*?\\))\\)?}}}"
       (lambda (macro)
         (let ((key (downcase (match-string 1 macro))) (args (match-string 2 macro)))
           ;; Expanding matches regexps of its own; the replacement needs ours.
           (save-match-data
             (or (org-macro-expand (list 'macro (list :key key :args (and args (org-macro-extract-arguments args))))
                                   templates)
                 (user-error "graph: {{{%s}}} names no #+macro:" key)))))
       raw t t))))

(defun org-graph--named-file (anchor svg)
  "(ANCHOR, SVG) ↦ the file a graph named ANCHOR renders to, e.g. \"lineage\" ↦ \"lineage.png\", or .svg; nil sans ANCHOR."
  (when anchor (format "%s.%s" anchor (if svg "svg" "png"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A rendered image is clickable: its nodes lead to their items, or to their links

(defun org-graph--cmapx->map (cmapx scale)
  "A Graphviz CMAPX image map ↦ an Emacs image :map, its coordinates times SCALE; each area shows its title on hover.
  <area shape=\"rect\" href=\"org-graph:3\" title=\"A\" coords=\"1,2,3,4\"/>
    ↦  (((rect (1 . 2) 3 . 4) org-graph-area (help-echo \"A\" pointer hand org-graph-href \"org-graph:3\")))
Every area has one id, `org-graph-area', so one key binding serves them all; see `org-graph-follow-area'."
  (--map (let* ((area (car it))
                (attr (lambda (key) (and (string-match (format "\\_<%s=\"\\([^\"]*\\)\"" key) area)
                                         (xml-substitute-special (match-string 1 area)))))
                (cs (--map (round (* scale (string-to-number it))) (split-string (funcall attr "coords") ","))))
           (list (pcase (funcall attr "shape")
                   ("rect"   `(rect (,(nth 0 cs) . ,(nth 1 cs)) . (,(nth 2 cs) . ,(nth 3 cs))))
                   ("circle" `(circle (,(nth 0 cs) . ,(nth 1 cs)) . ,(nth 2 cs)))
                   (_        (cons 'poly (vconcat cs))))
                 'org-graph-area
                 (list 'help-echo (-some->> (funcall attr "title") (replace-regexp-in-string "\\\\[nlr]" "\n"))
                       'pointer 'hand 'org-graph-href (funcall attr "href"))))
         (s-match-strings-all "<area [^>]*>" cmapx)))

(defun org-graph--in-polygon-p (vertices x y)
  "Non-nil when (X, Y) lies inside the polygon of VERTICES, [x₀ y₀ x₁ y₁ …]: a ray from it crosses an odd number
of edges."
  (let ((n (/ (length vertices) 2)) inside)
    (dotimes (i n inside)
      (let ((xi (aref vertices (* 2 i)))             (yi (aref vertices (1+ (* 2 i))))
            (xj (aref vertices (* 2 (mod (1- i) n)))) (yj (aref vertices (1+ (* 2 (mod (1- i) n))))))
        (when (and (not (eq (> yi y) (> yj y)))
                   (< x (+ xi (/ (* (- xj xi) (- y yi)) (float (- yj yi))))))
          (setq inside (not inside)))))))

(defun org-graph--area-at (map x y)
  "(MAP, X, Y) ↦ the first entry of the image MAP whose area holds pixel (X, Y); else nil.
Graphviz lists nodes before clusters, so a node wins over the box around it."
  (--first (pcase (car it)
             (`(rect (,x0 . ,y0) . (,x1 . ,y1)) (and (<= x0 x x1) (<= y0 y y1)))
             (`(circle (,cx . ,cy) . ,r)         (<= (+ (expt (- x cx) 2) (expt (- y cy) 2)) (* r r)))
             (`(poly . ,vertices)                (org-graph--in-polygon-p vertices x y)))
           map))

(defun org-graph--attach-map (end-point png map-file origin)
  "Make PNG's inline image, in the #+RESULTS: after END-POINT, clickable by the areas of MAP-FILE, a Graphviz cmapx;
ORIGIN marks the line its hrefs count from.  The map is scaled as the image is; Emacs keeps it so when it rescales."
  (-when-let* ((ov (--first (overlay-get it 'org-image-overlay)
                            (overlays-in end-point (save-excursion (goto-char end-point) (forward-line 3) (point)))))
               (image (overlay-get ov 'display))
               (cmapx (with-temp-buffer (insert-file-contents map-file) (buffer-string))))
    (let ((scale (/ (float (car (image-size image t))) (car (image-size (create-image png nil nil :scale 1) t)))))
      (setf (image-property image :map) (org-graph--cmapx->map cmapx scale)
            (image-property image :original-map) (org-graph--cmapx->map cmapx 1)
            (image-property image :org-graph-origin) origin)
      (overlay-put ov 'display image))))

(defun org-graph-follow-area (event)
  "Follow the node or cluster clicked, in EVENT, on a graph's rendered image: to its link's target, when its item has a
web, file, or [[*Heading]] link; else to its item in the #+begin_graph block."
  (interactive "e")
  (-let* ((posn    (event-start event))
          (image   (posn-image posn))
          ((x . y) (posn-object-x-y posn))
          (href    (plist-get (nth 2 (org-graph--area-at (image-property image :map) x y)) 'org-graph-href)))
    (select-window (posn-window posn))
    (cond
     ((null href) nil)
     ((string-match "\\`org-graph:\\([0-9]+\\)\\'" href)
      (goto-char (image-property image :org-graph-origin))
      (forward-line (1- (string-to-number (match-string 1 href))))
      (back-to-indentation)
      (org-reveal))
     ;; Graphviz knows a file link only by its path; a bare path is a file: link again.
     (t (org-link-open-from-string
         (org-link-make-string (if (string-match-p "\\`\\(?:[*#]\\|[[:alpha:]][[:alnum:]+.-]*:\\)" href) href
                                 (concat "file:" href))))))))

;; A hot spot's click arrives as [org-graph-area mouse-1], ahead of the image's own overlay keymap.
(define-key org-mode-map [org-graph-area mouse-1] #'org-graph-follow-area)
(define-key org-mode-map [org-graph-area down-mouse-1] #'ignore)

(defun org-graph--body-origin ()
  "Point on a #+begin_graph line ↦ a marker at its body's first non-blank line: line 1, as `org--graph-read' counts the
trimmed body that `org-special-block-contents' yields."
  (save-excursion (forward-line 1) (skip-chars-forward " \t\n") (copy-marker (line-beginning-position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-graph--cc-handler ()
  "Render the #+begin_graph block at point, or show its dot source.

With no prefix argument and no :C-c_C-c kwdarg (or :C-c_C-c png):
  renders a PNG and inserts a #+RESULTS: [[file:…]] link, its image clickable: hovering over a node shows its tooltip,
  and clicking it follows it, as `org-graph-follow-area' says.

With C-u prefix, or when the block carries :C-c_C-c dot:
  generates dot source, displays it in *org-graph dot*, and inserts a
  #+RESULTS: #+begin_src dot … #+end_src block.

The two controls compose via XOR: if :C-c_C-c dot is set, C-u toggles
back to PNG.  On a checkbox, it first toggles it, as Org's C-c C-c would.
Returns t (claiming C-c C-c)."
  (when (org-at-item-checkbox-p) (org-toggle-checkbox))
  (save-excursion
    ;; The graph around point ---past any #+begin_cluster within it--- not merely the nearest #+begin_graph above it.
    (-some--> (org-graph--block-at-point) (org-element-property :post-affiliated it) (goto-char it))
    (let* ((origin     (org-graph--body-origin))
           (block      (org-special-block-after-point "graph"))
           (kwdargs    (org-special-block-kwdargs block))
           (raw        (org--graph-expand-macros (org-special-block-contents block)))
           (main-arg   (org-special-block-main-arg block))
           (end-pt     (org-special-block-end-point block))
           (block-mode (plist-get kwdargs :C-c_C-c))
           (show-dot?  (xor (equal (format "%s" block-mode) "dot") current-prefix-arg))
           (smart      (plist-get kwdargs :smartdiagram))
           (dot        (if smart (org--smartdiagram raw main-arg kwdargs) (org--graph-raw->dot raw main-arg kwdargs)))
           (lang       (if smart "latex" "dot")))
      (if show-dot?
          (progn
            (org-graph--show-dot dot)
            (org-graph--insert-results end-pt (format "#+begin_src %s\n%s\n#+end_src" lang dot))
            (message "Graph %s source → *org-graph dot*" lang))
        (let ((outfile (or (-some->> (plist-get kwdargs :file) (format "%s"))
                           (org-graph--named-file (org-special-block-anchor block) nil)
                           (expand-file-name
                            (format "graph-%s.png" (substring (md5 dot) 0 12))
                            temporary-file-directory))))
          (if smart
              (-let [(exit . err-str) (org-graph--run-latex dot "png" outfile (org-graph--dpi kwdargs))]
                (unless (zerop exit)
                  (user-error "pdflatex failed: %s" (org-graph--annotate-latex-error err-str)))
                (org-graph--insert-results end-pt (format "[[file:%s]]" outfile))
                (when (fboundp 'org-display-inline-images)
                  (org-display-inline-images nil t end-pt (save-excursion (goto-char end-pt) (forward-line 3) (point))))
                (message "Graph rendered → %s" (expand-file-name outfile)))
          ;; The image renders from dot with hrefs, so its map leads back to the items; the plain dot names the file.
          (let ((map-file (make-temp-file "org-graph-" nil ".map")))
            (unwind-protect
                (-let [(exit . err-str) (let ((org--graph-hrefs t))
                                          (org-graph--run-dot (org--graph-raw->dot raw main-arg kwdargs) "png" outfile
                                                              map-file))]
                  (if (zerop exit)
                      (progn
                        (org-graph--insert-results end-pt (format "[[file:%s]]" outfile))
                        ;; Only the result's image, so its preview is immediate rather than queued behind others'.
                        (when (fboundp 'org-display-inline-images)
                          (org-display-inline-images nil t end-pt
                                                     (save-excursion (goto-char end-pt) (forward-line 3) (point)))
                          (org-graph--attach-map end-pt outfile map-file origin))
                        (message "Graph rendered → %s" (expand-file-name outfile)))
                    (user-error "dot failed: %s"
                                (if (s-blank-str-p err-str)
                                    "is Graphviz installed? (M-x shell-command: dot -V)"
                                  (org-graph--annotate-dot-error err-str dot)))))
              (delete-file map-file)))))))
    t))

(org-def-action graph ()
  "Render the graph at point, or show its dot source; see `org-graph--cc-handler'."
  (org-graph--cc-handler))

(org-def-action cluster ()
  "Within a graph's #+begin_cluster, render the graph; elsewhere, a cluster block is none of ours."
  (and (org-graph--block-at-point) (org-graph--cc-handler)))

(org-def-action note ()
  "Within a graph, a #+begin_note is a tooltip, so render the graph; elsewhere, a note keeps its :C-c_C-c switch."
  (and (org-graph--block-at-point) (org-graph--cc-handler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-defblock registration

(defun org-graph--render (dot kind outfile)
  "(DOT, KIND, OUTFILE) ↦ OUTFILE, once `dot -TKIND' writes it; else nil, the error logged to *Warnings*."
  (-let [(exit . err-str) (org-graph--run-dot dot kind outfile)]
    (if (zerop exit)
        outfile
      (warn "org-graph: dot failed rendering %s:\n%s" outfile (org-graph--annotate-dot-error err-str dot))
      nil)))

(defun org-graph--dpi (kwdargs)
  "KWDARGS ↦ the dots per inch a smartdiagram's PNG renders at: its `:dpi', else 300."
  (or (plist-get kwdargs :dpi) 300))

(defun org-graph--annotate-latex-error (err-str)
  "ERR-STR, pdflatex's errors, ↦ them with a hint at how to see the LaTeX; sans errors, a hint at installing LaTeX."
  (if (s-blank-str-p err-str)
      "is LaTeX installed, with smartdiagram? (M-x shell-command: kpsewhich smartdiagram.sty)"
    (concat err-str "\n\nC-u C-c C-c on the block shows the LaTeX it typesets.")))

(defun org-graph--render-latex (latex kind outfile dpi)
  "(LATEX, KIND, OUTFILE, DPI) ↦ OUTFILE, once pdflatex typesets it as KIND; else nil, the error logged to *Warnings*."
  (-let [(exit . err-str) (org-graph--run-latex latex kind outfile dpi)]
    (if (zerop exit)
        outfile
      (warn "org-graph: pdflatex failed rendering %s:\n%s" outfile (org-graph--annotate-latex-error err-str))
      nil)))

(defun org-graph--png-to-base64 (pngfile)
  "Return a base64 string of the binary contents of PNGFILE."
  (base64-encode-string
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (insert-file-contents-literally pngfile)
     (buffer-string))
   t))

(defun org-graph--svg-contents (svgfile)
  "Return the <svg>…</svg> string from SVGFILE, stripping the XML preamble."
  (with-temp-buffer
    (insert-file-contents svgfile)
    (goto-char (point-min))
    (when (re-search-forward "<svg[ \t\n]" nil t)
      (goto-char (match-beginning 0))
      (buffer-substring (point) (point-max)))))

(defun org-graph--svg-tooltips (svg tips)
  "(SVG, TIPS) ↦ SVG whose tooltips tooltips.js shows, TIPS mapping each key Graphviz wrote as an xlink:title to its Org
text, which follows the <svg> in a <template>, as an <svg> keeps none within.  Graphviz's own titles ---node ids, and a
linked node's label--- vanish, lest the browser's native tooltips compete.
  <a xlink:title=\"osbe-tip-g1\">  ↦  <a class=\"tooltip osbe-tip-g1\" tabindex=\"0\">, then <template id=\"osbe-tip-g1\">…"
  (concat (replace-regexp-in-string
           " xlink:title=\"\\([^\"]*\\)\""
           ;; Graphviz writes a - as &#45;, which decoding, by regexps of its own, would lose our match for.
           (lambda (title) (let ((key (save-match-data (xml-substitute-special (match-string 1 title)))))
                             (if (gethash key tips) (format " class=\"tooltip %s\" tabindex=\"0\"" key) "")))
           (replace-regexp-in-string "<title>[^<]*</title>\n?" "" svg) t t)
          (let (templates)
            (maphash (lambda (key tip) (push (concat "\n" (org--tooltip-template key tip)) templates)) tips)
            (apply #'concat (nreverse templates)))))

(org-defblock graph (&optional title &key file svg &allow-other-keys :edit-indirect-mode org-graph-edit-mode
                    :manual "#Graphs" &context backend raw-contents args anchor)
  "Render an Org list + tables as a Graphviz dot graph.

TITLE is the graph's label.
The header's other keys are graph attributes, such as :rankdir LR; :node.KEY and :edge.KEY set node and edge defaults;
:directed nil makes an undirected graph; :wrap N fills labels to N characters a line; :unflatten N staggers
wide rows of leaves over N ranks; and :unknown-tags random-templates gives an unknown tag a made-up template,
rather than erring.  Pass :file foo.png on the
#+begin_graph line to fix the output filename; else a #+name: foo line above the block makes it foo.png, and the graph
a figure that [[foo]] reaches and #+caption: captions; otherwise the name is graph-HASH.png, HASH being that of the
dot source, so re-exporting an unchanged graph rewrites one file rather than adding another.

For HTML export: embeds an inline <svg> element, whose nodes' and arcs' tooltips hover as prose's do, by tooltips.js;
under :svg nil, an inline base64 PNG, sans tooltips.
For Markdown, it writes the image file and links it, as for a named graph.
For other backends it emits a #+begin_src dot block.  In HTML, a node's [[*Section]] link leads to that section.

With :smartdiagram TYPE ---constellation, flow:horizontal, descriptive, and so on--- LaTeX's smartdiagram package
typesets the list instead, as `org--smartdiagram' says: the #+graph: rows are \\smartdiagramset's, and prose after the
list sits beneath the diagram.  It renders by pdflatex, at :dpi dots per inch, 300 by default; LaTeX export typesets it
in place, adding smartdiagram to the preamble.

Within the block body:

  #+graph: table  ↦ global graph[ ] attributes
  #+node:  table  ↦ default node[ ] attributes
  #+edge:  table  ↦ default edge[ ] attributes
  #+rank:  n1 n2  ↦ { rank=same; n1; n2 } for nodes without a common parent; ids may be [[links]]
  Under an item, #+graph:, #+node:, #+edge:, and #+rank: reach its whole subtree, as Org's inherited properties do.
  #+begin_cluster TITLE :KEY V … #+end_cluster  ↦ subgraph cluster_TITLE { … }; nests, and may sit in an item
  :NAME: … :END:  ↦ a drawer of any name, folding such directives away
  - :T: :: MEANING  ↦ at the top, a legend entry: a sample of template T, captioned MEANING

A #+name: table outside any graph is a template every graph in the buffer may use; the graph's own wins.
A section tagged :graph: exports as a graph of its text, its :PROPERTIES: drawer serving as the header line.

Each list item declares a node.  Nesting encodes edges (parent → child); ordered children, 1. 2. 3., share a rank
in the order written.  An item's text may hold a <<target>>, its id; [[TARGET]] or [[TARGET][LABEL]] links, extra arcs;
and *bold*, /italic/, _underline_, +strike+, =code=, or ~pre~, a braced x^{2} or H_{2}O, or a @@dot:…@@ snippet,
which make its label an HTML-like one; an entity, \\alpha, is its character.  A <<<radio target>>> is a node, every
item mentioning it having an arc to it.  Other Org syntax in a block, such as a headline or #+begin_quote, is an error.
A description tag, `+ L :: CHILD', labels the arc into CHILD; trailing `:T:' tags splice #+name: templates;
an inline footnote, [fn::NOTE], is the node's tooltip, as is a note: a paragraph past the item's first, or a
#+begin_note block ---with `#+begin_note :edge', the arc's---, shown beneath the label instead under :notes inline;
a web, file, or [[*Section]] link, its URL; an [[file:IMAGE.png]], its image;
a checkbox, [X], [-], or [ ], applies the template checked, partial, or unchecked, when one exists; and a `\\\\'
breaks the label's line ---in a table value or #+record: cell, too.
Org's {{{macros}}} expand, by the buffer's #+macro: lines.
A two-column inline table on a list item supplies node attributes.

  | key        | val    |  ↦ a Graphviz attribute of the node; values are quoted for you.
  | :id        | alias  |  ↦ cross-reference name for this node.
  | :edge.KEY  | val    |  ↦ attribute of the arc from the parent; a parallel arc is a second child item.
  | :edge[T].KEY | val  |  ↦ attribute of this item's arc to T, from a [[T]] link; \"rec::port\" reaches a record's port.
  | :inherit   | T₁ T₂  |  ↦ inherit the #+name: templates' rows; explicit rows win.  Also :edge.:inherit, and :T₁:T₂: tags:
                            after an item, its node's; after a reference, or a description term, its arc's.
  A child item that is a lone [[T]] link is a reference to T: an arc to it, styled by the child's :edge.KEY rows.
  | :children-rank | same | ↦ constrain immediate children to the same rank;
                            also accepts min, max, source, sink.
  | shape | cluster |      ↦ make this node an inline subgraph cluster_ID { … }.
  #+record: table       ↦ the node's <TABLE> label, as laid out: a cell's <<p>> is port p, its [[T]] an arc from there;
                            ^ or an empty cell spans, a <l>/<c>/<r> row aligns, and hlines rule.

Example:

  #+begin_graph :rankdir LR
  + A
    + calls :: B
      + returns :: C
  #+end_graph"
  (let* ((smart   (plist-get args :smartdiagram))
         (raw     (org--graph-expand-macros raw-contents))
         ;; HTML inlines a graph's <svg>, for its tooltips, unless told :svg nil; `svg' alone cannot tell nil from omitted.
         (inline-svg (and (eq backend 'html) (not anchor) (not (org-export-markdown-p))
                          (or svg (not (or smart (plist-member args :svg))))))
         (org--graph-tips (and inline-svg (make-hash-table :test #'equal)))
         (dot     (if smart (org--smartdiagram raw title args)
                    (org--graph-raw->dot raw title args)))
         (render  (if smart
                      (lambda (kind file) (org-graph--render-latex dot kind file (org-graph--dpi args)))
                    (lambda (kind file) (org-graph--render dot kind file))))
         ;; The header reads :file foo.png as the symbol foo.png; we want its name.
         (outfile (or (and file (format "%s" file)) (org-graph--named-file anchor svg)
                      (format "graph-%s.png" (substring (md5 dot) 0 12)))))
    (pcase backend
      ;; LaTeX typesets a smartdiagram itself: no image, and its packages join the preamble.
      ((guard (and smart (org-export-derived-backend-p backend 'latex)))
       (dolist (package org-graph-smartdiagram-latex-head) (cl-pushnew package org--latex-head-used :test #'equal))
       (format "\\begin{center}\n%s\n\\end{center}" dot))
      ;; A named graph is a figure: an image link, which the block's #+name: and #+caption: label, as with Babel's results.
      ;; Markdown gets one too, since its renderers strip the inline base64 <img> that HTML gets.
      ((guard (or anchor (org-export-markdown-p)))
       (funcall render (if (s-ends-with-p ".svg" outfile) "svg" "png") outfile)
       (org-parse (format "[[file:%s]]" outfile)))
      (`html
       (let ((tmp (make-temp-file "org-graph-" nil (if inline-svg ".svg" ".png"))))
         (unwind-protect
             ;; An inline <svg> keeps the tooltips; else, an inline base64 PNG.
             (or (when (funcall render (if inline-svg "svg" "png") tmp)
                   (if inline-svg (-some--> (org-graph--svg-contents tmp) (org-graph--svg-tooltips it org--graph-tips))
                     (format "<img src=\"data:image/png;base64,%s\" alt=\"graph\" />" (org-graph--png-to-base64 tmp))))
                 ;; dot unavailable, or its output sans <svg>: emit the raw dot source in a <pre>, its tooltips as text.
                 (format "<pre>%s</pre>" (s-replace "<" "&lt;" (if (and inline-svg (not smart))
                                                                    (let (org--graph-tips)
                                                                      (org--graph-raw->dot raw title args))
                                                                  dot))))
           (delete-file tmp))))
      (_
       ;; `org-parse' lets Org export the src block, rather than emit its raw syntax into the output.
       (org-parse (if smart (format "#+begin_src latex\n%s\n#+end_src" dot)
                    (format "#+begin_src dot :file %s\n%s\n#+end_src" outfile dot)))))))

;; A graph's body is its own syntax ---its #+begin_note blocks are tooltips, not callouts--- so export rewrites it
;; first.
(setq org--supported-blocks (cons 'graph (delq 'graph org--supported-blocks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-graph-block)
;;; org-graph-block.el ends here
