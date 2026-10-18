;;; org-graph-smartdiagram.el --- #+begin_graph :smartdiagram TYPE: an Org list ↦ LaTeX's smartdiagram  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Part of org-graph-block.el: a graph block whose header says `:smartdiagram constellation' is typeset by LaTeX's
;; smartdiagram package, rather than laid out by Graphviz.  The body is the same Org:
;;
;;   #+begin_graph One syntax :smartdiagram constellation  ; the title sits above the diagram
;;   #+graph:                                              ; \smartdiagramset{…}, in smartdiagram's own words
;;   | planet color | black!75 |
;;   + =easy-access.el=                                    ; constellation & bubble: one item, the planet …
;;     | fontcolor | white |                               ; … an item's color, fontcolor, and fontsize rows
;;     + =(:k plist)= \\ replaces =(plist-get plist :k)=   ; … its children, the satellites
;;
;;   -----                                                 ; prose after the list sits beneath the diagram
;;   #+end_graph
;;
;; The keys and diagram types, with their descriptions, are here for completion and eldoc; see org-graph-edit.el.

;;; Code:

(require 'org-graph-parse)
(require 'ox-latex)

(defconst org-graph-smartdiagram-types
  '(("circular"                . "Items round a circle, each with an arrow to the next.")
    ("circular:clockwise"      . "Items round a circle, clockwise.")
    ("flow"                    . "Items top to bottom, each with an arrow to the next, and one back from the last.")
    ("flow:horizontal"         . "Items left to right, each with an arrow to the next, and one back from the last.")
    ("descriptive"             . "Each `+ TERM :: DESCRIPTION' item, a round title beside its description.")
    ("priority-descriptive"    . "Items bottom to top, beside an arrow of rising priority.")
    ("bubble"                  . "One item, a bubble; its children, bubbles overlapping it.")
    ("constellation"           . "One item, a planet; its children, satellites, each with an arrow from it.")
    ("connected-constellation" . "One item, a planet; its children, satellites, each joined to the next.")
    ("sequence"                . "Items in a row of boxes, sans arrows."))
  "The `:smartdiagram' types ↦ what each draws.
A type of two words takes a dash: `connected-constellation' is smartdiagram's “connected constellation diagram”.")

(defconst org-graph-smartdiagram-keys
  '((:name "font" :default "\\small" :description "The font of every item's text.")
    (:name "text color" :default "black" :description "The colour of every item's text.")
    (:name "text width" :default "1.5cm" :description "How wide a flow or circular item's text fills before it breaks.")
    (:name "border color" :default "gray" :description "The colour of each item's border.")
    (:name "set color list" :default "red!40, cyan!40, blue!40, green!40, orange!40, yellow!40, magenta!40, brown!40, violet!40, teal!40"
     :description "The items' colours, in order; an item's | color | row overrides its own.")
    (:name "uniform color list" :description "`C for N items': the first N items all in colour C.")
    (:name "module shape" :default "rnd rectangle" :description "The shape of a flow or circular item."
     :values ("rnd rectangle" "rectangle" "circle" "ellipse" "diamond"))
    (:name "module minimum width" :default "2cm" :description "The least width of a flow or circular item.")
    (:name "module minimum height" :default "1cm" :description "The least height of a flow or circular item.")
    (:name "module x sep" :default "2.75" :description "The distance between items of a horizontal flow, in cm.")
    (:name "module y sep" :default "1.65" :description "The distance between items of a vertical flow, in cm.")
    (:name "circular distance" :default "2.75cm" :description "The radius of a circular diagram.")
    (:name "arrow color" :default "gray" :description "The colour of every arrow, under uniform arrow color.")
    (:name "arrow line width" :default "0.1cm" :description "The thickness of the arrows.")
    (:name "arrow tip" :default "stealth" :description "The tip of the arrows." :values ("stealth" "latex" "to" "triangle 45"))
    (:name "arrow style" :default "<-" :description "The direction of a flow's arrows." :values ("<-" "->" "<->"))
    (:name "uniform arrow color" :default "false" :description "With true, every arrow in arrow color, not its item's." :values ("true" "false"))
    (:name "back arrow disabled" :default "false" :description "With true, a flow has no arrow from its last item back to its first." :values ("true" "false"))
    (:name "back arrow distance" :default "0.5" :description "How far below the items a flow's back arrow runs.")
    (:name "circular final arrow disabled" :default "false" :description "With true, a circular diagram's last item has no arrow to its first." :values ("true" "false"))
    (:name "insert decoration" :description "A TikZ decoration on each item's border, such as `zigzag'.")
    (:name "descriptive items y sep" :default "1.75" :description "The distance between rows of a descriptive diagram, in cm.")
    (:name "description title font" :default "\\small" :description "The font of a descriptive diagram's terms.")
    (:name "description title width" :default "1.5cm" :description "The width of a descriptive diagram's round terms.")
    (:name "description title text width" :default "1.25cm" :description "How wide a term's text fills before it breaks.")
    (:name "description font" :default "\\small" :description "The font of a descriptive diagram's descriptions.")
    (:name "description width" :default "5.5cm" :description "The width of a descriptive diagram's descriptions.")
    (:name "description text width" :default "5cm" :description "How wide a description's text fills before it breaks.")
    (:name "priority arrow width" :default "1.5cm" :description "The width of a priority diagram's arrow.")
    (:name "priority arrow head extend" :default "0.15cm" :description "How far a priority arrow's head juts out.")
    (:name "priority arrow height advance" :default "2cm" :description "How far a priority arrow runs past its items.")
    (:name "priority tick size" :default "5pt" :description "The thickness of the ticks joining items to a priority arrow.")
    (:name "bubble center node color" :default "lightgray!60" :description "The colour of a bubble diagram's middle bubble.")
    (:name "bubble center node font" :default "\\large" :description "The font of a bubble diagram's middle bubble.")
    (:name "bubble center node size" :default "4cm" :description "The size of a bubble diagram's middle bubble.")
    (:name "bubble node font" :default "\\normalfont" :description "The font of the outer bubbles.")
    (:name "bubble node size" :default "2.5cm" :description "The size of the outer bubbles.")
    (:name "bubble fill opacity" :default "0.5" :description "How opaque the outer bubbles are.")
    (:name "bubble text opacity" :default "0.8" :description "How opaque the outer bubbles' text is.")
    (:name "distance center/other bubbles" :default "0.8cm" :description "How far the outer bubbles sit from the middle one.")
    (:name "distance text center bubble" :default "0.5cm" :description "The inner margin of the outer bubbles.")
    (:name "planet color" :default "lightgray!60" :description "The colour of a constellation's planet.")
    (:name "planet font" :default "\\large" :description "The font of a constellation's planet.")
    (:name "planet size" :default "2.5cm" :description "The size of a constellation's planet.")
    (:name "planet text width" :default "1.75cm" :description "How wide the planet's text fills before it breaks.")
    (:name "distance planet-text" :default "0.5cm" :description "The planet's inner margin.")
    (:name "distance planet-connection" :default "0.1cm" :description "The gap between the planet and its arrows.")
    (:name "distance planet-satellite" :default "3.5cm" :description "How far the satellites sit from the planet.")
    (:name "satellite font" :default "\\normalfont" :description "The font of a constellation's satellites.")
    (:name "satellite size" :default "1.75cm" :description "The size of a constellation's satellites.")
    (:name "satellite text width" :default "1.5cm" :description "How wide a satellite's text fills before it breaks.")
    (:name "satellite fill opacity" :default "0.5" :description "How opaque the satellites are.")
    (:name "satellite text opacity" :default "0.8" :description "How opaque the satellites' text is.")
    (:name "distance satellite-connection" :default "0.075cm" :description "The gap between a satellite and its arrow.")
    (:name "connection color" :default "gray" :description "The colour of the planet's arrows, under uniform connection color.")
    (:name "connection line width" :default "0.1cm" :description "The thickness of the planet's arrows.")
    (:name "uniform connection color" :default "false" :description "With true, every arrow of the planet in connection color." :values ("true" "false"))
    (:name "sequence item height" :default "1cm" :description "The height of a sequence's boxes.")
    (:name "sequence item width" :default "2cm" :description "The width of a sequence's boxes.")
    (:name "sequence item text width" :default "1.9cm" :description "How wide a sequence box's text fills before it breaks.")
    (:name "sequence item font size" :default "\\normalfont" :description "The font of a sequence's boxes.")
    (:name "sequence item text color" :default "black" :description "The colour of a sequence's text.")
    (:name "sequence item border color" :default "gray" :description "The colour of a sequence's borders.")
    (:name "sequence item border size" :default "1.75\\pgflinewidth" :description "The thickness of a sequence's borders.")
    (:name "sequence item fill opacity" :default "1" :description "How opaque a sequence's boxes are.")
    (:name "sequence item text opacity" :default "1" :description "How opaque a sequence's text is.")
    (:name "sequence item uniform color" :default "gray!60!black" :description "The one colour of a sequence's boxes, under uniform sequence color.")
    (:name "uniform sequence color" :default "false" :description "With true, every box of a sequence in sequence item uniform color." :values ("true" "false")))
  "The keys of smartdiagram's \\smartdiagramset, as a smartdiagram graph's #+graph: rows take them; from smartdiagram's
own sources.  Each is a plist: its :name, :description, :default when it has one, and :values when they are few.")

(defconst org-graph-smartdiagram-item-keys
  '(("color"     . "This item's colour; for the planet or middle bubble, that of the planet or bubble.")
    ("fontcolor" . "The colour of this item's text; e.g., white, on a dark planet.")
    ("fontsize"  . "The size of this item's text, in points; e.g., 17.")
    (":inherit"  . "Inherit the rows of #+name: templates N₁ … Nₖ; this item's own rows win.  Also spelled :N₁:…:Nₖ: after the item."))
  "The keys of a smartdiagram item's table ↦ what they do; the first three are Graphviz's names for the same things.")

(defconst org-graph-smartdiagram-latex-head '("smartdiagram" "varwidth")
  "What a LaTeX document needs, to typeset a smartdiagram graph: smartdiagram, and varwidth, which centres src blocks.")

(defun org-graph--smartdiagram-key (name)
  "A #+graph: row's key NAME ↦ its `org-graph-smartdiagram-keys' entry; else nil."
  (--find (equal (plist-get it :name) name) org-graph-smartdiagram-keys))

(defun org--smartdiagram-type (type)
  "A `:smartdiagram' TYPE ↦ smartdiagram's name for it, else a user error.
  flow:horizontal  ↦  \"flow diagram:horizontal\"      connected-constellation  ↦  \"connected constellation diagram\""
  (let ((type (format "%s" type)))
    (unless (assoc type org-graph-smartdiagram-types)
      (user-error "graph: `:smartdiagram %s' should be one of %s" type (s-join ", " (mapcar #'car org-graph-smartdiagram-types))))
    (-let [(kind option) (s-split ":" type)]
      (concat (s-replace "-" " " kind) " diagram" (if option (concat ":" option) "")))))

(defun org--smartdiagram-unbreakable (text _backend _info)
  "Export filter: =code= TEXT, already LaTeX, in an \\mbox, so a narrow item never breaks it across lines; the
spaces after it stay outside."
  (string-match "[ \t]*\\'" text)
  (format "\\mbox{%s}%s" (substring text 0 (match-beginning 0)) (match-string 0 text)))

(defun org--smartdiagram-latex (org)
  "ORG text ↦ LaTeX, by Org's own exporter; =code= never breaks across lines, and <<targets>> vanish.
  \"=(:k plist)= \\\\\\\\\\n[[color:gray][replaces]]\"  ↦  \"\\\\mbox{\\\\texttt{(:k plist)}}\\\\\\\\\\n\\\\begingroup\\\\color{gray}replaces\\\\endgroup\\\\,\"
A src block is a centred box as wide as its widest line; a rule spans the diagram, not the page."
  (let ((org-export-with-toc nil)
        (org-latex-src-block-backend 'verbatim)
        (org-export-filter-code-functions (cons #'org--smartdiagram-unbreakable org-export-filter-code-functions))
        (org-export-filter-verbatim-functions (cons #'org--smartdiagram-unbreakable org-export-filter-verbatim-functions))
        (org-export-filter-target-functions (cons (lambda (&rest _) "") org-export-filter-target-functions))
        (org-export-filter-src-block-functions
         (cons (lambda (text &rest _) (format "\\begin{varwidth}{\\linewidth}\n%s\\end{varwidth}\n" text))
               org-export-filter-src-block-functions))
        (org-export-filter-horizontal-rule-functions
         (cons (lambda (text &rest _) (s-replace "\\textwidth" "\\linewidth" text))
               org-export-filter-horizontal-rule-functions)))
    (s-trim (org-export-string-as (s-trim org) 'latex t))))

(defun org--smartdiagram-label (text)
  "An item's or title's TEXT ↦ LaTeX; a `\\\\' breaks the line, mid-line too, as in a graph's labels."
  (org--smartdiagram-latex (org--graph-break-lines (s-trim text))))

(defun org--smartdiagram-term (text)
  "A description item's TEXT ↦ (TERM . DESCRIPTION); a plain item's ↦ (nil . TEXT).  \" T :: D\" ↦ (\"T\" . \"D\")"
  (if (string-match "\\`[ \t]*\\(.*?\\)[ \t]+::\\(?:[ \t]+\\|\n\\|\\'\\)" text)
      (cons (match-string 1 text) (substring text (match-end 0)))
    (cons nil text)))

(defun org--smartdiagram-item (item tables descriptive)
  "(ITEM, TABLES of #+name: templates, DESCRIPTIVE?) ↦ (CELL . COLOR): its smartdiagram argument, and its | color | row.
The item's text, then its notes ---later paragraphs--- beneath; in the item's fontcolor and fontsize.  A descriptive
item's CELL is {{TERM},{DESCRIPTION}}.  Graph-only syntax, such as a #+record: table or an :edge row, is an error."
  (-let* (((text . tags) (org--graph-split-tags (plist-get item :text)))
          (pre   (plist-get item :pre))
          (rows  (org--graph-expand-inherit (append (plist-get pre :attr-rows)
                                                    (when tags `((":inherit" ,(s-join " " tags)))))
                                            tables))
          (row   (lambda (key) (cadr (assoc key rows))))
          ((term . body) (org--smartdiagram-term text)))
    (-each rows (-lambda ((key)) (unless (assoc key org-graph-smartdiagram-item-keys)
                                   (user-error "graph: `%s' means nothing to a smartdiagram item; it takes %s" key
                                               (s-join ", " (butlast (mapcar #'car org-graph-smartdiagram-item-keys)))))))
    (when (plist-get pre :record) (user-error "graph: a smartdiagram item has no #+record: table"))
    (when (--any (plist-get it :edge) (plist-get pre :notes))
      (user-error "graph: a smartdiagram draws no labelled arcs, so a `#+begin_note :edge' has nothing to describe"))
    (cond ((and descriptive (not term))
           (user-error "graph: each item of a descriptive diagram is `+ TERM :: DESCRIPTION'; not `%s'" (s-trim text)))
          ((and term (not descriptive))
           (user-error "graph: `%s ::' labels an arc, yet only a descriptive diagram has terms" term)))
    (let* ((style (concat (or (-some--> (funcall row "fontcolor") (format "\\color{%s}" it)) "")
                          (or (-some--> (funcall row "fontsize")
                                (format "\\fontsize{%s}{%s}\\selectfont " it (* 1.2 (string-to-number it))))
                              "")))
           (cell  (concat style (org--smartdiagram-label body)
                          (mapconcat (lambda (note) (concat "\\\\[0.5em]" (org--smartdiagram-latex (plist-get note :text))))
                                     (plist-get pre :notes) ""))))
      (cons (if descriptive (format "{{%s%s},{%s}}" style (org--smartdiagram-label term) cell) (format "{%s}" cell))
            (funcall row "color")))))

(defun org--smartdiagram-prose (raw)
  "A smartdiagram graph's RAW body ↦ (ABOVE . BELOW), the Org prose before and after its list; tables, directives,
and the list itself aside.  \"Intro\\n+ a\\n-----\"  ↦  (\"Intro\\n\" . \"-----\")"
  (with-temp-buffer
    (insert raw)
    (org-mode)
    (let (above below seen-list)
      (dolist (el (org--graph-elements (org-element-contents (org-element-parse-buffer))))
        (pcase (org-element-type el)
          ('plain-list (setq seen-list t))
          ((or 'table 'keyword) nil)
          ((and 'special-block (guard (member (downcase (org-element-property :type el)) '("cluster" "note")))) nil)
          (_ (let ((text (buffer-substring-no-properties (org-element-property :begin el) (org-element-property :end el))))
               (if seen-list (push text below) (push text above))))))
      (cons (s-join "" (nreverse above)) (s-join "" (nreverse below))))))

(defun org--smartdiagram (raw-contents main-arg kwdargs)
  "(RAW-CONTENTS, MAIN-ARG, KWDARGS) ↦ the LaTeX typesetting a `#+begin_graph … :smartdiagram TYPE' block.

MAIN-ARG, the title, sits above the diagram, as does the body's prose before its list; its prose after the list sits
beneath.  The #+graph: rows are \\smartdiagramset's; the items are the diagram's, as TYPE reads them:
  constellation, connected-constellation, bubble  ↦  one item, the planet; its children, the satellites
  descriptive                                     ↦  `+ TERM :: DESCRIPTION' items
  otherwise                                       ↦  the items, in order
#+node:, #+edge:, and #+rank: are Graphviz's, so they are errors here; as is any nesting TYPE does not read."
  (let* ((type   (org--smartdiagram-type (plist-get kwdargs :smartdiagram)))
         (raw    (org--graph-read raw-contents))
         (tables (plist-get raw :name-tables))
         (_      (maphash (lambda (name rows) (unless (gethash name tables) (puthash name rows tables)))
                          (org--graph-buffer-templates)))
         (hub    (string-match-p "\\`\\(?:connected constellation\\|constellation\\|bubble\\) " type))
         (items  (plist-get raw :items))
         (members (if hub
                      (if (= 1 (length items)) (cons (car items) (plist-get (car items) :children))
                        (user-error "graph: a %s has one top item, its centre; this has %s" type (length items)))
                    items))
         (cells  (--map (org--smartdiagram-item it tables (string-prefix-p "descriptive" type)) members))
         (set    (--map (list (car it) (cadr it)) (org--graph-expand-inherit (plist-get raw :graph-rows) tables)))
         (colors (if hub (cdr cells) cells))
         (prose  (org--smartdiagram-prose raw-contents)))
    (when (or (plist-get raw :node-rows) (plist-get raw :edge-rows) (plist-get raw :ranks))
      (user-error "graph: #+node:, #+edge:, and #+rank: are Graphviz's; a smartdiagram's settings are its #+graph: rows"))
    (-each (if hub (cdr members) members)
      (lambda (it) (when (plist-get it :children)
                     (user-error "graph: a %s nests no deeper than %s; `%s' has children" type
                                 (if hub "its centre's children" "its items") (s-trim (plist-get it :text))))))
    (--each set (unless (org-graph--smartdiagram-key (car it))
                  (user-error "graph: `%s' is no smartdiagram key; C-M-i in the #+graph: table lists them" (car it))))
    ;; An item's | color | overrides its slot of the colour list; the centre's, the planet's or middle bubble's.
    (when (-some #'cdr colors)
      (let ((base (s-split "[ \t]*,[ \t]*"
                           (or (cadr (assoc "set color list" set))
                               (plist-get (org-graph--smartdiagram-key "set color list") :default))
                           t)))
        (setq set (append (--remove (equal (car it) "set color list") set)
                          `(("set color list" ,(s-join ", " (--map-indexed (or (cdr it) (nth it-index base) "gray")
                                                                            colors))))))))
    (-some--> (and hub (cdr (car cells)))
      (setq set (append set `((,(if (string-prefix-p "bubble" type) "bubble center node color" "planet color") ,it)))))
    (concat "{"
            (when set (format "\\smartdiagramset{%s}%%\n" (s-join ",\n  " (--map (format "%s={%s}" (car it) (cadr it)) set))))
            "\\ifdefined\\osbesmartdiagram\\else\\newsavebox\\osbesmartdiagram\\fi\n"
            (format "\\sbox\\osbesmartdiagram{\\smartdiagram[%s]{%%\n  %s}}%%\n" type (s-join ",\n  " (-map #'car cells)))
            "\\begin{minipage}{\\wd\\osbesmartdiagram}\\centering\n"
            (unless (s-blank-str-p main-arg) (format "{\\large %s}\\par\\medskip\n" (org--smartdiagram-label main-arg)))
            (unless (s-blank-str-p (car prose)) (format "%s\\par\\medskip\n" (org--smartdiagram-latex (car prose))))
            "\\usebox\\osbesmartdiagram\\par\n"
            (unless (s-blank-str-p (cdr prose)) (format "\\medskip\n%s\n" (org--smartdiagram-latex (cdr prose))))
            "\\end{minipage}}")))

(defun org-graph--smartdiagram-document (latex)
  "LATEX, as `org--smartdiagram' writes it, ↦ a standalone document typesetting just it, cropped to a small border."
  (concat "\\documentclass[border=12pt]{standalone}\n"
          (mapconcat (lambda (package) (format "\\usepackage{%s}\n" package)) org-graph-smartdiagram-latex-head "")
          "\\usepackage{hyperref}\n\\begin{document}\n" latex "\n\\end{document}\n"))

(defun org-graph--latex-errors (log)
  "A pdflatex LOG ↦ its errors: each `!' line, and the `l.N' line saying where."
  (s-join "\n" (--filter (string-match-p "\\`\\(?:! \\|l\\.[0-9]+ \\)" it) (s-lines log))))

(defun org-graph--run-latex (latex format outfile dpi)
  "(LATEX, FORMAT, OUTFILE, DPI) ↦ (EXIT-CODE . ERRORS) of typesetting LATEX with pdflatex, then writing OUTFILE as a
FORMAT ---png at DPI dots per inch, via pdftoppm; or svg, via pdftocairo.  Relative image paths resolve as Org's do."
  (let* ((dir (make-temp-file "org-graph-" t))
         (tex (expand-file-name "graph.tex" dir))
         (pdf (expand-file-name "graph.pdf" dir)))
    (unwind-protect
        (condition-case nil
            (progn
              (write-region (org-graph--smartdiagram-document latex) nil tex)
              (if (not (zerop (call-process "pdflatex" nil nil nil "-interaction=nonstopmode" "-halt-on-error"
                                            "-output-directory" dir tex)))
                  (cons 1 (org-graph--latex-errors (with-temp-buffer
                                                     (insert-file-contents (expand-file-name "graph.log" dir))
                                                     (buffer-string))))
                (cons (if (equal format "svg")
                          (call-process "pdftocairo" nil nil nil "-svg" pdf outfile)
                        (call-process "pdftoppm" nil nil nil "-png" "-r" (format "%s" dpi) "-singlefile" pdf
                                      (file-name-sans-extension outfile)))
                      "")))
          (file-missing (cons 1 "")))
      (delete-directory dir t))))

(provide 'org-graph-smartdiagram)
;;; org-graph-smartdiagram.el ends here
