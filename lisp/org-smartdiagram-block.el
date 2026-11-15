;;; org-smartdiagram-block.el --- #+begin_smartdiagram TITLE :type TYPE: an Org list ↦ LaTeX's smartdiagram  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, latex, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A #+begin_smartdiagram block is typeset by LaTeX's smartdiagram package, from the Org a #+begin_graph reads:
;;
;;   #+begin_smartdiagram One syntax :type constellation  ; the title sits above the diagram
;;   #+graph:                                              ; \smartdiagramset{…}, in smartdiagram's own words
;;   | planet color | black!75 |
;;   + =easy-access.el=                                    ; constellation & bubble: one item, the planet …
;;     | fontcolor | white |                               ; … an item's color, fontcolor, and fontsize rows
;;     + =(:k plist)= \\ replaces =(plist-get plist :k)=   ; … its children, the satellites
;;
;;   -----                                                 ; prose after the list sits beneath the diagram
;;   #+end_smartdiagram
;;
;; It is a thin facade over #+begin_image latex: the list becomes LaTeX, which the image block typesets in
;; place along LaTeX, and renders as an image along any other backend, and upon C-c C-c.  The keys and diagram
;; types, with their descriptions, are here for completion and eldoc; see org-graph-edit.el.

;;; Code:

(require 'org-graph-parse)
(require 'org-image-block)
(require 'ox-latex)

(defconst org-smartdiagram-types
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
  "The `:type' values of a smartdiagram ↦ what each draws.
A type of two words takes a dash: `connected-constellation' is smartdiagram's “connected constellation diagram”.")

(defconst org-smartdiagram-keys
  '((:name "font" :default "\\small" :description "The font of every item's text.")
    (:name "text color" :default "black" :description "The color of every item's text.")
    (:name "text width" :default "1.5cm" :description "How wide a flow or circular item's text fills before it breaks.")
    (:name "border color" :default "gray" :description "The color of each item's border.")
    (:name "set color list" :default "red!40, cyan!40, blue!40, green!40, orange!40, yellow!40, magenta!40, brown!40, violet!40, teal!40"
     :description "The items' colors, in order; an item's | color | row overrides its own.")
    (:name "uniform color list" :description "`C for N items': the first N items all in color C.")
    (:name "module shape" :default "rnd rectangle" :description "The shape of a flow or circular item."
     :values ("rnd rectangle" "rectangle" "circle" "ellipse" "diamond"))
    (:name "module minimum width" :default "2cm" :description "The least width of a flow or circular item.")
    (:name "module minimum height" :default "1cm" :description "The least height of a flow or circular item.")
    (:name "module x sep" :default "2.75" :description "The distance between items of a horizontal flow, in cm.")
    (:name "module y sep" :default "1.65" :description "The distance between items of a vertical flow, in cm.")
    (:name "circular distance" :default "2.75cm" :description "The radius of a circular diagram.")
    (:name "arrow color" :default "gray" :description "The color of every arrow, under uniform arrow color.")
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
    (:name "bubble center node color" :default "lightgray!60" :description "The color of a bubble diagram's middle bubble.")
    (:name "bubble center node font" :default "\\large" :description "The font of a bubble diagram's middle bubble.")
    (:name "bubble center node size" :default "4cm" :description "The size of a bubble diagram's middle bubble.")
    (:name "bubble node font" :default "\\normalfont" :description "The font of the outer bubbles.")
    (:name "bubble node size" :default "2.5cm" :description "The size of the outer bubbles.")
    (:name "bubble fill opacity" :default "0.5" :description "How opaque the outer bubbles are.")
    (:name "bubble text opacity" :default "0.8" :description "How opaque the outer bubbles' text is.")
    (:name "distance center/other bubbles" :default "0.8cm" :description "How far the outer bubbles sit from the middle one.")
    (:name "distance text center bubble" :default "0.5cm" :description "The inner margin of the outer bubbles.")
    (:name "planet color" :default "lightgray!60" :description "The color of a constellation's planet.")
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
    (:name "connection color" :default "gray" :description "The color of the planet's arrows, under uniform connection color.")
    (:name "connection line width" :default "0.1cm" :description "The thickness of the planet's arrows.")
    (:name "uniform connection color" :default "false" :description "With true, every arrow of the planet in connection color." :values ("true" "false"))
    (:name "sequence item height" :default "1cm" :description "The height of a sequence's boxes.")
    (:name "sequence item width" :default "2cm" :description "The width of a sequence's boxes.")
    (:name "sequence item text width" :default "1.9cm" :description "How wide a sequence box's text fills before it breaks.")
    (:name "sequence item font size" :default "\\normalfont" :description "The font of a sequence's boxes.")
    (:name "sequence item text color" :default "black" :description "The color of a sequence's text.")
    (:name "sequence item border color" :default "gray" :description "The color of a sequence's borders.")
    (:name "sequence item border size" :default "1.75\\pgflinewidth" :description "The thickness of a sequence's borders.")
    (:name "sequence item fill opacity" :default "1" :description "How opaque a sequence's boxes are.")
    (:name "sequence item text opacity" :default "1" :description "How opaque a sequence's text is.")
    (:name "sequence item uniform color" :default "gray!60!black" :description "The one color of a sequence's boxes, under uniform sequence color.")
    (:name "uniform sequence color" :default "false" :description "With true, every box of a sequence in sequence item uniform color." :values ("true" "false")))
  "The keys of smartdiagram's \\smartdiagramset, as a smartdiagram's #+graph: rows take them; from smartdiagram's
own sources.  Each is a plist: its :name, :description, :default when it has one, and :values when they are few.")

(defconst org-smartdiagram-item-keys
  '(("color"     . "This item's color; for the planet or middle bubble, that of the planet or bubble.")
    ("fontcolor" . "The color of this item's text; e.g., white, on a dark planet.")
    ("fontsize"  . "The size of this item's text, in points; e.g., 17."))
  "The keys of a smartdiagram item's table ↦ what they do; Graphviz's names for the same things.")

(defconst org-smartdiagram-latex-head "\\usepackage{smartdiagram}\n\\usepackage{varwidth}"
  "What LaTeX needs, to typeset a smartdiagram: smartdiagram, and varwidth, which centres src blocks; the preamble of
the #+begin_image latex block a smartdiagram is.")

(defun org-smartdiagram--key (name)
  "A #+graph: row's key NAME ↦ its `org-smartdiagram-keys' entry; else nil."
  (--find (equal (plist-get it :name) name) org-smartdiagram-keys))

(defun org--smartdiagram-type (type)
  "A `:type' TYPE ↦ smartdiagram's name for it, else a user error.
  flow:horizontal  ↦  \"flow diagram:horizontal\"      connected-constellation  ↦  \"connected constellation diagram\""
  (let ((type (format "%s" type)))
    (unless (assoc type org-smartdiagram-types)
      (user-error "smartdiagram: `:type %s' should be one of %s" type (s-join ", " (mapcar #'car org-smartdiagram-types))))
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
item's CELL is {{TERM},{DESCRIPTION}}.  Graph-only syntax, such as a record table or an :edge row, is an error."
  (-let* (((text . tags) (org--graph-split-tags (plist-get item :text)))
          (pre   (plist-get item :pre))
          (rows  (org--graph-expand-inherit (append (plist-get pre :attr-rows)
                                                    (when tags `((":inherit" ,(s-join " " tags)))))
                                            tables))
          (row   (lambda (key) (cadr (assoc key rows))))
          ((term . body) (org--smartdiagram-term text)))
    (-each rows (-lambda ((key)) (unless (assoc key org-smartdiagram-item-keys)
                                   (user-error "smartdiagram: `%s' means nothing to a smartdiagram item; it takes %s" key
                                               (s-join ", " (butlast (mapcar #'car org-smartdiagram-item-keys)))))))
    (when (plist-get pre :table)
      (user-error "smartdiagram: a smartdiagram item has no record table; its table is | key | value | rows"))
    (when (--any (plist-get it :edge) (plist-get pre :notes))
      (user-error "smartdiagram: a smartdiagram draws no labelled arcs, so a `#+begin_note :edge' has nothing to describe"))
    (cond ((and descriptive (not term))
           (user-error "smartdiagram: each item of a descriptive diagram is `+ TERM :: DESCRIPTION'; not `%s'" (s-trim text)))
          ((and term (not descriptive))
           (user-error "smartdiagram: `%s ::' labels an arc, yet only a descriptive diagram has terms" term)))
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
  "A smartdiagram's RAW body ↦ (ABOVE . BELOW), the Org prose before and after its list; tables, directives,
and the list itself aside.  \"Intro\\n+ a\\n-----\"  ↦  (\"Intro\\n\" . \"-----\")"
  (with-temp-buffer
    (insert raw)
    (org-mode)
    (let (above below seen-list)
      (dolist (el (org--graph-elements (org-element-contents (org-element-parse-buffer))))
        (pcase (org-element-type el)
          ('plain-list (setq seen-list t))
          ((or 'table 'keyword) nil)
          ((and 'special-block (guard (equal (downcase (org-element-property :type el)) "note"))) nil)
          (_ (let ((text (buffer-substring-no-properties (org-element-property :begin el) (org-element-property :end el))))
               (if seen-list (push text below) (push text above))))))
      (cons (s-join "" (nreverse above)) (s-join "" (nreverse below))))))

(defun org--smartdiagram (raw-contents main-arg type)
  "(RAW-CONTENTS, MAIN-ARG, TYPE) ↦ the LaTeX typesetting a `#+begin_smartdiagram MAIN-ARG :type TYPE' block.

MAIN-ARG, the title, sits above the diagram, as does the body's prose before its list; its prose after the list sits
beneath.  The #+graph: rows are \\smartdiagramset's; the items are the diagram's, as TYPE reads them:
  constellation, connected-constellation, bubble  ↦  one item, the planet; its children, the satellites
  descriptive                                     ↦  `+ TERM :: DESCRIPTION' items
  otherwise                                       ↦  the items, in order
#+node: and #+edge: are Graphviz's, so they are errors here; as is any nesting TYPE does not read."
  (let* ((type   (org--smartdiagram-type (or type (user-error "smartdiagram: which diagram? `:type' is one of %s"
                                                                  (s-join ", " (mapcar #'car org-smartdiagram-types))))))
         (raw    (org--graph-read raw-contents))
         (tables (plist-get raw :name-tables))
         (_      (maphash (lambda (name rows) (unless (gethash name tables) (puthash name rows tables)))
                          (org--graph-buffer-templates)))
         (hub    (string-match-p "\\`\\(?:connected constellation\\|constellation\\|bubble\\) " type))
         (items  (plist-get raw :items))
         (members (if hub
                      (if (= 1 (length items)) (cons (car items) (plist-get (car items) :children))
                        (user-error "smartdiagram: a %s has one top item, its centre; this has %s" type (length items)))
                    items))
         (cells  (--map (org--smartdiagram-item it tables (string-prefix-p "descriptive" type)) members))
         (set    (--map (list (car it) (cadr it)) (org--graph-expand-inherit (plist-get raw :graph-rows) tables)))
         (colors (if hub (cdr cells) cells))
         (prose  (org--smartdiagram-prose raw-contents)))
    (when (or (plist-get raw :node-rows) (plist-get raw :edge-rows))
      (user-error "smartdiagram: #+node: and #+edge: are Graphviz's; a smartdiagram's settings are its #+graph: rows"))
    (-each (if hub (cdr members) members)
      (lambda (it) (when (plist-get it :children)
                     (user-error "smartdiagram: a %s nests no deeper than %s; `%s' has children" type
                                 (if hub "its centre's children" "its items") (s-trim (plist-get it :text))))))
    (--each set (unless (org-smartdiagram--key (car it))
                  (user-error "smartdiagram: `%s' is no smartdiagram key; C-M-i in the #+graph: table lists them" (car it))))
    ;; An item's | color | overrides its slot of the color list; the centre's, the planet's or middle bubble's.
    (when (-some #'cdr colors)
      (let ((base (s-split "[ \t]*,[ \t]*"
                           (or (cadr (assoc "set color list" set))
                               (plist-get (org-smartdiagram--key "set color list") :default))
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

;;;; The block: a thin facade over #+begin_image latex

(defun org--smartdiagram-image-source (latex)
  "Returns the #+begin_image latex source typesetting LATEX, as `org--smartdiagram' writes it: its packages, a line
of dashes, then LATEX."
  (format "%s\n-----\n%s" org-smartdiagram-latex-head latex))

(org-defblock smartdiagram (&optional title &key type file resolution as-image
                            :edit-indirect-mode org-graph-edit-mode :manual "#Graphs"
                            :hover "TYPE is circular, flow, descriptive, priority-descriptive, bubble, constellation,
connected-constellation, or sequence; a :option follows, as in flow:horizontal.  The #+graph: rows are
\\smartdiagramset's; C-M-i in its table lists them.  For a picture whose arcs we choose, see #+begin_graph."
                            &context backend raw-contents)
  "Typeset an Org list as LaTeX's smartdiagram of TYPE, such as constellation; TITLE above it.
The list reads as a #+begin_graph's does, see `org--smartdiagram'; the result is a #+begin_image latex block, see
`org--image-block': LaTeX typesets it in place, its packages joining the preamble; any other backend shows its
image.  FILE, RESOLUTION, and AS-IMAGE are the image block's."
  (let ((latex (org--smartdiagram (org--graph-expand-macros raw-contents) title type)))
    (if (and (org-export-derived-backend-p backend 'latex) (not as-image))
        (progn (cl-pushnew org-smartdiagram-latex-head org--latex-head-used :test #'equal)
               (format "\\begin{center}\n%s\n\\end{center}" latex))
      (org-parse (org--image-block backend (org--smartdiagram-image-source latex) "latex"
                                   :file file :resolution resolution :as-image as-image
                                   :alt (if (s-blank-str-p title) "A smartdiagram" title))))))

(org-defblock-attribute smartdiagram :example "
#+begin_smartdiagram From text to value :type flow:horizontal
+ Read
+ Expand
+ Evaluate
#+end_smartdiagram")

(org-def-action smartdiagram (&optional title &key type file resolution)
  "Render the smartdiagram as #+begin_image latex does, and show it atop the block; again, to remove it."
  (org--image-preview 'latex (org--smartdiagram-image-source
                              (org--smartdiagram (org--graph-expand-macros raw-contents) title type))
                      (list :file file :resolution resolution)
                      (org-element-property :name (org-special-block-at-point))))

(provide 'org-smartdiagram-block)
;;; org-smartdiagram-block.el ends here
