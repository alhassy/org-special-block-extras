;;; org-graph-parse.el --- #+begin_graph: read data ⟶ graph statements  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Musa Al-hassy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Part of org-graph-block.el: items, their text, and their record tables ⟶ the statements that
;; org-graph-dot.el prints.

;;; Code:

(require 'org-graph-read)
(require 'org-markup-links)
(require 'org-math-markup)

(defconst org-graph-reserved-keys '(":id" ":inherit" ":children-rank")
  "Table keys meaningful to the `graph' block itself, rather than to Graphviz; alongside the prefixes `:edge.' and `:edge[TARGET].'.")

(defcustom org-graph-checkbox-templates
  '(("checked" ("color" "forestgreen") ("fontcolor" "forestgreen"))
    ("partial" ("color" "darkorange") ("fontcolor" "darkorange")))
  "The default #+name: templates that an item's checkbox applies: [X] ↦ checked, [-] ↦ partial, [ ] ↦ unchecked.
A graph's own #+name: template of the same name wins; a checkbox whose template exists nowhere styles nothing."
  :type '(alist :key-type string :value-type (repeat (list string string)))
  :group 'org-special-block-extras)

(defvar org-graph-builtin-templates nil
  "The (NAME . ROWS) of each template every graph has, sans defining it; `org-defgraph-tag' defines them.
A graph's, or the buffer's, own #+name: template of the same name wins; see `org--graph-parse'.")

(defvar org-graph-builtin-tag-docs nil
  "The (NAME . DOCSTRING) of each tag `org-defgraph-tag' defines; clicking such a tag in a graph shows it.")

(defun org--graph-define-tag (name pairs docstring)
  "Returns NAME, having made it a template every graph has: PAIRS' rows, read as a #+name: line's, documented by
DOCSTRING; a former definition of NAME is replaced.  See `org-defgraph-tag'."
  (setf (alist-get name org-graph-builtin-templates nil nil #'equal) (org--graph-keyword-rows 'name pairs)
        (alist-get name org-graph-builtin-tag-docs nil nil #'equal) docstring)
  name)

(defmacro org-defgraph-tag (name pairs docstring)
  "Defines the tag :NAME: of every graph, as the KEY=VALUE PAIRS of a `#+name: NAME' line, documented by DOCSTRING.
  (org-defgraph-tag hot \"color=red penwidth=2\" \"Marks what needs attention now: red, and bold.\")
  + Deadline :hot:   ;; in any graph, now a red, bold node
As with `defvar', the value comes with its docstring; yet, as with `setq', evaluating it anew redefines the tag.
PAIRS may name other tags too, as in \":hot: style=dashed\".  A graph's, or the buffer's, own #+name: NAME wins."
  (declare (indent defun) (doc-string 3))
  `(org--graph-define-tag ,(format "%s" name) ,pairs ,docstring))

;; UML class diagrams: an arc's tag, as in “+ inherits :: [[Animal]] :inherits:”; the arrow points at what is inherited.
(org-defgraph-tag inherits "arrowhead=empty"
  "Draws an arc as UML inheritance: a hollow triangle at the class inherited from.")
(org-defgraph-tag implements "arrowhead=empty style=dashed"
  "Draws an arc as UML realisation: dashed, with a hollow triangle at the interface implemented.")
(org-defgraph-tag composes "dir=both arrowtail=diamond arrowhead=none"
  "Draws an arc as UML composition: a filled diamond at the owner, the parent; its parts live and die with it.")
(org-defgraph-tag aggregates "dir=both arrowtail=odiamond arrowhead=none"
  "Draws an arc as UML aggregation: a hollow diamond at the owner, the parent; its parts may outlive it.")
(org-defgraph-tag depends "arrowhead=vee style=dashed"
  "Draws an arc as UML dependency: dashed, with an open arrow at what is depended upon.")

;; State machines: a node's tag; a composite state is a container.
(org-defgraph-tag state "shape=box style=rounded"
  "Draws a node as a state of a state machine: a rounded box.")
(org-defgraph-tag start "shape=point width=0.2 label=\"\""
  "Draws a node as a state machine's initial pseudo-state: a small dot, sans label.")
(org-defgraph-tag final "shape=point width=0.2 peripheries=2 label=\"\""
  "Draws a node as a state machine's final state: a ringed dot, sans label.")
(org-defgraph-tag choice "shape=diamond width=0.3 height=0.3 fixedsize=true label=\"\""
  "Draws a node as a state machine's choice pseudo-state: a small diamond, sans label.")

;; Entity-relationship cardinalities, crow's-foot style: an arc's tag, read from the parent's end to the child's.
(org-defgraph-tag one-to-one "dir=both arrowtail=teetee arrowhead=teetee"
  "Draws an arc as an entity-relationship one-to-one: a double bar at either end.")
(org-defgraph-tag one-to-many "dir=both arrowtail=teetee arrowhead=crowtee"
  "Draws an arc as an entity-relationship one-to-many: a double bar at the parent, a crow's foot at the child.")
(org-defgraph-tag many-to-one "dir=both arrowtail=crowtee arrowhead=teetee"
  "Draws an arc as an entity-relationship many-to-one: a crow's foot at the parent, a double bar at the child.")
(org-defgraph-tag many-to-many "dir=both arrowtail=crowtee arrowhead=crowtee"
  "Draws an arc as an entity-relationship many-to-many: a crow's foot at either end.")

;; Lines: a node's outline, or an arc.  A style's flags join, so :dashed:bold: is both, and a theme's fill survives.
(org-defgraph-tag dashed "style=dashed"
  "Draws a node's outline, or an arc, dashed.")
(org-defgraph-tag dotted "style=dotted"
  "Draws a node's outline, or an arc, dotted.")
(org-defgraph-tag bold "style=bold"
  "Draws a node's outline, or an arc, thick.")
(org-defgraph-tag bidirectional "dir=both"
  "Draws an arc with an arrowhead at either end.")
(org-defgraph-tag undirected "dir=none"
  "Draws an arc with no arrowhead, in a directed graph.")

;; Colours: Catppuccin Latte's accents, legible on light backgrounds, a theme's included.
(org-defgraph-tag red "color=\"#d20f39\" fontcolor=\"#d20f39\""
  "Colours a node's outline and text, or an arc and its label, red: #d20f39.")
(org-defgraph-tag orange "color=\"#fe640b\" fontcolor=\"#fe640b\""
  "Colours a node's outline and text, or an arc and its label, orange: #fe640b.")
(org-defgraph-tag yellow "color=\"#df8e1d\" fontcolor=\"#df8e1d\""
  "Colours a node's outline and text, or an arc and its label, yellow: #df8e1d.")
(org-defgraph-tag green "color=\"#40a02b\" fontcolor=\"#40a02b\""
  "Colours a node's outline and text, or an arc and its label, green: #40a02b.")
(org-defgraph-tag teal "color=\"#179299\" fontcolor=\"#179299\""
  "Colours a node's outline and text, or an arc and its label, teal: #179299.")
(org-defgraph-tag blue "color=\"#1e66f5\" fontcolor=\"#1e66f5\""
  "Colours a node's outline and text, or an arc and its label, blue: #1e66f5.")
(org-defgraph-tag purple "color=\"#8839ef\" fontcolor=\"#8839ef\""
  "Colours a node's outline and text, or an arc and its label, purple: #8839ef.")
(org-defgraph-tag magenta "color=\"#c2148e\" fontcolor=\"#c2148e\""
  "Colours a node's outline and text, or an arc and its label, magenta: #c2148e.")
(org-defgraph-tag pink "color=\"#ea76cb\" fontcolor=\"#ea76cb\""
  "Colours a node's outline and text, or an arc and its label, pink: #ea76cb.")
(org-defgraph-tag brown "color=\"#8b4513\" fontcolor=\"#8b4513\""
  "Colours a node's outline and text, or an arc and its label, brown: #8b4513.")
(org-defgraph-tag gray "color=\"#7c7f93\" fontcolor=\"#7c7f93\""
  "Colours a node's outline and text, or an arc and its label, gray: #7c7f93.")
(org-defgraph-tag black "color=\"#000000\" fontcolor=\"#000000\""
  "Colours a node's outline and text, or an arc and its label, black: #000000.")

;; Shapes: Graphviz's, named by what they draw; D2's cylinder is :database:.
(org-defgraph-tag database "shape=cylinder"
  "Draws a node as a database: a cylinder.")
(org-defgraph-tag note "shape=note"
  "Draws a node as a note, or document: a page with a folded corner.")
(org-defgraph-tag folder "shape=folder"
  "Draws a node as a folder: a box with a tab atop.")
(org-defgraph-tag component "shape=component"
  "Draws a node as a UML component: a box with two prongs.")
(org-defgraph-tag tab "shape=tab"
  "Draws a node as a tab: a box with a small tab atop.")
(org-defgraph-tag circle "shape=circle"
  "Draws a node as a circle.")
(org-defgraph-tag diamond "shape=diamond"
  "Draws a node as a diamond, say a decision.")
(org-defgraph-tag hexagon "shape=hexagon"
  "Draws a node as a hexagon.")
(org-defgraph-tag star "shape=star"
  "Draws a node as a star.")
(org-defgraph-tag oval "shape=ellipse"
  "Draws a node as an oval, Graphviz's own default.")
(org-defgraph-tag rectangle "shape=box"
  "Draws a node as a rectangle.")
(org-defgraph-tag text "shape=plaintext"
  "Draws a node as its label alone, sans outline.")

;; People: Graphviz has no such shape, so `org--graph-person' draws one, from a stick figure of its own.
(org-defgraph-tag person "shape=person"
  "Draws a node as a person: a stick figure, in the node's colour, above its name.")
(org-defgraph-tag actor "shape=person"
  "Draws a node as a UML actor: a stick figure, in the node's colour, above its name; as :person: does.")

;; Sizes; :size_N: is any size, by `org--graph-pattern-template'.
(org-defgraph-tag small "fontsize=10"
  "Writes a node's, or an arc's, label small: 10pt.  :size_N: writes it at N points.")
(org-defgraph-tag large "fontsize=20"
  "Writes a node's, or an arc's, label large: 20pt.  :size_N: writes it at N points.")

;; Layout.
(org-defgraph-tag container "shape=cluster"
  "Draws a node as a box around its children, which are its members rather than its arcs' targets; an arc to, or
from, it is clipped at its border.  Its own table rows, such as | bgcolor | grey |, style the box.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing: Org list ⟶ graph statements
;;
;; A parse yields plain data, which `org--graph-stmt->dot' then prints:
;;   (:node ID)                    ---ID's name and attributes live in the context
;;   (:edge SRC DST ATTRS)         ---SRC and DST are raw ids, or "record::port"
;;   (:rank KIND IDS)
;;   (:cluster ID :label L :attrs ROWS :scope PREAMBLE :stmts STATEMENTS)
;;   (:scope PREAMBLE STATEMENTS)  ---an anonymous subgraph: an item's defaults, reaching its descendants
;;   (:legend ID ROWS LABEL TEMPLATES)  ---a sample of TEMPLATES' ROWS, captioned LABEL

(cl-defstruct (org--graph-ctx (:constructor org--graph-ctx-create))
  "What a parse accumulates, and what printing its statements consults."
  (nodes (make-hash-table :test #'equal))    ; id ↦ (NAME . ATTRS), merged across repeat occurrences
  (records (make-hash-table :test #'equal))  ; record id ↦ t
  (clusters (make-hash-table :test #'equal)) ; cluster id ↦ the first node inside it
  name-tables                                ; template name ↦ rows, from #+name:
  (arc-templates (make-hash-table :test #'equal)) ; template name ↦ t, when it styles some arc
  (legends 0)                                ; how many legend entries so far
  (ranks (make-hash-table :test #'equal))    ; node id ↦ its rank group, under splines=curved; see `org--graph-raw->dot'
  (lines (make-hash-table :test #'equal))    ; node or cluster id ↦ the body line of the item declaring it
  wrap                                       ; the width labels fill to, from `#+begin_graph :wrap N'; else nil
  inline                                     ; non-nil under `:notes inline': notes show in labels, not tooltips
  compound                                   ; non-nil once an arc is clipped at a cluster
  (tag-clusters (make-hash-table :test #'equal))) ; container title ↦ the ids its :container_TITLE: tag gathers, last first

(defvar org--graph-legend nil
  "Non-nil while reading the items of a container titled Legend, wherein `- MEANING :T:' is a legend entry, not a node.")

(defvar org--graph-random-templates nil
  "While a graph reads `:unknown-tags random-templates', how many templates it has made up so far; else nil.")

(defconst org-graph-random-template-colours
  '("#d20f39" "#1e66f5" "#40a02b" "#df8e1d" "#8839ef" "#179299" "#fe640b" "#ea76cb")
  "The colours `:unknown-tags random-templates' gives made-up templates, in turn: Catppuccin Latte's accents.")

(defconst org-graph-random-template-shapes
  '("box" "ellipse" "hexagon" "diamond" "octagon" "parallelogram" "house" "cylinder")
  "The shapes `:unknown-tags random-templates' gives made-up templates, in turn; an arc ignores its template's shape.")

(defconst org-graph-random-template-fonts '("Helvetica" "Times" "Courier")
  "The fonts `:unknown-tags random-templates' gives made-up templates, in turn.")

(defun org--graph-random-template (name name-tables)
  "Under `:unknown-tags random-templates', the default, NAME ↦ the rows of a made-up template, kept in NAME-TABLES so that every use
of NAME ---a legend entry's too--- looks alike; else nil.  The Nth unknown name takes the Nth colour, shape, and font.
  \"lazy\", the first  ↦  ((\"color\" \"#d20f39\") (\"fontcolor\" \"#d20f39\") (\"shape\" \"box\") (\"fontname\" \"Helvetica\"))"
  (when org--graph-random-templates
    (let* ((n (1- (cl-incf org--graph-random-templates)))
           (pick (lambda (xs) (nth (mod n (length xs)) xs)))
           (colour (funcall pick org-graph-random-template-colours)))
      (puthash name `(("color" ,colour) ("fontcolor" ,colour)
                      ("shape" ,(funcall pick org-graph-random-template-shapes))
                      ("fontname" ,(funcall pick org-graph-random-template-fonts)))
               name-tables))))

(defun org--graph-pattern-template (name)
  "Returns the rows of a template that NAME's very spelling defines, else nil: size_N writes labels at N points.
  \"size_24\"  ⇒  ((\"fontsize\" \"24\"))"
  (when (string-match "\\`size_\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\'" name)
    `(("fontsize" ,(match-string 1 name)))))

(defun org--graph-expand-inherit (rows name-tables &optional seen)
  "ROWS with each | :inherit N₁ … Nₖ | row replaced by templates N₁ … Nₖ's rows, sans keys ROWS sets explicitly ---wherever
:inherit sits.  As with Emacs faces, an earlier template's key wins over a later one's; save a style, whose flags all
join, as `org--graph-style-union' says, so :dashed:bold: is both.  Templates may themselves inherit;
one already being expanded, as listed in SEEN, contributes nothing; one that no #+name: defines is an error, rather than
silently contributing nothing."
  (let ((explicit (--remove (equal (car it) ":inherit") rows)) inherited)
    (dolist (name (--mapcat (s-split "[ \t]+" (cadr it) t) (--filter (equal (car it) ":inherit") rows)))
      (unless (member name seen)
        (dolist (row (org--graph-expand-inherit
                      (or (gethash name name-tables)
                          (org--graph-pattern-template name)
                          (org--graph-random-template name name-tables)
                          (user-error "graph: `%s' names no #+name: template, as `:unknown-tags error' requires; for literal text \
ending in `:%s:', write =:%s:= or a | label | row" name name name))
                      name-tables (cons name seen)))
          (cond ((equal (car row) "style")
                 (setq inherited (cons (list "style" (org--graph-style-union (cadr (assoc "style" inherited)) (cadr row)))
                                       (--remove (equal (car it) "style") inherited))))
                ((not (or (assoc (car row) explicit) (assoc (car row) inherited)))
                 (push row inherited))))))
    (let ((style (and (assoc "style" inherited) (assoc "style" explicit)
                      (org--graph-style-union (cadr (assoc "style" inherited)) (cadr (assoc "style" explicit))))))
      (append (nreverse (if style (--remove (equal (car it) "style") inherited) inherited))
              (--map (if (and style (equal (car it) "style")) (list "style" style) it) explicit)))))

(defun org--graph-expand-scope (scope name-tables)
  "SCOPE, a preamble plist, with :inherit rows expanded in its #+graph:, #+node:, and #+edge: tables."
  (dolist (key '(:graph-rows :node-rows :edge-rows) scope)
    (setq scope (plist-put scope key (org--graph-expand-inherit (plist-get scope key) name-tables)))))

(defun org--graph-item-rows (rows name-tables)
  "An item's table ROWS ↦ (:id :attrs :edge :targeted :children-rank), routing reserved keys away from node attributes.

  ((\":edge[b].style\" \"dotted\") (\"shape\" \"box\"))  ↦  (:attrs ((\"shape\" \"box\")) :targeted ((\"b\" (\"style\" \"dotted\"))) …)

`:edge.ATTR' rows describe the arc from the parent; an ATTR given twice is an error, since a parallel arc is a second item.
`:edge[TARGET].ATTR' rows describe this item's arc to TARGET, from a [[TARGET]] link in its text."
  (let (id attrs edge targeted rank)
    (dolist (row (org--graph-expand-inherit (--filter (cadr it) rows) name-tables))
      (-let [(key val) row]
        (cond
         ((equal key ":id") (setq id (org--graph-unquote val)))
         ((equal key ":children-rank") (setq rank val))
         ((string-match "\\`:edge\\[\\(.+?\\)\\]\\.\\(.+\\)\\'" key)
          (let ((target (org--graph-unquote (match-string 1 key))) (attr (match-string 2 key)))
            (setf (alist-get target targeted nil nil #'equal)
                  (append (alist-get target targeted nil nil #'equal) (list (list attr val))))))
         ((s-starts-with-p ":edge." key)
          (let ((attr (substring key 6)))
            (when (and (assoc attr edge) (not (equal attr ":inherit")))
              (user-error "graph: `%s' is given twice; for a parallel arc, write its child item twice" key))
            (setq edge (append edge (list (list attr val))))))
         ((s-starts-with-p ":" key)
          (user-error "graph: unknown key `%s'; expected one of %s, or :edge.ATTR, :edge[TARGET].ATTR"
                      key org-graph-reserved-keys))
         (t (push row attrs)))))
    (list :id id :attrs (nreverse attrs) :children-rank rank
          :edge (org--graph-expand-inherit edge name-tables)
          :targeted (--map (cons (car it) (org--graph-expand-inherit (cdr it) name-tables)) (nreverse targeted)))))

(defun org--graph-style-union (&rest styles)
  "Returns the union of STYLES' flags, in order, as one Graphviz style; else nil.  Since a style is a set of flags,
\"filled\" and \"dashed,bold\" give \"filled,dashed,bold\": a theme's fill survives a :dashed: tag, and tags compose."
  (-some--> (-uniq (--mapcat (and it (split-string it "[ \t]*,[ \t]*" t)) styles)) (s-join "," it)))

(defvar org--graph-filled-by-default nil
  "While a graph prints, the kinds, `node' and `graph', whose graph-wide defaults fill already, as a theme's do.")

(defun org--graph-fills-p (rows)
  "Returns whether ROWS' style fills: filled, radial, striped, or wedged."
  (seq-intersection (split-string (or (cadr (assoc "style" rows)) "") "[ \t]*,[ \t]*" t) '("filled" "radial" "striped" "wedged")))

(defun org--graph-filling-kinds (scope)
  "Returns SCOPE's kinds, `node' and `graph', whose defaults fill: by a style that fills, or a fillcolor, which
`org--graph-filled' then fills; atop those that already fill, `org--graph-filled-by-default'."
  (-uniq (append (--keep (let ((rows (plist-get scope (cdr it))))
                           (and (or (org--graph-fills-p rows) (assoc "fillcolor" rows)) (car it)))
                         '((node . :node-rows) (graph . :graph-rows)))
                 org--graph-filled-by-default)))

(defun org--graph-filled (rows &optional kind)
  "Returns ROWS with the style flag `filled' when they set a fillcolor, which Graphviz silently ignores otherwise;
unless their style fills already, or ---for a node's or cluster's ROWS, by KIND `node' or `graph'--- the graph-wide
defaults do, since a style of its own would replace theirs, say a mindmap's rounded.  Never for an arc's rows."
  (if (and (assoc "fillcolor" rows) (not (org--graph-fills-p rows)) (not (memq kind org--graph-filled-by-default)))
      (org--graph-override rows '(("style" "filled")))
    rows))

(defun org--graph-override (rows over)
  "Returns ROWS with OVER's rows replacing those of the same key, e.g. ((a 1) (b 2)) and ((a 3)) give ((b 2) (a 3));
save a style, whose flags join those of ROWS', as `org--graph-style-union' says."
  (let ((style (and (assoc "style" rows) (assoc "style" over)
                    (org--graph-style-union (cadr (assoc "style" rows)) (cadr (assoc "style" over))))))
    (--map (if (and style (equal (car it) "style")) (list "style" style) it)
           (append (--remove (assoc (car it) over) rows) over))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Item text: <<targets>>, [[links]], and *markup*

(defconst org--graph-markup-tags
  '((bold "<B>" "</B>") (italic "<I>" "</I>") (underline "<U>" "</U>") (strike-through "<S>" "</S>")
    (code "<FONT FACE=\"monospace\">" "</FONT>") (verbatim "<FONT FACE=\"monospace\">" "</FONT>")
    (subscript "<SUB>" "</SUB>") (superscript "<SUP>" "</SUP>"))
  "Org markup ↦ the Graphviz HTML-label tags around it.")

(defconst org--graph-objects
  '(bold italic underline strike-through code verbatim subscript superscript entity export-snippet link target
         radio-target footnote-reference line-break latex-fragment)
  "The Org objects an item's text may hold; others, such as timestamps and statistics cookies, are plain text.")

(defvar org--graph-radio nil
  "While a graph is parsed, (REGEXP . TARGETS): its <<<radio targets>>>, and Org's regexp matching their mentions.")

(defun org--graph-parse-objects (text restriction)
  "TEXT ↦ its Org objects of the types RESTRICTION lists; a mention of one of this graph's radio targets is a link.
  \"Lisp, after Church\"  ↦  (\"Lisp, after \" (link (:type \"radio\" :path \"Church\" …)))  when <<<Church>>> is one"
  ;; The parse copies the current buffer's locals, and only those; so our binding must be a local one.
  (mapc #'make-local-variable '(org-target-link-regexp org-target-link-regexps))
  (let ((org-target-link-regexp (car org--graph-radio)) (org-target-link-regexps nil))
    (org--graph-splice-maths (org-element-parse-secondary-string text restriction) restriction)))

(defun org--graph-splice-maths (objs restriction)
  "Returns OBJS with each maths, such as $…$ or \\(…\\), spliced as the objects of RESTRICTION its inside parses to,
as a label holds no maths; so $\\alpha_{1}$ shows as \\alpha_{1} does, an α subscripted 1, and $red:x$ as red:x does."
  (--mapcat
   (let ((value (and (eq (org-element-type it) 'latex-fragment) (org-element-property :value it))))
     (cond ((and value (string-match org--math-delimited-re value))
            (append (org--graph-parse-objects (substring-no-properties (match-string 2 value)) restriction)
                    (-some--> (org-element-property :post-blank it) (and (< 0 it) (list (make-string it ?\s))))))
           (t (unless (or (stringp it) (null (org-element-contents it)))
                (apply #'org-element-set-contents it (org--graph-splice-maths (org-element-contents it) restriction)))
              (list it))))
   objs))

(defun org--graph-link-meaning (o)
  "Returns the meaning of O, a link of `org-markup-links', else nil; e.g., for red:x, (:color \"red\").
It is noted on O, as a label's every reader asks for it, and a doc: link's meaning costs a glossary lookup."
  (when (and (eq (org-element-type o) 'link) (assoc (org-element-property :type o) org-markup-links))
    (or (org-element-property :osbe-meaning o)
        (let ((meaning (org-markup-link-meaning (org-element-property :type o) (org-element-property :path o)
                                                (-some-> (org-element-contents o) org-element-interpret-data
                                                         substring-no-properties))))
          (org-element-put-property o :osbe-meaning meaning)
          meaning))))

(defun org--graph-meaning-tip (meaning)
  "Returns the tooltip MEANING hovers over a node, if any: Org while `org--graph-tips' collects tooltips, else plain."
  (-when-let (tip (plist-get meaning :tip))
    (cond ((and org--graph-tips (plist-get meaning :plain)) (format "\n#+begin_example\n%s\n#+end_example\n" tip))
          ((or org--graph-tips (plist-get meaning :plain)) tip)
          (t (org--graph-item-render nil (org--graph-parse-objects tip org--graph-objects))))))

(defun org--graph-radio-target (path)
  "A radio link's PATH ↦ the radio target it mentions, as declared; Org matches mentions sans case, e.g. \"church\" ↦ \"Church\"."
  (or (--first (equal (downcase (s-collapse-whitespace it)) (downcase (s-collapse-whitespace path))) (cdr org--graph-radio))
      path))

(defun org--graph-html-object-p (o)
  "Non-nil when object O needs an HTML-like label: markup, a line break, a braced a_{b} or a^{b}, or a @@dot:…@@ snippet."
  (pcase (org-element-type o)
    ((or 'subscript 'superscript) (org-element-property :use-brackets-p o))
    ;; A colour is a <FONT COLOR>; else a link needs one only for the description it shows, not, say, a tooltip's tip.
    ('link (let ((meaning (org--graph-link-meaning o)))
             (or (plist-get meaning :color)
                 (and (not (plist-get meaning :text)) (org--graph-html-label-p (org-element-contents o))))))
    ('export-snippet (equal "dot" (org-element-property :back-end o)))
    (_ t)))

(defun org--graph-html-label (objs)
  "Returns the HTML-like label of OBJS, a parsed item text, when they need one, as *bold* does; else nil."
  (when (org--graph-html-label-p objs)
    (format "<%s>" (s-trim (s-collapse-whitespace (org--graph-item-render t objs))))))

(defun org--graph-title-label (title)
  "Returns the label of a graph's TITLE, which reads as an item's text does: “*Origins* of red:Lisp” gives
<<B>Origins</B> of <FONT COLOR=\"red\">Lisp</FONT>>; plain words, themselves."
  (let ((objs (org--graph-parse-objects title org--graph-objects)))
    (or (org--graph-html-label objs) (s-trim (org--graph-item-render nil objs)))))

(defun org--graph-html-label-p (objs)
  "Returns whether OBJS, a parsed item text, need an HTML-like label; see `org--graph-html-object-p'."
  (org-element-map objs `(line-break export-snippet link ,@(mapcar #'car org--graph-markup-tags))
    #'org--graph-html-object-p nil t '(footnote-reference link)))

(defun org--graph-text (html? s)
  "Returns S as label text: escaped, by `org--graph-html-escape', when HTML?; else as is."
  (if html? (org--graph-html-escape s) s))

(defun org--graph-html-escape (s)
  "S with the characters that HTML-like labels reserve escaped, e.g. \"a<b\" ↦ \"a&lt;b\"."
  (s-replace-all '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;")) s))

(defun org--graph-item-text (text)
  "An item's TEXT ↦ (:name N :label L :target T :links ((PATH . DESCRIPTION) …) :bare-label D :arc-label A :arc-tags AS
:tags TS :tooltip F).

  \"calls :faint: :: <<lam>> *λ* [[church][encodes]] [fn::Church, 1936] :pure:\"
    ↦  (:name \"λ\" :label \"<<B>λ</B>>\" :target \"lam\" :links ((\"church\" . \"encodes\"))
        :arc-label \"calls\" :arc-tags (\"faint\") :tags (\"pure\") :tooltip \"Church, 1936\")

The name is TEXT sans tag, targets, links, footnotes, and markup; the label is an HTML-like one when TEXT has markup,
else the name.  A TEXT that is a lone [[PATH][D]] names the node PATH, with D labelling the arc from its parent; so does
a description-list term, `A ::', whose own tags name the arc's templates.  Trailing tags name templates, and inline
footnotes become the node's tooltip."
  (-let* ((checkbox (when (string-match "\\`\\[\\([ X-]\\)\\][ \t]+" text)
                      (prog1 (pcase (match-string 1 text) ("X" "checked") ("-" "partial") (_ "unchecked"))
                        (setq text (substring text (match-end 0))))))
          ;; As Org's, greedy; yet `[^\0]', since a `\\\\' may have broken the term's line.
          (term (when (string-match "\\`\\([^\0]*\\)[ \t]+::\\(?:[ \t]+\\|\\'\\)" text)
                  (prog1 (match-string 1 text) (setq text (substring text (match-end 0))))))
          ;; A term's own tags, `+ wed :married: :: Khadija', style the arc it labels.
          (arc-tags (when (and term (string-match (concat "\\(?:\\`\\|[ \t]+\\)" org--graph-tag-group-re "[ \t]*\\'") term))
                      (prog1 (s-split ":" (match-string 1 term) t) (setq term (substring term 0 (match-beginning 0))))))
          (arc-label (unless (s-blank-str-p term) (plist-get (org--graph-item-text-sans-tags term) :label)))
          ((text . tags) (org--graph-split-tags text))
          (_ (org--graph-check-stray-tags text))
          (parts (org--graph-item-text-sans-tags text)))
    (append parts (list :arc-label arc-label :arc-tags arc-tags :tags tags :checkbox checkbox))))

(defun org--graph-check-stray-tags (text)
  "Signals a user error when TEXT, an item's sans its trailing tags, still holds a tag, such as :x: in \"a :x: [[b]]\";
since only tags at the end of an item's text count, a stray one would silently become part of its name.  Verbatim
text, =:x:= or ~:x:~, and links are literal, so they are skipped."
  (let ((plain (replace-regexp-in-string (concat "=[^=\n]+=\\|~[^~\n]+~\\|" org-link-bracket-re) " " text)))
    (when (string-match (concat "\\(?:\\`\\|[ \t]\\)" org--graph-tag-group-re "\\(?:[ \t]\\|\\'\\)") plain)
      (user-error "graph: ‘%s’ in ‘%s’ is a tag, yet only tags at the end of an item's text count; move it there, or write =%s= for literal text"
                  (match-string 1 plain) (s-trim text) (match-string 1 plain)))))

(defun org--graph-split-tags (text)
  "TEXT ↦ (TEXT′ . TAGS), its trailing `:t₁:t₂:' tags split off: \"a :x:y:\" ↦ (\"a\" \"x\" \"y\"); TAGS is nil sans tags.
A TEXT of tags alone splits too: \":start:\" ↦ (\"\" \"start\")."
  (if (string-match (concat "\\(?:\\`\\|[ \t]+\\)" org--graph-tag-group-re "[ \t]*$") text)
      (cons (substring text 0 (match-beginning 0)) (s-split ":" (match-string 1 text) t))
    (list text)))

(defun org--graph-template-rows (tags name-tables)
  "TAGS ↦ the rows of their #+name: templates, from NAME-TABLES; as `org--graph-expand-inherit' reads | :inherit TAGS |."
  (org--graph-expand-inherit `((":inherit" ,(s-join " " tags))) name-tables))

(defun org--graph-arc-tags (text)
  "An item's parsed TEXT ↦ the tags styling its arc: its term's, and also its trailing ones when it is a reference."
  (append (plist-get text :arc-tags) (when (plist-get text :reference) (plist-get text :tags))))

(defun org--graph-node-link-p (o)
  "Returns whether O is a link object to a node: [[target]], or a radio target's mention; [[*Heading]] is a section's."
  (and (eq (org-element-type o) 'link)
       (member (org-element-property :type o) '("fuzzy" "radio"))
       (not (s-starts-with-p "*" (org-element-property :path o)))))

(defun org--graph-external-link-p (o)
  "Returns whether O is a link object to neither a node nor of `org-markup-links': a file, a section, such as
[[*Heading]], or a page of a scheme we know not, such as [[ftp:…]]."
  (and (eq (org-element-type o) 'link) (not (org--graph-node-link-p o)) (not (org--graph-link-meaning o))))

(defun org--graph-image-link-p (o)
  "Non-nil when O is a description-less [[file:PATH]] link to an image, such as [[file:emacs.png]]."
  (and (org--graph-external-link-p o) (equal (org-element-property :type o) "file") (not (org-element-contents o))
       (string-match-p (image-file-name-regexp) (org-element-property :path o))))

(defun org--graph-link-url (o)
  "A link object O ↦ where it points: [[file:a.org]] ↦ \"a.org\", [[ftp://o.org][O]] ↦ \"ftp://o.org\", and
[[*Org mode]] ↦ \"*Org mode\", which HTML export resolves to that section's anchor, by
`org--resolve-section-links'.  A web link's is its meaning's :url; see `org-markup-links'."
  (if (equal (org-element-property :type o) "file") (org-element-property :path o) (org-element-property :raw-link o)))

(defun org--graph-footnote-text (reference)
  "A footnote REFERENCE ↦ its definition's text, as a tooltip: [fn::NOTE] and [fn:NAME: NOTE] carry it inline, and a
bare [fn:NAME] finds it in the buffer, by `org-footnote-get-definition'; e.g., [fn::Born /1958/] ↦ \"Born 1958\",
or, while `org--graph-tips' collects tooltips, the Org \"Born /1958/\"."
  (-some--> (or (org-element-contents reference)
                (-some--> (org-element-property :label reference)
                  (nth 3 (org-footnote-get-definition it))
                  (org-element-parse-secondary-string it (org-element-restriction 'paragraph))))
    (s-trim (if org--graph-tips (org-element-interpret-data it) (org--graph-item-render nil it)))))

(defun org--graph-image-path (path)
  "Returns PATH, an image link's, as Graphviz can open it: ~ and $VARIABLES expanded, since Graphviz expands neither,
and would silently draw no image; a relative path stays relative, to the document's directory, as Org's links are.
An image that does not exist is a user error, rather than a node silently sans its image."
  (let ((path (substitute-in-file-name path)))
    (when (string-prefix-p "~" path) (setq path (expand-file-name path)))
    (unless (file-exists-p path)
      (user-error "graph: the image ‘%s’ does not exist" path))
    path))

(defun org--graph-item-text-sans-tags (text)
  "An item's TEXT, sans description tag and trailing tags, ↦ the plist of `org--graph-item-text', bar :arc-label and :tags.
A web or file link becomes the node's URL, its description staying in the label; an image link, its image."
  (let* ((objs (org--graph-parse-objects text org--graph-objects))
         (image  (-some--> (org-element-map objs 'link (lambda (o) (and (org--graph-image-link-p o) o)) nil t)
                   (org--graph-image-path (org-element-property :path it))))
         ;; A markup link may lead somewhere too, as [[https://o.org][Org]] does.
         (urls   (org-element-map objs 'link (lambda (o) (if (org--graph-external-link-p o)
                                                             (and (not (org--graph-image-link-p o)) (org--graph-link-url o))
                                                           (plist-get (org--graph-link-meaning o) :url)))))
         (_      (when (cdr urls) (user-error "graph: a node has one URL, but `%s' links to %s" text (s-join " and " urls))))
         ;; Its footnotes, and its markup links' tips, such as [[tooltip:x][a tip]]'s.
         (tooltip (-some--> (append (org-element-map objs 'footnote-reference #'org--graph-footnote-text)
                                    (org-element-map objs 'link (lambda (o) (org--graph-meaning-tip (org--graph-link-meaning o)))))
                    (s-join " " it)))
         (target (car (org-element-map objs '(target radio-target) (lambda (o) (org-element-property :value o)))))
         ;; A radio target's mention is an arc sans label; one in its own item, none.
         (links  (--remove (equal (car it) target)
                           (--map (if (equal "radio" (org-element-property :type it))
                                      (list (org--graph-radio-target (org-element-property :path it)))
                                    (cons (org-element-property :path it)
                                          (-some->> (org-element-contents it) (org--graph-item-render nil))))
                                  (org-element-map objs 'link (lambda (o) (and (org--graph-node-link-p o) o))))))
         (name   (s-trim (s-collapse-whitespace (org--graph-item-render nil objs)))))
    (append
     (cond
      ((-all? #'stringp objs) (list :name text :label text))
      ((and (s-blank-str-p name) (not target) (= 1 (length links)))
       (list :name (caar links) :label (caar links) :bare-label (cdar links) :reference t))
      ;; A lone image is its own name, drawn sans text.
      (t (list :name (if (s-blank-str-p name) (or target image) name)
               ;; Markup in a footnote is the tooltip's, not the label's.
               :label (cond ((org--graph-html-label objs))
                            ((not (s-blank-str-p name)) name)
                            (target)
                            (t "\"\""))
               :target target :links links)))
     (list :tooltip tooltip :url (car urls) :image image))))

(defun org--graph-item-render (html? objs)
  "OBJS, a parsed item text, ↦ its plain text; or, when HTML?, its HTML-like label body. Edge links and targets vanish,
while radio targets and their mentions stay; an entity is its character, \\alpha ↦ α; and a @@dot:…@@ snippet is raw
label HTML, any other snippet vanishing."
  ;; Sans the parse's text properties, which would otherwise ride along into the dot source.
  (substring-no-properties
   (mapconcat
   (lambda (o)
     (let ((type (org-element-type o)))
       ;; Org keeps an object's trailing spaces apart, as its :post-blank.
       (concat
        (cond
         ((stringp o) (org--graph-text html? o))
         ((memq type '(target footnote-reference)) "")
         ((eq type 'line-break) (if html? "<BR/>" " "))
         ((eq type 'entity) (org-element-property :utf-8 o))
         ((eq type 'export-snippet) (if (and html? (equal "dot" (org-element-property :back-end o)))
                                        (org-element-property :value o)
                                      ""))
         ;; As under Org's `^:{}', a_b is plain text; only a_{b} subscripts, so names such as snake_case stay whole.
         ((and (memq type '(subscript superscript)) (not (org-element-property :use-brackets-p o)))
          (concat (if (eq type 'subscript) "_" "^") (org--graph-item-render html? (org-element-contents o))))
         ((or (eq type 'radio-target) (equal "radio" (org-element-property :type o)))
          (org--graph-item-render html? (org-element-contents o)))
         ;; A markup link shows its text, a colour's as <FONT COLOR>; its tip and URL are the node's.
         ((-when-let (meaning (org--graph-link-meaning o))
            (let* ((text (plist-get meaning :text)) (colour (plist-get meaning :color))
                   (shown (cond (text (org--graph-text html? text))
                                ((org-element-contents o) (org--graph-item-render html? (org-element-contents o)))
                                ;; A path such as \\lambda, in $red:\\lambda$, is Org: here, an entity.
                                (t (org--graph-item-render html? (org--graph-parse-objects (org-element-property :path o)
                                                                                           org--graph-objects))))))
              (if (and html? colour) (format "<FONT COLOR=\"%s\">%s</FONT>" colour shown) shown))))
         ;; Maths not spliced, such as a bare \\frac{a}{b}, is its LaTeX.
         ((eq type 'latex-fragment) (org--graph-text html? (org-element-property :value o)))
         ((eq type 'link) (if (or (not (org--graph-external-link-p o)) (org--graph-image-link-p o)) ""
                            ;; A bare [[*Org mode]] shows as the section's title, sans star.
                            (org--graph-item-render html? (or (org-element-contents o)
                                                              (list (org--section-link-title (org--graph-link-url o)))))))
         (t (-let [(open close) (and html? (alist-get type org--graph-markup-tags))]
              (concat open
                      (if (memq type '(code verbatim))
                          (org--graph-text html? (org-element-property :value o))
                        (org--graph-item-render html? (org-element-contents o)))
                      close))))
        (unless (stringp o) (make-string (or (org-element-property :post-blank o) 0) ?\s)))))
   objs "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Records: an item's table, unless | key | value | rows ⟶ an HTML-like <TABLE> label

(defun org--graph-cell-html (cell)
  "A record CELL's text ↦ its HTML-like label body: markup as tags, a `\\\\' as <BR/>; targets and links vanish."
  (s-trim (org--graph-item-render t (org--graph-parse-objects (org--graph-break-lines cell)
                                                              (remq 'footnote-reference org--graph-objects)))))

(defun org--graph-break-lines (text)
  "TEXT with each `\\\\' ending a line, as Org's line breaks must; e.g. \"a \\\\ b\" ↦ \"a\\\\\\nb\"."
  (replace-regexp-in-string "[ \t]*\\\\\\\\[ \t]*" "\\\\\\\\\n" text))

(defun org--graph-table-label (lines)
  "A record table's LINES ↦ (LABEL . LINKS), its <TABLE> label and its cells' arcs, each (PORT TARGET DESCRIPTION TAGS);
nil sans LINES.

  | left | <<mid>> middle [[b]] :faint: |  ↦  (\"<<TABLE …><TR><TD>left</TD><TD PORT=\\\"mid\\\">middle</TD></TR></TABLE>>\"
                                                (\"mid\" \"b\" nil (\"faint\")))

A cell's <<target>> is its port; a cell with links but no target is ported by its text.  A cell's trailing `:t:' tags
style its arcs, as a reference's style its arc.  A `^' cell continues the
cell above; an empty one, the cell to its left ---or, in the first column, above.  A row of <l>, <c>, and <r> cookies
aligns its columns.  Hlines draw rules between rows; a table with them has one outer border rather than cell borders."
  (when lines
    (let* ((hline? (lambda (l) (s-matches-p "\\`|-" l)))
           (lines  (->> lines (-drop-while hline?) reverse (-drop-while hline?) reverse))
           (cookie? (lambda (row) (and (--all? (s-matches-p "\\`\\(?:<[lcr]?[0-9]*>\\)?\\'" it) row)
                                       (--some (not (s-blank-str-p it)) row))))
           (rows   (--map (unless (funcall hline? it) (-map #'s-trim (butlast (cdr (s-split "|" it))))) lines))
           (aligns (-some--> (-first (lambda (r) (and r (funcall cookie? r))) rows)
                     (--map (pcase (and (string-match "<\\([lcr]\\)" it) (match-string 1 it))
                              ("l" "LEFT") ("c" "CENTER") ("r" "RIGHT"))
                            it)))
           (rows   (--remove (and it (funcall cookie? it)) rows))
           ;; Each grid row, and whether an hline sits just above it.
           (grid   (let (acc rule) (dolist (r rows (nreverse acc))
                                     (if r (progn (push (cons rule r) acc) (setq rule nil)) (setq rule t)))))
           (width  (-max (--map (length (cdr it)) grid)))
           (owner  (make-hash-table :test #'equal))
           (cell   (lambda (r c) (or (nth c (cdr (nth r grid))) "")))
           links)
      ;; Every grid position ↦ the position of the cell it belongs to.
      (dotimes (r (length grid))
        (dotimes (c width)
          (puthash (cons r c)
                   (let ((text (funcall cell r c)))
                     (cond ((and (equal text "^") (> r 0)) (gethash (cons (1- r) c) owner))
                           ((and (s-blank-str-p text) (> c 0)) (gethash (cons r (1- c)) owner))
                           ((and (s-blank-str-p text) (> r 0)) (gethash (cons (1- r) c) owner))
                           ((member text '("" "^"))
                            (user-error "graph: a record table's `%s' cell, in row %d, column %d, continues nothing"
                                        text (1+ r) (1+ c)))
                           (t (cons r c))))
                   owner)))
      (let ((body
             (s-join "" (-map-indexed
                         (lambda (r row)
                           (concat
                            (when (car row) "<HR/>")
                            "<TR>"
                            (s-join "" (-keep (lambda (c)
                                                (when (equal (gethash (cons r c) owner) (cons r c))
                                                  (-let* (((text . tags) (org--graph-split-tags (funcall cell r c)))
                                                          (parts (org--graph-item-text-sans-tags text))
                                                          (port (-some-> (or (plist-get parts :target)
                                                                             (and (plist-get parts :links) (plist-get parts :name)))
                                                                  org--graph-dot-port))
                                                          (mine (--filter (equal (gethash it owner) (cons r c))
                                                                          (hash-table-keys owner)))
                                                          (rowspan (1+ (- (-max (-map #'car mine)) r)))
                                                          (colspan (1+ (- (-max (-map #'cdr mine)) c)))
                                                          (align (nth c aligns)))
                                                    (when (and tags (not (plist-get parts :links)))
                                                      (user-error "graph: a record cell's tags style its arcs, but `%s' \
has no [[link]]; for literal text ending in `:%s:', write =:%s:=" text (car tags) (car tags)))
                                                    (--each (plist-get parts :links)
                                                      (push (list port (car it) (cdr it) tags) links))
                                                    (concat "<TD"
                                                            (when port (format " PORT=\"%s\"" (org--graph-html-escape port)))
                                                            (when (> rowspan 1) (format " ROWSPAN=\"%d\"" rowspan))
                                                            (when (> colspan 1) (format " COLSPAN=\"%d\"" colspan))
                                                            (when align (format " ALIGN=\"%s\" BALIGN=\"%s\"" align align))
                                                            ">" (org--graph-cell-html text) "</TD>"))))
                                              (number-sequence 0 (1- width))))
                            "</TR>"))
                         grid))))
        (cons (format "<<TABLE %s CELLSPACING=\"0\">%s</TABLE>>"
                      (if (-some #'car grid) "BORDER=\"1\" CELLBORDER=\"0\"" "BORDER=\"0\" CELLBORDER=\"1\"")
                      body)
              (nreverse links))))))

(defun org--graph-item-parts (item)
  "A read ITEM ↦ (TEXT PREAMBLE CHILDREN KIND): its parsed text, its tables and directives, its sub-items, and their KIND.
As in Org, an item's text is its first paragraph, so a long line of [[links]] may wrap; its lines join, and only then
does a `\\\\' break the label's line, even mid-line.  An item of tags alone, such as `+ :start:', is anonymous: unlabelled,
its id made of its line, so `+ :container:' is an untitled box."
  (let ((text (->> (plist-get item :text) s-trim s-lines (-map #'s-trim) (s-join " ")
                   org--graph-break-lines org--graph-item-text)))
    (when (and (s-blank-str-p (plist-get text :name)) (plist-get text :tags) (not (plist-get text :target)))
      (setq text (plist-put (plist-put text :name (format "anonymous %d" (plist-get item :line))) :label "")))
    (list text (plist-get item :pre) (plist-get item :children) (plist-get item :kind))))

(defun org--graph-item-id (item)
  "A read ITEM ↦ its node id: its | :id | row, else its <<target>>, else its text."
  (-let [(text pre) (org--graph-item-parts item)]
    (or (-some-> (assoc ":id" (plist-get pre :attr-rows)) cadr org--graph-unquote)
        (plist-get text :target)
        (org--graph-unquote (plist-get text :name)))))

(defun org--graph-container-tag (tag)
  "Returns the container title TAG names, when it is :container_TITLE:, its underscores spaces; else nil.
  \"container_Holy_Land\"  ⇒  \"Holy Land\";  \"Container_Islam\"  ⇒  \"Islam\";  \"prophet\"  ⇒  nil"
  (let ((case-fold-search t))
    (and (string-match "\\`container_\\(.+\\)\\'" tag) (s-replace "_" " " (match-string 1 tag)))))

(defun org--graph-tint (colour)
  "Returns COLOUR, a #RRGGBB, blended three parts in four with white, so text stays legible atop it; else COLOUR."
  (if (not (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" colour)) colour
    (apply #'format "#%02x%02x%02x"
           (--map (+ it (/ (* 3 (- 255 it)) 4))
                  (list (string-to-number (substring colour 1 3) 16) (string-to-number (substring colour 3 5) 16)
                        (string-to-number (substring colour 5 7) 16))))))

(defun org--graph-tag-colours (tags name-tables)
  "Returns the distinct colours TAGS' templates set, in order; e.g., one per made-up template."
  (-uniq (--keep (cadr (assoc "color" (org--graph-template-rows (list it) name-tables))) tags)))

(defun org--graph-several-colours (node-tags arc-tags explicit name-tables)
  "Returns the rows that show every colour of several tags, sans keys EXPLICIT sets: a node's as stripes ---wedges, if
round--- of its NODE-TAGS' colours, tinted; an arc's as parallel lines, one per colour of its ARC-TAGS.  A single tag,
several of one colour, or a node's own fillcolor or style, needs none; the earlier tag wins every other clash."
  (let ((node (org--graph-tag-colours node-tags name-tables))
        (arc (org--graph-tag-colours arc-tags name-tables))
        (shape (or (cadr (assoc "shape" explicit))
                   (cadr (assoc "shape" (org--graph-template-rows node-tags name-tables))))))
    (--remove (assoc (car it) explicit)
              (append (when (and (cdr node) (not (assoc "fillcolor" explicit)) (not (assoc "style" explicit)))
                        `(("style" ,(if (member shape '(nil "ellipse" "oval" "circle" "doublecircle")) "wedged" "striped"))
                          ("fillcolor" ,(s-join ":" (mapcar #'org--graph-tint node)))))
                      (when (cdr arc) `((":edge.color" ,(s-join ":" arc))))))))

(defun org--graph-item-table (text pre name-tables)
  "An item's parsed TEXT and PREAMBLE ↦ its `org--graph-item-rows', its tags inheriting their #+name: templates.
A tag applies a template to whatever its line describes: after an item, its node; after a reference, or in a
description term, its arc.  Explicit rows still win.  A :container_TITLE: tag is no template, but a box to join;
see `org--graph-container-tag'.  Several tags of several colours show them all; see `org--graph-several-colours'."
  (let* ((arc-tags (org--graph-arc-tags text))
         (node-tags (unless (plist-get text :reference)
                      (append (-remove #'org--graph-container-tag (plist-get text :tags))
                              (-some--> (plist-get text :checkbox) (and (gethash it name-tables) (list it))))))
         (explicit (plist-get pre :attr-rows)))
    (org--graph-item-rows (append (when node-tags (list (list ":inherit" (s-join " " node-tags))))
                                  (when arc-tags (list (list ":edge.:inherit" (s-join " " arc-tags))))
                                  (org--graph-several-colours node-tags arc-tags explicit name-tables)
                                  explicit)
                          name-tables)))

(defun org--graph-ordered-rank (kind children)
  "An ordered list's CHILDREN, two or more, ↦ the statement setting them side by side; else nil.
\"1. a 2. b\" reads as “first a, then b”, on one rank."
  (when (and (eq kind 'ordered) (cdr children))
    (list (list :rank "same" (-map #'org--graph-item-id children)))))

(defun org--graph-declare (ctx id name attrs)
  "The statements declaring node ID, shown as NAME with ATTRS: (:node ID) the first time, else none.
A repeat occurrence contributes the ATTRS keys not yet set, and warns about those it contradicts."
  (let ((seen (gethash id (org--graph-ctx-nodes ctx))))
    (if (not seen)
        (progn (puthash id (cons name attrs) (org--graph-ctx-nodes ctx))
               (list (list :node id)))
      (dolist (row attrs)
        (let ((old (assoc (car row) (cdr seen))))
          (cond ((null old) (setcdr seen (append (cdr seen) (list row))))
                ((not (equal old row))
                 (display-warning 'org-graph (format "Node `%s' is given %s=%s again, keeping its first %s=%s"
                                                     id (car row) (cadr row) (car old) (cadr old)))))))
      nil)))

(defun org--graph-first-node (stmts)
  "The id of the first node STMTS declare, looking inside clusters; nil when there is none."
  (-some (lambda (s) (pcase (car s)
                       (:node (cadr s))
                       (:cluster (org--graph-first-node (plist-get (cddr s) :stmts)))
                       (:scope (org--graph-first-node (caddr s)))))
         stmts))

(defun org--graph-note-lines (raw)
  "A note's RAW body ↦ its lines: each paragraph's lines joined, then split at its `\\\\' line breaks.
  \"Called *by*\\nname.\\n\\nNever \\\\\\\\ twice.\"  ↦  (\"Called *by* name.\" \"Never\" \"twice.\")"
  (--mapcat (s-split "[ \t]*\\\\\\\\[ \t]*" (s-join " " (-map #'s-trim (s-lines it))) t)
            (--remove (s-blank-str-p it) (s-split "\n[ \t]*\n" (or raw "")))))

(defun org--graph-note-render (html? line)
  "A note's LINE ↦ its text sans markup; or, when HTML?, its HTML-like label body, markup as tags."
  (s-trim (s-collapse-whitespace (org--graph-item-render html? (org-element-parse-secondary-string
                                                                line '(bold italic underline strike-through
                                                                       code verbatim link))))))

(defun org--graph-join-tips (tips)
  "TIPS, tooltip texts or nils, ↦ one tooltip, a line each, or a paragraph each while `org--graph-tips' collects Org
tooltips; nil sans any.  (\"a\" nil \"b\")  ↦  \"a\\\\nb\""
  (-some->> (-non-nil tips) (s-join (if org--graph-tips "\n\n" "\\n"))))

(defun org--graph-notes-tooltip (raws)
  "Notes' RAWS bodies ↦ one tooltip, sans markup, a line per paragraph or `\\\\' break; nil sans RAWS.
While `org--graph-tips' collects tooltips, they are Org, so RAWS stay as written."
  (org--graph-join-tips (if org--graph-tips (--remove (s-blank-str-p it) (--map (s-trim (or it "")) raws))
                          (--map (org--graph-note-render nil it) (-mapcat #'org--graph-note-lines raws)))))

(defun org--graph-notes-body (raws width)
  "Notes' RAWS bodies ↦ an HTML-like label body: their lines, markup kept, filled to WIDTH and left-aligned;
nil sans RAWS.
  (\"Code /is/ data.\"), 40  ↦  \"Code <I>is</I> data.<BR ALIGN=\\\"LEFT\\\"/>\""
  (-some->> (--mapcat (org--graph-fill (org--graph-note-render t it) width) (-mapcat #'org--graph-note-lines raws))
    (--map (concat it "<BR ALIGN=\"LEFT\"/>"))
    (s-join "")))

(defun org--graph-with-body (label body)
  "(LABEL, BODY) ↦ an HTML-like label: LABEL's lines, then BODY in a smaller font; LABEL itself sans BODY.
  (\"Lisp\", \"Code is data.<BR ALIGN=\\\"LEFT\\\"/>\")  ↦  \"<Lisp<BR/><FONT POINT-SIZE=\\\"10\\\">Code is data.<BR ALIGN=\\\"LEFT\\\"/></FONT>>\""
  (if (not body)
      label
    (format "<%s<FONT POINT-SIZE=\"10\">%s</FONT>>"
            (cond ((member label '(nil "" "\"\"")) "")
                  ((string-match "\\`<\\(.*\\)>\\'" label) (concat (match-string 1 label) "<BR/>"))
                  (t (concat (s-replace "\\n" "<BR/>" (org--graph-html-escape label)) "<BR/>")))
            body)))

(defun org--graph-item-notes (pre reference)
  "An item's PREAMBLE ↦ (NODE-NOTES . ARC-NOTES), its notes' raw bodies.  A note describes what its item's line does
---a node, else, for a REFERENCE, an arc---; a `#+begin_note :edge', the arc into the item."
  (-let [(edge plain) (-separate (lambda (n) (plist-get n :edge)) (plist-get pre :notes))]
    (cons (unless reference (--map (plist-get it :text) plain))
          (--map (plist-get it :text) (append (when reference plain) edge)))))

(defun org--graph-label-words (body)
  "An HTML-like or plain label BODY ↦ its words, split at the spaces outside <tags>; a tag's own spaces stay put.
  \"<FONT FACE=\\\"monospace\\\">a b</FONT> c\"  ↦  (\"<FONT FACE=\\\"monospace\\\">a\" \"b</FONT>\" \"c\")"
  (let ((depth 0) (start 0) words)
    (dotimes (i (length body))
      (pcase (aref body i)
        (?< (cl-incf depth))
        (?> (cl-decf depth))
        (?\s (when (zerop depth) (push (substring body start i) words) (setq start (1+ i))))))
    (-remove #'s-blank-str-p (nreverse (cons (substring body start) words)))))

(defun org--graph-fill (body width)
  "(BODY, WIDTH) ↦ BODY's words, HTML-like or plain, filled into lines of at most WIDTH visible characters; a tag shows
nothing, an entity one character, and a word longer than WIDTH gets a line of its own.
  (\"Structure and Interpretation\", 14)  ↦  (\"Structure and\" \"Interpretation\")"
  (let ((visible (lambda (s) (length (replace-regexp-in-string "&[#[:alnum:]]+;" "x"
                                                               (replace-regexp-in-string "<[^>]*>" "" s)))))
        lines)
    (dolist (word (org--graph-label-words body) (nreverse lines))
      (if (and lines (<= (+ (funcall visible (car lines)) 1 (funcall visible word)) width))
          (setcar lines (concat (car lines) " " word))
        (push word lines)))))

(defun org--graph-wrap (label width)
  "(LABEL, WIDTH) ↦ LABEL filled into lines of at most WIDTH visible characters; LABEL itself sans WIDTH, or when a
`\\\\' already breaks it.
  (\"Structure and Interpretation\", 14)  ↦  \"Structure and\\\\nInterpretation\"   ---<BR/> in an HTML-like <label>"
  (let* ((html (string-match "\\`<\\(.*\\)>\\'" label))
         (body (if html (match-string 1 label) label)))
    (if (or (not width) (s-contains-p "<BR/>" body) (s-contains-p "\\n" body))
        label
      (let ((filled (s-join (if html "<BR/>" "\\n") (org--graph-fill body width))))
        (if html (format "<%s>" filled) filled)))))

(defun org--graph-subtree (pre name-tables stmts)
  "An item's PREAMBLE and its children's STMTS ↦ STMTS, inside an anonymous subgraph holding the item's #+graph:,
#+node:, #+edge:, and #+rank: lines when it has any; so its defaults reach every descendant, as Org's inherited
properties do, yet no box is drawn.
  + Weather              subgraph {
    #+node:          ↦      node [ shape=box ]
    | shape | box |         Weather -> Rain; Rain [ label=Rain ]; …
    + Rain …              }"
  (let ((scope (org--graph-expand-scope pre name-tables)))
    (if (and stmts (--some (plist-get scope it) '(:graph-rows :node-rows :edge-rows :ranks)))
        (list (list :scope scope stmts))
      stmts)))

(defun org--graph-parse-item (item parent ctx)
  "A read ITEM under PARENT (an id, or nil at the top) ↦ its statements; its nodes accrue in CTX.

The order is: arcs PARENT → ITEM, ITEM's [[link]] arcs, ITEM's declaration, then its children's statements.
An item that is a lone [[link]] is a reference: it draws arcs to its target, but leaves declaring it to the target's own item."
  (-let* (((text pre children kind) (org--graph-item-parts item))
          (name     (org--graph-unquote (plist-get text :name)))
          (label    (org--graph-unquote (plist-get text :label)))
          (tables   (org--graph-ctx-name-tables ctx))
          (rows     (org--graph-item-table text pre tables))
          (id       (or (plist-get rows :id) (plist-get text :target) name))
          (reference (plist-get text :reference))
          (_        (unless reference
                      (dolist (title (-keep #'org--graph-container-tag (plist-get text :tags)))
                        (push id (gethash title (org--graph-ctx-tag-clusters ctx))))))
          ((node-notes . arc-notes) (org--graph-item-notes pre reference))
          (wrap     (org--graph-ctx-wrap ctx))
          ;; Under `:notes inline', notes are label bodies; else, tooltips.
          (inline   (org--graph-ctx-inline ctx))
          (node-body (and inline (org--graph-notes-body node-notes (or wrap 40))))
          (arc-body (and inline (org--graph-notes-body arc-notes (or wrap 40))))
          (attrs    (org--graph-override (append (-some--> (org--graph-join-tips
                                                            (list (plist-get text :tooltip)
                                                                  (unless inline (org--graph-notes-tooltip node-notes))))
                                                   `(("tooltip" ,it)))
                                                 (-some--> (plist-get text :url) `(("URL" ,it)))
                                                 (-some--> (plist-get text :image) `(("image" ,it))))
                                         (plist-get rows :attrs)))
          (arc-label (or (plist-get text :arc-label) (plist-get text :bare-label)))
          (shape    (cadr (assoc "shape" attrs)))
          (fresh    (not (gethash id (org--graph-ctx-nodes ctx))))
          (targeted (plist-get rows :targeted))
          (to-arc   (lambda (target attrs)
                      (list :edge id target (org--graph-override attrs (alist-get target targeted nil nil #'equal)))))
          ;; A legend entry: a top-level `- :T: :: MEANING', Org's glossary; or, in a container titled Legend, `- MEANING :T:'.
          (legend-tags (cond ((and (not parent) (plist-get text :arc-tags)))
                             ((and org--graph-legend (not parent) (not reference) (not (plist-get text :arc-label)))
                              (plist-get text :tags))))
          (legend?  (and legend-tags t))
          (ranked   (org--graph-ordered-rank kind children))
          (rank-stmts (-if-let (rank (plist-get rows :children-rank))
                          (list (list :rank rank (-map #'org--graph-item-id children)))
                        ranked))
          ;; A record: Graphviz's nest { } fields; ours is an HTML-like <TABLE>, which a plain node wears as its outline.
          (table    (org--graph-table-label (plist-get pre :table)))
          (attrs    (if table (org--graph-override `(("shape" "plain") ("label" ,(car table))) attrs) attrs))
          (arcs     (append (when parent
                              (list (list :edge parent id
                                          (org--graph-override
                                           (append (-some--> (org--graph-with-body
                                                              (-some--> arc-label (org--graph-wrap it wrap)) arc-body)
                                                     `(("label" ,it)))
                                                   (-some--> (unless inline (org--graph-notes-tooltip arc-notes))
                                                     `(("tooltip" ,it))))
                                           (plist-get rows :edge)))))
                            (--map (funcall to-arc (car it) (-some--> (cdr it) `(("label" ,it))))
                                   (plist-get text :links))
                            ;; A cell's [[link]] is an arc from its port.
                            (-map (-lambda ((port target desc tags))
                                    (list :edge (format "%s::%s" id port) target
                                          (org--graph-override
                                           (org--graph-override
                                            (-some--> desc `(("label" ,it)))
                                            (when tags (org--graph-template-rows tags tables)))
                                           (alist-get target targeted nil nil #'equal))))
                                  (cdr table)))))
    (when (and (not parent) arc-notes)
      (user-error "graph: a note on `%s' describes the arc from a parent, but there is none" id))
    (when (and (not parent) (plist-get text :arc-label))
      (user-error "graph: a `TERM ::' describes the arc from a parent, but `%s' has none; for a legend entry, the TERM is \
just tags: `- :T: :: %s', or, in a container titled Legend, `- %s :T:'" id id id))
    ;; A legend entry's tags style its sample, not the graph's arcs.
    (unless legend?
      (org--graph-note-arc-templates ctx (append (org--graph-arc-tags text) (-mapcat #'cadddr (cdr table)))
                                     (plist-get pre :attr-rows)))
    (-when-let (stray (--remove (or (assoc it (plist-get text :links)) (assoc it (mapcar #'cdr (cdr table))))
                                (-map #'car targeted)))
      (user-error "graph: `%s' has `:edge[%s].…' rows, but no [[link]] to %s" id (car stray) (car stray)))
    (when table (puthash id t (org--graph-ctx-records ctx)))
    (when (and fresh (not legend?) (not reference)) (puthash id (plist-get item :line) (org--graph-ctx-lines ctx)))
    (when (and (plist-get text :reference) (or attrs (plist-get rows :id)))
      (user-error "graph: `[[%s]]' refers to a node declared elsewhere; give it node attributes there, not here" id))
    (cond
     ;; `- :married: :: marriage', with no parent to arc from, is a legend entry; so is a Legend container's `- marriage :married:'.
     (legend?
      (list (list :legend (format "legend_%d" (cl-incf (org--graph-ctx-legends ctx)))
                  (org--graph-template-rows legend-tags tables)
                  label legend-tags)))
     (reference
      (append arcs (org--graph-subtree pre tables (append (--mapcat (org--graph-parse-item it id ctx) children)
                                                          rank-stmts))))
     ((and fresh (equal shape "cluster"))
      (when table
        (user-error "graph: container `%s' has a table that is no | key | value | rows; a box of nodes cannot also be a \
record; if each key names an attribute, check its spelling" id))
      (org--graph-declare ctx id name nil)
      ;; Its children are its members; but a reference, `+ [[T]]', is an arc from the box, drawn outside it, lest T,
      ;; first met there, be drawn inside it.
      (-let* (((refs members) (--separate (plist-get (car (org--graph-item-parts it)) :reference) children))
              (stmts (let ((org--graph-legend (equal (downcase (format "%s" name)) "legend")))
                       (append (--mapcat (org--graph-parse-item it nil ctx) members) ranked)))
              (arcs (append arcs (--mapcat (org--graph-parse-item it id ctx) refs))))
        (-some--> (org--graph-first-node stmts) (puthash id it (org--graph-ctx-clusters ctx)))
        ;; The cluster comes first, so that its first node is declared inside it before any arc mentions that node.
        (cons (list :cluster id
                    :label (or (cadr (assoc "label" attrs)) (org--graph-with-body label node-body))
                    :attrs (--remove (member (car it) '("shape" "label")) attrs)
                    :scope (org--graph-expand-scope pre tables)
                    :stmts stmts)
              arcs)))
     (t
      ;; An ordered list keeps its order: `ordering=out' stops Graphviz from reshuffling the arcs to it.
      (append arcs
              (org--graph-declare ctx id (org--graph-with-body (org--graph-wrap label wrap) node-body)
                                  (if (and ranked (not (assoc "ordering" attrs)))
                                      (append attrs '(("ordering" "out")))
                                    attrs))
              (org--graph-subtree pre tables (append (--mapcat (org--graph-parse-item it id ctx) children)
                                                     rank-stmts)))))))

(defun org--graph-note-arc-templates (ctx tags rows)
  "Record in CTX the templates that style arcs: the arc TAGS, and those of ROWS' :edge.:inherit and :edge[T].:inherit.
So a legend draws such a template as an arc, rather than as a node."
  (dolist (name (append tags (--mapcat (s-split "[ \t]+" (cadr it) t)
                                       (--filter (string-match-p "\\`:edge\\(?:\\[.+\\]\\)?\\.:inherit\\'" (car it)) rows))))
    (puthash name t (org--graph-ctx-arc-templates ctx))))

(defun org--graph-buffer-templates ()
  "The current buffer's #+name: tables, outside graph blocks, ↦ templates, name ↦ rows; so, as with Babel's
named tables, a house style is named once per file rather than once per graph.
`#+name: wife :base:' inherits, as in a graph; and, as #+name: is Org's affiliated keyword, a #+caption: may sit by it.
`#+name: red color=red' is a template on one line, as `org--graph-keyword-rows' reads its pairs."
  (let ((templates (make-hash-table :test #'equal)))
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (re-search-forward "^[ \t]*#\\+\\(?:\\(begin_graph\\_>\\)\\|name:\\)" nil t)
            (if (match-beginning 1)
                ;; A graph's own templates are its own.
                (re-search-forward "^[ \t]*#\\+end_graph" nil 'move)
              (let ((table (save-match-data (org-element-at-point))))
                (cond ((eq (org-element-type table) 'table)
                       (org--graph-add-template templates (org-element-property :name table)
                                                (org--graph-table-rows table)))
                      ;; A template on one line, `#+name: red color=red', above whatever it may be.
                      ((save-excursion (beginning-of-line) (looking-at org--graph-name-pairs-re))
                       (org--graph-add-template templates (string-trim (match-string 2)) nil)))))))))
    templates))

(defun org--graph-option (kwdargs key default choices)
  "KWDARGS' KEY, as a string, else DEFAULT; one of CHOICES, else a user error.
  ((:notes inline), :notes, \"tooltip\", (\"tooltip\" \"inline\"))  ↦  \"inline\"   ---a :graph: section's property is a string"
  (let ((value (format "%s" (or (plist-get kwdargs key) default))))
    (if (member value choices) value
      (user-error "graph: `%s %s' should be one of %s" key value (s-join ", " choices)))))

(defun org--graph-parse (raw-contents &optional kwdargs)
  "RAW-CONTENTS of a #+begin_graph block ↦ (PREAMBLE STATEMENTS CTX), as the header's KWDARGS say:
`:wrap N' fills labels to N characters; `:notes inline' shows notes in labels, rather than as tooltips; and
`:unknown-tags random-templates' makes up a template for each tag that names none, rather than erring.
A template is the graph's own #+name: table, else the buffer's, else one of `org-graph-checkbox-templates', or of
`org-graph-builtin-templates'.
A note outside every item is the graph's own; PREAMBLE's :body holds it under `:notes inline'."
  (let* ((wrap     (-some--> (plist-get kwdargs :wrap) (if (numberp it) it (string-to-number (format "%s" it)))))
         (inline   (equal "inline" (org--graph-option kwdargs :notes "tooltip" '("tooltip" "inline"))))
         (org--graph-random-templates (and (equal "random-templates" (org--graph-option kwdargs :unknown-tags "random-templates"
                                                                                        '("error" "random-templates")))
                                           0))
         (buffer   (org--graph-buffer-templates))
         (raw      (let ((org--graph-strict t)) (org--graph-read raw-contents)))
         (org--graph-radio (plist-get raw :radio))
         (tables   (plist-get raw :name-tables))
         ;; Before `org--graph-expand-scope' expands them away, `#+edge: :faint:' style arcs.
         (edge-uses (--map (cons (concat ":edge." (car it)) (cdr it)) (plist-get raw :edge-rows)))
         (_        (maphash (lambda (name rows) (unless (gethash name tables) (puthash name rows tables))) buffer))
         (_        (pcase-dolist (`(,name . ,rows) (append org-graph-checkbox-templates org-graph-builtin-templates))
                     (unless (gethash name tables) (puthash name rows tables))))
         (preamble (org--graph-expand-scope raw tables))
         (preamble (-let [(notes . edge-notes) (org--graph-item-notes preamble nil)]
                     (when edge-notes
                       (user-error "graph: a `#+begin_note :edge' outside every item has no arc to describe"))
                     (if inline
                         (plist-put preamble :body (org--graph-notes-body notes (or wrap 40)))
                       (plist-put preamble :graph-rows (org--graph-override
                                                        (-some--> (org--graph-notes-tooltip notes) `(("tooltip" ,it)))
                                                        (plist-get preamble :graph-rows))))))
         (ctx      (org--graph-ctx-create :name-tables tables :wrap wrap :inline inline))
         (_        (org--graph-note-arc-templates ctx nil edge-uses))
         (items    (plist-get preamble :items))
         (stmts    (append (--mapcat (org--graph-parse-item it nil ctx) items)
                           (org--graph-ordered-rank (plist-get preamble :kind) items))))
    (list preamble stmts ctx)))

(provide 'org-graph-parse)
;;; org-graph-parse.el ends here
