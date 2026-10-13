;;; tests-graph.el --- Tests for org-special-block-extras-graph  -*- lexical-binding: t; -*-

;;; Requires

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 's)
(require 'dash)

(load-file (expand-file-name "~/snap/snap.el"))
(load-file "./org-special-block-extras.el")
(load-file "./org-graph-block.el")

;;; Test fixtures

;; Identity fixture for readable ERT test names
(snap-define-fixture deftest)

;; Relation: Org graph syntax → dot output
;; :input  is the raw block body (string)
;; :output is the expected dot source (string)
;; C-u C-x C-e on any `define-graph-test' call auto-fills :output from actual.
(snap-define-relation graph (input output)
  "Verify that INPUT (a #+begin_graph block body) produces OUTPUT (dot source)."
  (let ((actual (org--graph-raw->dot input "directed")))
    (should (equal output actual))
    (list :output actual)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org--graph-parse-table

(deftest "`org--graph-parse-table' returns nil on nil input"
  (should (null (org--graph-parse-table nil))))

(deftest "`org--graph-parse-table' returns nil on blank input"
  (should (null (org--graph-parse-table "   "))))

(deftest "`org--graph-parse-table' parses a basic 2-col table"
  (should (equal '(("shape" "ellipse") ("color" "red"))
                 (org--graph-parse-table "| shape | ellipse |\n| color | red |"))))

(deftest "`org--graph-parse-table' skips separator rows"
  (should (equal '(("key" "value") ("foo" "bar"))
                 (org--graph-parse-table "| key | value |\n|-----+-------|\n| foo | bar |"))))

(deftest "`org--graph-parse-table' parses a 3-col row"
  (should (equal '(("to" "mynode" "label=hello style=dashed"))
                 (org--graph-parse-table "| to | mynode | label=hello style=dashed |"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org--graph-parse-attrs

(deftest "`org--graph-parse-attrs' returns empty string on nil"
  (should (equal "" (org--graph-parse-attrs nil))))

(deftest "`org--graph-parse-attrs' parses unquoted key=value pairs"
  (should (equal "style=dashed color=red"
                 (org--graph-parse-attrs "style=dashed color=red"))))

(deftest "`org--graph-parse-attrs' parses quoted values containing spaces"
  (should (equal "label=\"hello world\""
                 (org--graph-parse-attrs "label=\"hello world\""))))

(deftest "`org--graph-parse-attrs' handles mixed quoted and unquoted pairs"
  (should (equal "label=\"foo bar\" style=dashed penwidth=2"
                 (org--graph-parse-attrs "label=\"foo bar\" style=dashed penwidth=2"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org--graph-extract-preamble

(deftest "`org--graph-extract-preamble' buckets #+graph: table rows"
  (let* ((p (org--graph-extract-preamble
             "#+graph:\n| rankdir | TB |\n| bgcolor | #000 |\n\n+ A\n  + B\n")))
    (should (equal '(("rankdir" "TB") ("bgcolor" "#000"))
                   (plist-get p :graph-rows)))
    (should (null (plist-get p :node-rows)))
    (should (s-contains-p "+ A" (plist-get p :list-body)))))

(deftest "`org--graph-extract-preamble' buckets #+node: and #+edge: rows"
  (let* ((p (org--graph-extract-preamble
             "#+node:\n| shape | box |\n#+edge:\n| color | red |\n+ X\n")))
    (should (equal '(("shape" "box")) (plist-get p :node-rows)))
    (should (equal '(("color" "red")) (plist-get p :edge-rows)))))

(deftest "`org--graph-extract-preamble' collects #+rank: lines"
  (let* ((p (org--graph-extract-preamble "#+rank: A B\n#+rank: C D\n+ A\n")))
    (should (equal '("A B" "C D") (plist-get p :ranks)))))

(deftest "`org--graph-extract-preamble' parses #+name: tables into name-tables"
  (let* ((p (org--graph-extract-preamble
             "#+name: wife\n| fillcolor | \"#f8d7e8\" |\n| color | \"#aa3366\" |\n\n+ A\n")))
    (let ((nt (plist-get p :name-tables)))
      (should (hash-table-p nt))
      (should (equal '(("fillcolor" "\"#f8d7e8\"") ("color" "\"#aa3366\""))
                     (gethash "wife" nt))))))

(deftest "`org--graph-extract-preamble' strips #+name: lines from list-body"
  (let* ((p (org--graph-extract-preamble
             "#+name: married\n| color | red |\n\n+ A\n  + B\n")))
    (should (not (s-contains-p "#+name:" (plist-get p :list-body))))
    (should (not (s-contains-p "| color | red |" (plist-get p :list-body))))
    (should (s-contains-p "+ A" (plist-get p :list-body)))))

(deftest "`org--graph-expand-eval-rows' splices template rows, explicit rows win"
  (let* ((nt (make-hash-table :test #'equal)))
    (puthash "wife" '(("fillcolor" "\"#f8d7e8\"") ("color" "\"#aa3366\"")) nt)
    (let* ((rows '(("id" "x") ("eval" "wife") ("color" "\"#ff0000\"")))
           (expanded (org--graph-expand-eval-rows rows nt)))
      ;; fillcolor comes from template (not overridden)
      (should (member '("fillcolor" "\"#f8d7e8\"") expanded))
      ;; explicit color beats template color
      (should (member '("color" "\"#ff0000\"") expanded))
      (should (not (member '("color" "\"#aa3366\"") expanded)))
      ;; id row is preserved
      (should (member '("id" "x") expanded))
      ;; eval row itself is gone
      (should (not (--any? (equal (car it) "eval") expanded))))))

(deftest "`org--graph-expand-eval-in-attr-str' expands (eval NAME) in edge attr strings"
  (let* ((nt (make-hash-table :test #'equal)))
    (puthash "married" '(("color" "\"#cc3399\"") ("dir" "both") ("style" "solid")) nt)
    (let ((expanded (org--graph-expand-eval-in-attr-str "(eval married)" nt)))
      (should (s-contains-p "color=\"#cc3399\"" expanded))
      (should (s-contains-p "dir=both" expanded))
      (should (s-contains-p "style=solid" expanded)))))

(deftest "`org--graph-expand-eval-in-attr-str' explicit tokens override template"
  (let* ((nt (make-hash-table :test #'equal)))
    (puthash "married" '(("color" "\"#cc3399\"") ("dir" "both") ("style" "solid")) nt)
    (let ((expanded (org--graph-expand-eval-in-attr-str "(eval married) color=red" nt)))
      ;; explicit color=red overrides template color="#cc3399"
      (should (s-contains-p "color=red" expanded))
      (should (not (s-contains-p "color=\"#cc3399\"" expanded)))
      ;; non-overridden template keys still present
      (should (s-contains-p "dir=both" expanded))
      (should (s-contains-p "style=solid" expanded)))))

(deftest "`org--graph-expand-eval-in-attr-str' returns original string unchanged when no (eval …)"
  (let* ((nt (make-hash-table :test #'equal)))
    (should (equal "color=red style=dashed"
                   (org--graph-expand-eval-in-attr-str "color=red style=dashed" nt)))
    (should (null (org--graph-expand-eval-in-attr-str nil nt)))))

(deftest "#+name: template expands into node attrs via | eval |"
  (let* ((dot (org--graph-raw->dot
               "#+name: wife\n| fillcolor | \"#f8d7e8\" |\n| color | \"#aa3366\" |\n\n+ Khadija\n  | eval | wife |\n"
               "directed")))
    (should (s-contains-p "fillcolor=\"#f8d7e8\"" dot))
    (should (s-contains-p "color=\"#aa3366\"" dot))))

(deftest "#+name: template expands into to-edge attrs via (eval NAME)"
  (let* ((dot (org--graph-raw->dot
               "#+name: married\n| color | \"#cc3399\" |\n| dir | both |\n| style | solid |\n\n+ A\n  | to | B | (eval married) |\n+ B\n"
               "directed")))
    (should (s-contains-p "color=\"#cc3399\"" dot))
    (should (s-contains-p "dir=both" dot))
    (should (s-contains-p "style=solid" dot))))

(deftest "#+name: explicit node attr overrides template via | eval |"
  (let* ((dot (org--graph-raw->dot
               "#+name: base\n| color | blue |\n| shape | box |\n\n+ A\n  | eval  | base  |\n  | color | red   |\n"
               "directed")))
    ;; explicit color=red wins over template color=blue
    (should (s-contains-p "color=red" dot))
    (should (not (s-contains-p "color=blue" dot)))
    ;; template shape=box is preserved
    (should (s-contains-p "shape=box" dot))))

(deftest "#+name: unknown template name silently produces empty expansion"
  (let* ((dot (org--graph-raw->dot
               "#+name: real\n| color | blue |\n\n+ A\n  | eval | typo |\n"
               "directed")))
    ;; node still renders, just without template attrs
    (should (s-contains-p "A [ label=A ];" dot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org--graph-raw->dot — snapshot relation tests

(define-graph-test "single node"
  :input "+ A\n"
  :output "digraph {
  A [ label=A ];
}")

(deftest "`org--graph-raw->dot' undirected graph uses -- arrows and graph keyword"
  (should (equal "graph {\n  A [ label=A ];\n  A -- B;\n  B [ label=B ];\n}"
                 (org--graph-raw->dot "+ A\n  + B\n" "undirected"))))

(define-graph-test "simple nesting A→B"
  :input "+ A\n  + B\n"
  :output "digraph {
  A [ label=A ];
  A -> B;
  B [ label=B ];
}")

(define-graph-test "A→B→C chain"
  :input "+ A\n  + B\n    + C\n"
  :output "digraph {
  A [ label=A ];
  A -> B;
  B [ label=B ];
  B -> C;
  C [ label=C ];
}")

(define-graph-test "EDGE row attaches label"
  :input "+ A\n  + B\n    | EDGE | label=calls |\n"
  :output "digraph {
  A [ label=A ];
  A -> B [ label=calls ];
  B [ label=B ];
}")

(define-graph-test "two EDGE rows produce two parallel arcs"
  :input "+ A\n  + B\n    | EDGE | label=first |\n    | EDGE | label=second |\n"
  :output "digraph {
  A [ label=A ];
  A -> B [ label=first ];
  A -> B [ label=second ];
  B [ label=B ];
}")

(define-graph-test "id row makes cross-reference alias"
  :input "+ lambda\n  | id | lam |\n  + church\n    | EDGE | label=encodes |\n"
  :output "digraph {
  lam [ label=lambda ];
  lam -> church [ label=encodes ];
  church [ label=church ];
}")

(define-graph-test "second occurrence emits edge only, not a second node decl"
  :input "+ A\n  + B\n+ C\n  + B\n    | EDGE | label=again |\n"
  :output "digraph {
  A [ label=A ];
  A -> B;
  B [ label=B ];
  C [ label=C ];
  C -> B [ label=again ];
}")

(deftest "`org--graph-raw->dot' explicit to row produces cross-edge"
  (should (equal "digraph {\n  a [ label=A ];\n  B -> a [ label=back ];\n  B [ label=B ];\n}"
                 (org--graph-raw->dot
                  (concat "+ A\n  | id | a |\n"
                          "+ B\n  | to | a | label=back |\n")
                  "directed"))))

(define-graph-test "#+rank: emits rank=same block for nodes without a common parent"
  :input "#+rank: A B\n+ A\n+ B\n"
  :output "digraph {
  { rank=same; A; B }
  A [ label=A ];
  B [ label=B ];
}")

(define-graph-test "rank=same on a node constrains its immediate children"
  :input "+ root\n  | rank | same |\n  + A\n  + B\n  + C\n"
  :output "digraph {
  root [ label=root ];
  root -> A;
  A [ label=A ];
  root -> B;
  B [ label=B ];
  root -> C;
  C [ label=C ];
  { rank=same; A; B; C }
}")

(define-graph-test "rank=source on a node constrains its immediate children"
  :input "+ entry\n  | rank | source |\n  + X\n  + Y\n"
  :output "digraph {
  entry [ label=entry ];
  entry -> X;
  X [ label=X ];
  entry -> Y;
  Y [ label=Y ];
  { rank=source; X; Y }
}")

(define-graph-test "rank row respects child id aliases"
  :input "+ root\n  | rank | same |\n  + alpha\n    | id | a |\n  + beta\n    | id | b |\n"
  :output "digraph {
  root [ label=root ];
  root -> a;
  a [ label=alpha ];
  root -> b;
  b [ label=beta ];
  { rank=same; a; b }
}")

(define-graph-test "#+graph: table sets graph-level attrs"
  :input "#+graph:\n| rankdir | TB |\n+ A\n"
  :output "digraph {
  graph [ rankdir=TB ]
  A [ label=A ];
}")

(define-graph-test "#+node: table sets default node attrs"
  :input "#+node:\n| shape | box |\n+ A\n"
  :output "digraph {
  node [ shape=box ]
  A [ label=A ];
}")

(define-graph-test "node attrs styled inline"
  :input "+ A\n  | shape | ellipse |\n  | color | red |\n"
  :output "digraph {
  A [ shape=ellipse color=red label=A ];
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org--graph-dot-id

(deftest "`org--graph-dot-id' leaves plain identifiers unchanged"
  (should (equal "lambda"   (org--graph-dot-id "lambda")))
  (should (equal "node_1"   (org--graph-dot-id "node_1")))
  (should (equal "A"        (org--graph-dot-id "A"))))

(deftest "`org--graph-dot-id' leaves already-quoted strings unchanged"
  (should (equal "\"Org mode\""   (org--graph-dot-id "\"Org mode\"")))
  (should (equal "\"hello world\"" (org--graph-dot-id "\"hello world\""))))

(deftest "`org--graph-dot-id' leaves bare numbers unchanged"
  (should (equal "42"   (org--graph-dot-id "42")))
  (should (equal "-3.14" (org--graph-dot-id "-3.14"))))

(deftest "`org--graph-dot-id' wraps multi-word names in double quotes"
  (should (equal "\"Org mode\""   (org--graph-dot-id "Org mode")))
  (should (equal "\"Org agenda\"" (org--graph-dot-id "Org agenda"))))

(deftest "`org--graph-dot-id' wraps names with hyphens in double quotes"
  (should (equal "\"my-node\"" (org--graph-dot-id "my-node"))))

(deftest "`org--graph-dot-id' escapes interior quotes"
  (should (equal "\"say \\\"hi\\\"\""
                 (org--graph-dot-id "say \"hi\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org--graph-raw->dot — multi-word nodes and Org comments

(deftest "`org--graph-raw->dot' auto-quotes multi-word node names"
  (let ((dot (org--graph-raw->dot "+ Org mode\n" "directed")))
    (should (s-contains-p "\"Org mode\"" dot))))

(deftest "`org--graph-raw->dot' auto-quotes on both ends of an edge"
  (let ((dot (org--graph-raw->dot "+ Org mode\n  + Org agenda\n" "directed")))
    (should (s-contains-p "\"Org mode\" -> \"Org agenda\"" dot))))

(deftest "`org--graph-raw->dot' ignores bare Org comment lines"
  (let ((dot (org--graph-raw->dot
              "+ A\n  # this is a comment\n  + B\n"
              "directed")))
    (should (s-contains-p "A -> B" dot))))

(deftest "`org--graph-raw->dot' comment before table row does not drop the table"
  ;; A # comment between a list item and its inline table must not lose the table
  (let ((dot (org--graph-raw->dot
              "+ A\n  + B\n    # a comment\n    | EDGE | label=calls |\n"
              "directed")))
    (should (s-contains-p "label=calls" dot))))

(deftest "`org--graph-raw->dot' mixed comments and nodes produce correct graph"
  (let ((dot (org--graph-raw->dot
              (concat "+ Emacs\n"
                      "  + \"Org mode\"\n"
                      "    # comment\n"
                      "    + \"Org agenda\"\n"
                      "      # another comment\n"
                      "      | EDGE | label=\"hola\" |\n")
              "directed")))
    (should (s-contains-p "\"Org mode\" -> \"Org agenda\"" dot))
    (should (s-contains-p "label=\"hola\"" dot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-graph--table-context

(defun tests-graph--make-graph-buffer (content)
  "Return a temp buffer containing CONTENT wrapped in a #+begin_graph block.
Leaves point at the end of CONTENT so callers can position it as needed."
  (let ((buf (generate-new-buffer " *test-graph-context*")))
    (with-current-buffer buf
      (org-mode)
      (insert "#+begin_graph\n")
      (insert content)
      (insert "#+end_graph\n"))
    buf))

(deftest "`org-graph--table-context' returns `nodes' for a list-item table"
  (let ((buf (tests-graph--make-graph-buffer
              "+ mynode\n  | shape | ellipse |\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| shape |")
      (should (eq 'nodes (org-graph--table-context))))
    (kill-buffer buf)))

(deftest "`org-graph--table-context' returns `graphs' for a #+graph: table"
  (let ((buf (tests-graph--make-graph-buffer
              "#+graph:\n| rankdir | TB |\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| rankdir |")
      (should (eq 'graphs (org-graph--table-context))))
    (kill-buffer buf)))

(deftest "`org-graph--table-context' returns `nodes' for a #+node: table"
  (let ((buf (tests-graph--make-graph-buffer
              "#+node:\n| shape | box |\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| shape |")
      (should (eq 'nodes (org-graph--table-context))))
    (kill-buffer buf)))

(deftest "`org-graph--table-context' returns `edges' for a #+edge: table"
  (let ((buf (tests-graph--make-graph-buffer
              "#+edge:\n| color | red |\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| color |")
      (should (eq 'edges (org-graph--table-context))))
    (kill-buffer buf)))

(deftest "`org-graph--table-context' skips separator rows correctly"
  (let ((buf (tests-graph--make-graph-buffer
              "#+graph:\n|------+----|\n| rankdir | LR |\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| rankdir |")
      (should (eq 'graphs (org-graph--table-context))))
    (kill-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-graph-attributes defcustom

(deftest "`org-graph-attributes' contains at least 20 entries"
  (should (>= (length org-graph-attributes) 20)))

(deftest "`org-graph-attributes' every entry has required plist keys"
  (dolist (entry org-graph-attributes)
    (should (plist-get entry :name))
    (should (plist-get entry :description))
    (should (plist-get entry :valid-on))))

(deftest "`org-graph-attributes' shape is valid-on nodes only"
  (let ((shape (--first (equal (plist-get it :name) "shape") org-graph-attributes)))
    (should shape)
    (should (equal '(nodes) (plist-get shape :valid-on)))
    (should (member "ellipse" (plist-get shape :values)))))

(deftest "`org-graph-attributes' rankdir is valid-on graphs only"
  (let ((attr (--first (equal (plist-get it :name) "rankdir") org-graph-attributes)))
    (should attr)
    (should (equal '(graphs) (plist-get attr :valid-on)))
    (should (member "LR" (plist-get attr :values)))))

(deftest "`org-graph-attributes' label is valid-on all four contexts"
  (let ((attr (--first (equal (plist-get it :name) "label") org-graph-attributes)))
    (should attr)
    (should (memq 'nodes    (plist-get attr :valid-on)))
    (should (memq 'edges    (plist-get attr :valid-on)))
    (should (memq 'graphs   (plist-get attr :valid-on)))
    (should (memq 'clusters (plist-get attr :valid-on)))
    (should (null (plist-get attr :values)))))

(deftest "`org-graph-attributes' arrowhead is valid-on edges only"
  (let ((attr (--first (equal (plist-get it :name) "arrowhead") org-graph-attributes)))
    (should attr)
    (should (equal '(edges) (plist-get attr :valid-on)))
    (should (member "vee" (plist-get attr :values)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-graph--insert-attribute-row

(deftest "`org-graph--insert-attribute-row' inserts a new row and aligns"
  (let ((buf (tests-graph--make-graph-buffer
              "#+node:\n| shape | box |\n")))
    (with-current-buffer buf
      ;; Position point on the existing table row
      (goto-char (point-min))
      (search-forward "| shape |")
      ;; Simulate C-u RET: intercept completing-read and read-string.
      ;; Stub pick-color to nil so the color attr falls through to read-string.
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _)
                   (if (string-match-p "Attribute" prompt)
                       ;; Pick "color — Border/line color…"
                       (car (--filter (string-prefix-p "color" it) choices))
                     ;; No fixed values expected here, but handle if needed
                     (car choices))))
                ((symbol-function 'read-string)
                 (lambda (_prompt &rest _) "#ff0000"))
                ((symbol-function 'org-graph--pick-color)
                 (lambda () nil)))
        (org-graph--insert-attribute-row '(4))))
    (with-current-buffer buf
      (let ((contents (buffer-string)))
        (should (s-contains-p "| color" contents))
        (should (s-contains-p "#ff0000" contents))))
    (kill-buffer buf)))

(deftest "`org-graph--insert-attribute-row' is a no-op without prefix arg"
  (let ((buf (tests-graph--make-graph-buffer
              "#+node:\n| shape | box |\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| shape |")
      (let ((before (buffer-string)))
        (org-graph--insert-attribute-row nil)
        (should (equal before (buffer-string)))))
    (kill-buffer buf)))

(deftest "`org-graph--insert-attribute-row' filters by context — no shape in graphs"
  (let ((buf (tests-graph--make-graph-buffer
              "#+graph:\n| rankdir | TB |\n"))
        (offered nil))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| rankdir |")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (setq offered choices)
                   ;; Abort after capturing choices
                   (keyboard-quit))))
        (condition-case nil
            (org-graph--insert-attribute-row '(4))
          (quit nil))))
    (kill-buffer buf)
    ;; shape is nodes-only — must not appear in graphs context
    (should (not (--any? (string-prefix-p "shape" it) offered)))
    ;; rankdir is graphs-only — must appear
    (should (--any? (string-prefix-p "rankdir" it) offered))))

(deftest "`org-graph--insert-attribute-row' falls back to read-string when org-graph--pick-color returns nil"
  ;; Simulates helm being absent or the user aborting without selecting a color.
  (let ((buf (tests-graph--make-graph-buffer "#+node:\n| shape | box |\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| shape |")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _)
                   (if (string-match-p "Attribute" prompt)
                       (car (--filter (string-prefix-p "color" it) choices))
                     (car choices))))
                ((symbol-function 'read-string)
                 (lambda (_prompt &rest _) "#aabbcc"))
                ;; Stub pick-color to return nil (helm absent / aborted)
                ((symbol-function 'org-graph--pick-color)
                 (lambda () nil)))
        (org-graph--insert-attribute-row '(4)))
      (should (s-contains-p "| color" (buffer-string)))
      (should (s-contains-p "#aabbcc" (buffer-string))))
    (kill-buffer buf)))

(deftest "plain RET inside a graph block table runs org-return, not attribute insertion"
  (let ((buf (tests-graph--make-graph-buffer "#+node:\n| shape | box |\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| shape |")
      (end-of-line)
      (let ((before (buffer-string)))
        ;; Call the advice with no prefix arg (plain RET scenario)
        (let ((result (org-graph--maybe-insert-attribute-row nil nil t)))
          ;; Advice must return nil so :before-until lets org-return through
          (should (null result))
          ;; Buffer must be unchanged — no | key | value | row was inserted
          (should (equal before (buffer-string))))))
    (kill-buffer buf)))

(deftest "C-u RET on a graph table triggers attribute insertion and suppresses org-return"
  (let ((buf (tests-graph--make-graph-buffer "#+node:\n| shape | box |\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "| shape |")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _)
                   (if (string-match-p "Attribute" prompt)
                       ;; Pick "style" — has fixed :values, avoids the helm branch
                       (car (--filter (string-prefix-p "style" it) choices))
                     ;; Value completion: pick first fixed option ("filled")
                     (car choices)))))
        ;; Call the advice with a C-u prefix arg
        (let ((result (org-graph--maybe-insert-attribute-row nil '(4) t)))
          ;; Advice must return t so :before-until suppresses org-return
          (should result)
          ;; The new attribute row must have been inserted
          (should (s-contains-p "| style" (buffer-string)))
          (should (s-contains-p "filled" (buffer-string))))))
    (kill-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-graph--cc-handler — C-u / :C-c_C-c dot XOR logic

;; Shared macro: stub out the org-special-block-* functions and call-process
;; so tests run without graphviz or a real org export pipeline.
(defmacro tests-graph--with-cc-stubs (dot-source kwdargs &rest body)
  "Evaluate BODY with org-special-block-* and call-process stubbed.
DOT-SOURCE is the string returned by `org--graph-raw->dot'.
KWDARGS is the plist for the block's keyword arguments."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'org-special-block-after-point)
              (lambda (_)
                (make-org-special-block
                 :kwdargs  ,kwdargs
                 :contents "+ A\n"
                 :main-arg "directed"
                 :end-point (point-max))))
             ((symbol-function 'org--graph-raw->dot)             (lambda (_ _2) ,dot-source))
             ((symbol-function 'call-process)                    (lambda (&rest _) 0))
             ((symbol-function 'write-region)                    (lambda (&rest _) nil))
             ((symbol-function 'make-temp-file)                  (lambda (&rest _) "/tmp/fake.dot"))
             ((symbol-function 'generate-new-buffer)             (lambda (&rest _) (get-buffer-create " *org-graph-err-stub*")))
             ((symbol-function 'org-display-inline-images)       (lambda (&rest _) nil))
             ((symbol-function 'org-graph--insert-results)       (lambda (&rest _) nil)))
     ,@body))

(deftest "`org-graph--cc-handler' plain C-c C-c renders PNG (no dot buffer)"
  (let ((buf (tests-graph--make-graph-buffer "+ A\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (tests-graph--with-cc-stubs "digraph { A; }" nil
        (let ((current-prefix-arg nil)
              (dot-shown nil))
          (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (_) (setq dot-shown t))))
            (org-graph--cc-handler)
            (should-not dot-shown)))))
    (kill-buffer buf)))

(deftest "`org-graph--cc-handler' C-u C-c C-c shows dot source buffer"
  (let ((buf (tests-graph--make-graph-buffer "+ A\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (tests-graph--with-cc-stubs "digraph { A; }" nil
        (let ((current-prefix-arg '(4))
              (dot-received nil))
          (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot-received d))))
            (org-graph--cc-handler)
            (should (equal "digraph { A; }" dot-received))))))
    (kill-buffer buf)))

(deftest "`org-graph--cc-handler' :C-c_C-c dot without prefix shows dot"
  (let ((buf (tests-graph--make-graph-buffer "+ A\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (tests-graph--with-cc-stubs "digraph { A; }" '(:C-c_C-c "dot")
        (let ((current-prefix-arg nil)
              (dot-received nil))
          (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (d) (setq dot-received d))))
            (org-graph--cc-handler)
            (should (equal "digraph { A; }" dot-received))))))
    (kill-buffer buf)))

(deftest "`org-graph--cc-handler' :C-c_C-c dot WITH prefix toggles back to PNG"
  (let ((buf (tests-graph--make-graph-buffer "+ A\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (tests-graph--with-cc-stubs "digraph { A; }" '(:C-c_C-c "dot")
        (let ((current-prefix-arg '(4))
              (dot-shown nil))
          (cl-letf (((symbol-function 'org-graph--show-dot) (lambda (_) (setq dot-shown t))))
            (org-graph--cc-handler)
            (should-not dot-shown)))))
    (kill-buffer buf)))

(deftest "`org-graph--cc-handler' C-u inserts #+begin_src dot results block"
  (let ((buf (tests-graph--make-graph-buffer "+ A\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      ;; Override end-point to insert after current content (not point-max stub)
      (cl-letf (((symbol-function 'org-special-block-after-point)
                 (lambda (_)
                   (make-org-special-block
                    :kwdargs  nil
                    :contents "+ A\n"
                    :main-arg "directed"
                    :end-point (save-excursion
                                 (goto-char (point-max))
                                 (search-backward "#+end_graph")
                                 (1- (point))))))
                ((symbol-function 'org--graph-raw->dot)             (lambda (_ _2) "digraph { A; }"))
                ((symbol-function 'org-graph--show-dot)             (lambda (_) nil)))
        (let ((current-prefix-arg '(4)))
          (org-graph--cc-handler)))
      (let ((contents (buffer-string)))
        (should (s-contains-p "#+RESULTS:" contents))
        (should (s-contains-p "#+begin_src dot" contents))
        (should (s-contains-p "digraph { A; }" contents))
        (should (s-contains-p "#+end_src" contents))))
    (kill-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML export

(deftest "`#+begin_graph' HTML export produces an <img> tag"
  ;; Call the graph block handler directly: simulate what org-export-string
  ;; does for a #+begin_graph block, bypassing the full export pipeline
  ;; (which triggers org-defblock docstring enrichment and breaks in batch).
  (cl-letf (((symbol-function 'org-graph--render-to-png)
             (lambda (_dot pngfile)
               ;; Write a minimal 1×1 white PNG so base64 encoding succeeds.
               (with-temp-file pngfile
                 (set-buffer-multibyte nil)
                 (insert "\x89PNG\r\n\x1a\n"))
               t)))
    (let ((html (org-block/graph 'html "+ A\n  + B\n    | EDGE | label=calls |\n"
                                 "directed" nil)))
      (should (s-contains-p "<img" html))
      (should (s-contains-p "base64" html)))))

(deftest "`#+begin_graph' HTML export: node and edge appear in underlying dot"
  ;; Verify the dot source that gets embedded is structurally correct
  ;; by checking the raw->dot output directly.
  (let ((dot (org--graph-raw->dot
              "+ A\n  + B\n    | EDGE | label=calls |\n"
              "directed")))
    (should (s-contains-p "A -> B [ label=calls ];" dot))
    (should (s-contains-p "digraph" dot))))

(deftest "`#+begin_graph' :svg t — HTML export embeds inline <svg>"
  ;; With :svg t the handler calls org-graph--render-to-svg and returns
  ;; the raw <svg>…</svg> string for inline embedding (no base64, no <img>).
  (cl-letf (((symbol-function 'org-graph--render-to-svg)
             (lambda (_dot svgfile)
               (with-temp-file svgfile
                 (insert "<?xml version=\"1.0\"?>\n<!DOCTYPE svg>\n<svg xmlns=\"http://www.w3.org/2000/svg\"><g><title>A</title></g></svg>\n"))
               svgfile)))
    (let ((html (org-block/graph 'html "+ A\n" "directed" :svg t)))
      (should (s-contains-p "<svg" html))
      (should (s-contains-p "<title>A</title>" html))
      (should-not (s-contains-p "<img" html))
      (should-not (s-contains-p "base64" html)))))

(deftest "`#+begin_graph' :svg t — XML preamble is stripped"
  ;; org-graph--svg-contents must strip the <?xml …?> and <!DOCTYPE> lines
  ;; so only the <svg>…</svg> element is embedded in the HTML.
  (cl-letf (((symbol-function 'org-graph--render-to-svg)
             (lambda (_dot svgfile)
               (with-temp-file svgfile
                 (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"\">\n<svg xmlns=\"http://www.w3.org/2000/svg\"><g/></svg>\n"))
               svgfile)))
    (let ((html (org-block/graph 'html "+ A\n" "directed" :svg t)))
      (should-not (s-contains-p "<?xml" html))
      (should-not (s-contains-p "<!DOCTYPE" html))
      (should (s-contains-p "<svg" html)))))

(deftest "`#+begin_graph' :svg t — falls back to <pre> when dot unavailable"
  ;; If org-graph--render-to-svg returns nil (dot not installed), the
  ;; handler must fall back to a <pre> block with escaped dot source.
  (cl-letf (((symbol-function 'org-graph--render-to-svg)
             (lambda (_dot _svgfile) nil)))
    (let ((html (org-block/graph 'html "+ A\n  + B\n" "directed" :svg t)))
      (should (s-contains-p "<pre>" html))
      (should-not (s-contains-p "<svg" html)))))

(deftest "`#+begin_graph' no :svg — HTML export still produces base64 PNG"
  ;; Omitting :svg (svg=nil) must leave the existing PNG path untouched.
  (cl-letf (((symbol-function 'org-graph--render-to-png)
             (lambda (_dot pngfile)
               (with-temp-file pngfile
                 (set-buffer-multibyte nil)
                 (insert "\x89PNG\r\n\x1a\n"))
               t)))
    (let ((html (org-block/graph 'html "+ A\n" "directed")))
      (should (s-contains-p "<img" html))
      (should (s-contains-p "base64" html))
      (should-not (s-contains-p "<svg" html)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :SETTINGS: drawer folding

(define-graph-test ":SETTINGS: drawer wraps preamble directives"
  :input ":SETTINGS:\n#+graph:\n| rankdir | LR |\n#+node:\n| shape | box |\n:END:\n+ A\n"
  :output "digraph {
  graph [ rankdir=LR ]
  node [ shape=box ]
  A [ label=A ];
}")

(define-graph-test ":SETTINGS: drawer with #+rank: and #+edge:"
  :input ":SETTINGS:\n#+edge:\n| color | red |\n#+rank: A B\n:END:\n+ A\n+ B\n"
  :output "digraph {
  edge [ color=red ]
  { rank=same; A; B }
  A [ label=A ];
  B [ label=B ];
}")

(define-graph-test ":SETTINGS: drawer with blank lines between directives"
  :input ":SETTINGS:\n#+graph:\n| rankdir | TB |\n\n#+node:\n| shape | ellipse |\n:END:\n+ X\n"
  :output "digraph {
  graph [ rankdir=TB ]
  node [ shape=ellipse ]
  X [ label=X ];
}")

(deftest "bare preamble and :SETTINGS: drawer produce identical output"
  (let ((bare   (org--graph-raw->dot "#+graph:\n| rankdir | LR |\n#+node:\n| color | red |\n+ A\n" "directed"))
        (drawer (org--graph-raw->dot ":SETTINGS:\n#+graph:\n| rankdir | LR |\n#+node:\n| color | red |\n:END:\n+ A\n" "directed")))
    (should (equal bare drawer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Preamble + list-body edge cases

(define-graph-test "regression: blank line between last preamble directive and first list item"
  ;; A blank line between a preamble directive and the first list item caused
  ;; org-list-to-lisp to be called on a buffer starting with a newline,
  ;; crashing with (max) / 0 arguments.  s-trim in org--graph-raw->dot fixes this.
  :input "#+graph:\n| rankdir | TB |\n\n+ G\n  | shape | cluster |\n  | label | \"G\" |\n  | color | red |\n  + A\n"
  :output "digraph {
  graph [ rankdir=TB ]
  subgraph cluster_G {
    label=\"G\"
    color=red
    A [ label=A ];
  }
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inline cluster (shape=cluster)

(define-graph-test "shape=cluster emits inline cluster block with attrs"
  :input "+ G\n  | id | grp |\n  | shape | cluster |\n  | color | red |\n  + A\n  + B\n"
  :output "digraph {
  subgraph cluster_grp {
    label=\"G\"
    color=red
    A [ label=A ];
    B [ label=B ];
  }
}")

(define-graph-test "shape=cluster: node name used as label, spaces normalised in cluster id"
  :input "+ My Group\n  | shape | cluster |\n  + X\n"
  :output "digraph {
  subgraph cluster_My_Group {
    label=\"My Group\"
    X [ label=X ];
  }
}")

(define-graph-test "shape=cluster: explicit label row overrides node name"
  :input "+ grp\n  | shape | cluster |\n  | label | \"The Group\" |\n  + A\n"
  :output "digraph {
  subgraph cluster_grp {
    label=\"The Group\"
    A [ label=A ];
  }
}")

(define-graph-test "shape=cluster: children carry their own attrs"
  :input "+ Legend\n  | id | legend |\n  | shape | cluster |\n  | style | filled |\n  + leg_solid\n    | id | leg_solid |\n    | label | \"solid = direct lineage\" |\n    | shape | plaintext |\n  + leg_dashed\n    | id | leg_dashed |\n    | label | \"dashed = inspiration\" |\n    | shape | plaintext |\n"
  :output "digraph {
  subgraph cluster_legend {
    label=\"Legend\"
    style=filled
    leg_solid [ label=\"solid = direct lineage\" shape=plaintext ];
    leg_dashed [ label=\"dashed = inspiration\" shape=plaintext ];
  }
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cluster-scoped directives (#+node: / #+edge: / #+graph: / #+rank: inside cluster item)

(define-graph-test "cluster: #+node: inside item sets default node attrs for children"
  :input "+ G\n  | shape | cluster |\n  #+node:\n  | shape | plaintext |\n  | fontcolor | \"#6c7086\" |\n  + Solid\n  + Dashed\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    node [ shape=plaintext fontcolor=\"#6c7086\" ]
    Solid [ label=Solid ];
    Dashed [ label=Dashed ];
  }
}")

(define-graph-test "cluster: #+edge: inside item sets default edge attrs for children"
  :input "+ G\n  | shape | cluster |\n  #+edge:\n  | style | dashed |\n  + A\n  + B\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    edge [ style=dashed ]
    A [ label=A ];
    B [ label=B ];
  }
}")

(define-graph-test "cluster: #+graph: inside item sets subgraph-level attrs"
  :input "+ G\n  | shape | cluster |\n  #+graph:\n  | bgcolor | \"#181825\" |\n  + A\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    graph [ bgcolor=\"#181825\" ]
    A [ label=A ];
  }
}")

(define-graph-test "cluster: #+rank: inside item emits rank constraint within subgraph"
  :input "+ G\n  | shape | cluster |\n  #+rank: A B\n  + A\n  + B\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    { rank=same; A; B }
    A [ label=A ];
    B [ label=B ];
  }
}")

(define-graph-test "cluster: #+node: and #+edge: together inside item"
  :input "+ G\n  | shape | cluster |\n  | color | red |\n  #+node:\n  | shape | plaintext |\n  #+edge:\n  | style | dotted |\n  + X\n  + Y\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    color=red
    node [ shape=plaintext ]
    edge [ style=dotted ]
    X [ label=X ];
    Y [ label=Y ];
  }
}")

(define-graph-test "cluster: child explicit attrs override #+node: defaults"
  ;; X carries its own shape=record, which must win over the cluster's #+node: shape=plaintext
  :input "+ G\n  | shape | cluster |\n  #+node:\n  | shape | plaintext |\n  + X\n    | shape | ellipse |\n  + Y\n"
  :output "digraph {
  subgraph cluster_G {
    label=\"G\"
    node [ shape=plaintext ]
    X [ shape=ellipse label=X ];
    Y [ label=Y ];
  }
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Record nodes

(define-graph-test "record node: three fields with port ids"
  :input "+ struct1\n  | shape | record |\n  + left\n    | id | f0 |\n  + mid dle\n    | id | f1 |\n  + right\n    | id | f2 |\n"
  :output "digraph {
  struct1 [ shape=record label=\"<f0> left|<f1> mid\\ dle|<f2> right\" ];
}")

(define-graph-test "record node: two records with cross-port edge"
  :input "+ struct1\n  | shape | record |\n  + left\n    | id | f0 |\n  + mid dle\n    | id | f1 |\n    | to | g0 |\n  + right\n    | id | f2 |\n+ struct2\n  | shape | record |\n  + one\n    | id | g0 |\n  + two\n    | id | g1 |\n"
  :output "digraph {
  struct1 [ shape=record label=\"<f0> left|<f1> mid\\ dle|<f2> right\" ];
  struct1:f1 -> struct2:g0;
  struct2 [ shape=record label=\"<g0> one|<g1> two\" ];
}")

(define-graph-test "record node: no id row — field name used as display label only, no port prefix"
  :input "+ box\n  | shape | record |\n  + alpha\n  + beta\n"
  :output "digraph {
  box [ shape=record label=\"alpha|beta\" ];
}")

(define-graph-test "Mrecord shape works identically to record"
  :input "+ rounded\n  | shape | Mrecord |\n  + x\n    | id | p0 |\n  + y\n    | id | p1 |\n"
  :output "digraph {
  rounded [ shape=Mrecord label=\"<p0> x|<p1> y\" ];
}")

(define-graph-test "record node preserves other node attrs beside shape"
  :input "+ s\n  | shape | record |\n  | color | red |\n  + a\n  + b\n"
  :output "digraph {
  s [ shape=record label=\"a|b\" color=red ];
}")

(define-graph-test "regression: field name with space normalises port to underscore form"
  ;; Without normalisation, `mid dle` as a port produces `struct1:mid dle` in the
  ;; edge statement, which Graphviz parses as node `struct1:mid` + stray node `dle`.
  ;; The `to` target `mid dle` has no explicit id row on the target field, so the
  ;; implicit port (spaces→underscores) is used for the edge but not the label.
  :input "+ s\n  | shape | record |\n  + mid dle\n    | to | mid dle |\n"
  :output "digraph {
  s [ shape=record label=\"mid\\ dle\" ];
  s:mid_dle -> s:mid_dle;
}")

(define-graph-test "{} child wraps fields in a layout-flip group"
  :input "+ s\n  | shape | record |\n  + a\n  + {}\n    + b\n    + c\n  + d\n"
  :output "digraph {
  s [ shape=record label=\"a|{b|c}|d\" ];
}")

(define-graph-test "nested {} groups flip orientation twice"
  :input "+ s\n  | shape | record |\n  + a\n  + {}\n    + b\n    + {}\n      + c\n      + d\n    + e\n  + f\n"
  :output "digraph {
  s [ shape=record label=\"a|{b|{c|d}|e}|f\" ];
}")

(define-graph-test "struct3: full Graphviz example with {} and port"
  :input "+ struct3\n  | shape | record |\n  + hello\\nworld\n  + {}\n    + b\n    + {}\n      + c\n      + d\n        | id | here |\n      + e\n    + f\n  + g\n  + h\n"
  :output "digraph {
  struct3 [ shape=record label=\"hello\\nworld|{b|{c|<here> d|e}|f}|g|h\" ];
}")

(define-graph-test "regression: ordinary node `to` row resolves port nested inside {} groups"
  ;; Port `x` is buried inside two {} groups; an ordinary node's `to` row must
  ;; still resolve it to struct3:x, not emit it as a bare (phantom) node.
  :input "+ struct3\n  | shape | record |\n  + {}\n    + {}\n      + d\n        | id | x |\n+ look\n  | to | x |\n"
  :output "digraph {
  struct3 [ shape=record label=\"{{<x> d}}\" ];
  look -> struct3:x;
  look [ label=look ];
}")

(define-graph-test "regression: bare port id in `to` row resolves to owner:port not bare id"
  ;; Field f1 of rec1 targets port g0; g0 belongs to rec2.
  ;; The emitted edge must be rec1:f1 -> rec2:g0, not rec1:f1 -> g0.
  :input "+ rec1\n  | shape | record |\n  + a\n    | id | f1 |\n    | to | g0 |\n+ rec2\n  | shape | record |\n  + b\n    | id | g0 |\n"
  :output "digraph {
  rec1 [ shape=record label=\"<f1> a\" ];
  rec1:f1 -> rec2:g0;
  rec2 [ shape=record label=\"<g0> b\" ];
}")

(define-graph-test "record node connected from a plain parent"
  :input "+ root\n  + rec\n    | shape | record |\n    + p\n    + q\n"
  :output "digraph {
  root [ label=root ];
  root -> rec;
  rec [ shape=record label=\"p|q\" ];
}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dispatcher integration — C-c C-c and C-c '

(deftest "org--block-cc-dispatcher routes C-c C-c into a graph block to org-graph--cc-handler"
  ;; This tests the full dispatch chain: org--block-cc-dispatcher →
  ;; org-special-block-at-point-p → org-graph--cc-handler.
  ;; We stub org-graph--cc-handler to record that it was called.
  (let ((buf (tests-graph--make-graph-buffer "+ A\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (let ((handler-called nil))
        (cl-letf (((symbol-function 'org-graph--cc-handler)
                   (lambda () (setq handler-called t) t)))
          (org--block-cc-dispatcher)
          (should handler-called))))
    (kill-buffer buf)))

(deftest "org--block-edit-special-dispatcher routes C-c ' into a graph block to org-graph--do-edit-special"
  ;; This tests the full dispatch chain: org--block-edit-special-dispatcher →
  ;; org-special-block-at-point-p → org-graph--do-edit-special.
  ;; We stub org-graph--do-edit-special to record that it was called.
  (let ((buf (tests-graph--make-graph-buffer "+ A\n")))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "+ A")
      (let ((handler-called nil))
        (cl-letf (((symbol-function 'org-graph--do-edit-special)
                   (lambda () (setq handler-called t) t)))
          (org--block-edit-special-dispatcher)
          (should handler-called))))
    (kill-buffer buf)))
