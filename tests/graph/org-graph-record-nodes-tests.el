;;; org-graph-record-nodes-tests.el --- Records: a table, unless | key | value | rows, is a node's <TABLE> label  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Records

(define-graph-test "an item's table, unless | key | value | rows, is the node's <TABLE> label: a cell's <<target>> is its port, and its [[link]] an arc from there"
  :input "
+ struct1
  | left | <<mid>> middle [[struct2::one]] | right |
+ struct2
  | <<one>> one | two |
+ reader
  + [[struct1::mid]]"
  :output "digraph {
  struct1:mid -> struct2:one;
  struct1 [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\"
CELLSPACING=\"0\"><TR><TD>left</TD><TD PORT=\"mid\">middle</TD><TD>right</TD></TR></TABLE>> ];
  struct2 [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD
PORT=\"one\">one</TD><TD>two</TD></TR></TABLE>> ];
  reader [ label=reader ];
  reader -> struct1:mid;
}")

(define-graph-test "`^' continues the cell above; an empty cell continues the one to its left, or, in the first column, the one above"
  :input "
+ struct3
  | hello \\\\ world | b |            |   | g | h |
  | ^              | c | <<here>> d | e | ^ | ^ |
  |                | f |            |   | ^ | ^ |
+ reader
  + [[struct3::here]]"
  :output "digraph {
  struct3 [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD
ROWSPAN=\"3\">hello<BR/>world</TD><TD COLSPAN=\"3\">b</TD><TD ROWSPAN=\"3\">g</TD><TD
ROWSPAN=\"3\">h</TD></TR><TR><TD>c</TD><TD PORT=\"here\">d</TD><TD>e</TD></TR><TR><TD
COLSPAN=\"3\">f</TD></TR></TABLE>> ];
  reader [ label=reader ];
  reader -> struct3:here;
}")

(define-graph-test "hlines draw rules, the table then having one outer border; a <l> cookie row aligns its column; cells keep markup"
  :input "
+ Person
  | <l>            |
  | *Person*       |
  |----------------|
  | name: =String= |
  | age: =Int=     |
  |----------------|
  | greet()        |"
  :output "digraph {
  Person [ shape=plain label=<<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\"><TR><TD ALIGN=\"LEFT\"
BALIGN=\"LEFT\"><B>Person</B></TD></TR><HR/><TR><TD ALIGN=\"LEFT\" BALIGN=\"LEFT\">name: <FONT
FACE=\"monospace\">String</FONT></TD></TR><TR><TD ALIGN=\"LEFT\" BALIGN=\"LEFT\">age: <FONT
FACE=\"monospace\">Int</FONT></TD></TR><HR/><TR><TD ALIGN=\"LEFT\"
BALIGN=\"LEFT\">greet()</TD></TR></TABLE>> ];
}")

(define-graph-test "a record's own attribute table, and a template's, sit beside its table; a user's shape beats `plain'"
  :input "
#+name: faint
| color | grey |

+ Point :faint:
  | shape | box |

  | x | y |"
  :output "digraph {
  Point [ label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\"
CELLSPACING=\"0\"><TR><TD>x</TD><TD>y</TD></TR></TABLE>> color=grey shape=box ];
}")

(define-graph-test "a cell's link label and `:edge[T].KEY' rows style its arc; a ported-by-text cell has spaces as underscores"
  :input "
+ <<lam>> λ-calculus
+ Lisp
  | :edge[lam].style | dotted |

  | car | cdr | eval apply [[lam][inspired by]] |"
  :output "digraph {
  lam [ label=\"λ-calculus\" ];
  Lisp:eval_apply -> lam [ label=\"inspired by\" style=dotted ];
  Lisp [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\"
CELLSPACING=\"0\"><TR><TD>car</TD><TD>cdr</TD><TD PORT=\"eval_apply\">eval apply</TD></TR></TABLE>> ];
}")

(define-graph-test "ports are scoped per record, record and port names may contain dots, and `::' after a non-record is text"
  :input "
+ r1
  | <<head>> head | tail |
+ org.el
  | <<head>> head | <<v1.2>> v1.2 |
+ z [[r1::head]] [[org.el::v1.2]] [[std::vector]]"
  :output "digraph {
  r1 [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD
PORT=\"head\">head</TD><TD>tail</TD></TR></TABLE>> ];
  \"org.el\" [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD
PORT=\"head\">head</TD><TD PORT=\"v1.2\">v1.2</TD></TR></TABLE>> ];
  z -> r1:head;
  z -> \"org.el\":\"v1.2\";
  z -> \"std::vector\";
  z [ label=z ];
}")

(define-graph-test "a record's children are ordinary nodes, its arcs leaving the record as a whole"
  :input "
+ Stack
  | <<top>> 3 | 2 | 1 |
  + push
  + pop"
  :output "digraph {
  Stack [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD
PORT=\"top\">3</TD><TD>2</TD><TD>1</TD></TR></TABLE>> ];
  Stack -> push;
  push [ label=push ];
  Stack -> pop;
  pop [ label=pop ];
}")

(deftest "a `^' in the first row, or an empty first cell, continues nothing: a user error"
  (should-error (org--graph-raw->dot "
+ r
  | ^ | a |" nil) :type 'user-error)
  (should-error (org--graph-raw->dot "
+ r
  |   | a |" nil) :type 'user-error))

(org-deftest "in a record table, completion offers no keys: its rows are cells"
  :mode #'org-graph-edit-mode
  :text "
+ r
  | left | <<mid>> middle | right |
  | sha |"
  :at "sha"
  (should-not (org-graph--completion-at-point)))

(org-deftest "in an item's table of keys, the key half typed is still completed, though it names no attribute yet"
  :mode #'org-graph-edit-mode
  :text "
+ r
  | color | red |
  | sha |"
  :at "sha"
  (should (org-graph--completion-at-point)))

;;; Which tables are records

(define-graph-test "a table is | key | value | rows only when each row is two cells keyed by an attribute or a :key; \
else a record: so a misspelt key shows, and =label= writes an attribute's name as a field"
  :input "
+ a
  | colour | red |
+ b
  | =label= | color |
+ c
  | label | C |
  | :id   | see |"
  :output "digraph {
  a [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\"
CELLSPACING=\"0\"><TR><TD>colour</TD><TD>red</TD></TR></TABLE>> ];
  b [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD><FONT
FACE=\"monospace\">label</FONT></TD><TD>color</TD></TR></TABLE>> ];
  see [ label=C ];
}")

(deftest "a container with a record table is an error: a box of nodes cannot also be a table"
  (should-error (org--graph-raw->dot "
+ Box :container:
  | left | right |
  + x" nil)
                :type 'user-error))

;;; Tags on a cell style its arcs

(define-graph-test "a cell's trailing tags style its arcs, as a reference's style its arc; legends see them as arc templates"
  :input "
#+name: dashed
| style | dashed |
+ struct1
  | left | <<mid>> middle [[struct2::one][calls]] :dashed: | right |
+ struct2
  | <<one>> one | two |
+ Legend :container:
  - :dashed: :: borrows"
  :output "digraph {
  struct1:mid -> struct2:one [ label=calls style=dashed ];
  struct1 [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\"
CELLSPACING=\"0\"><TR><TD>left</TD><TD PORT=\"mid\">middle</TD><TD>right</TD></TR></TABLE>> ];
  struct2 [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD
PORT=\"one\">one</TD><TD>two</TD></TR></TABLE>> ];
  subgraph cluster_Legend {
    label=Legend
    legend_1 [ shape=point style=invis ];
    legend_1_end [ shape=point style=invis ];
    legend_1 -> legend_1_end [ style=dashed label=borrows ];
  }
}")

(define-graph-test "a cell's `:edge[T].KEY' rows still win over its tags"
  :input "
#+name: hot
| color | red |
+ s
  | :edge[t].color | blue |

  | <<p>> [[t]] :hot: |
+ t"
  :output "digraph {
  s:p -> t [ color=blue ];
  s [ shape=plain label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD
PORT=\"p\"></TD></TR></TABLE>> ];
  t [ label=t ];
}")

(deftest "a cell with tags but no link is an error, rather than its tags silently vanishing"
  (should-error (org--graph-raw->dot "
#+name: x
| a | b |
+ s
  | Vim's :x: |" nil)
                :type 'user-error))

(provide 'org-graph-record-nodes-tests)
