;;; org-graph-read-tests.el --- org--graph-read: a graph's directives, templates, and items  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org--graph-read

(deftest "`org--graph-read' buckets #+graph: table rows, a blank line after them leaving the items be"
  (let* ((p (org--graph-read
             "
#+graph:
| rankdir | TB |
| bgcolor | #000 |

+ A
  + B")))
    (should (equal '(("rankdir" "TB") ("bgcolor" "#000"))
                   (plist-get p :graph-rows)))
    (should (null (plist-get p :node-rows)))
    (should (equal '("A") (--map (s-trim (plist-get it :text)) (plist-get p :items))))))

(deftest "`org--graph-read' buckets #+node: and #+edge: rows"
  (let* ((p (org--graph-read
             "
#+node:
| shape | box |
#+edge:
| color | red |
+ X")))
    (should (equal '(("shape" "box")) (plist-get p :node-rows)))
    (should (equal '(("color" "red")) (plist-get p :edge-rows)))))

(deftest "a #+rank: line is no graph syntax: an item's `:@X:' tag aligns it"
  (should-error (org--graph-raw->dot "
#+rank: A B
+ A
+ B" nil) :type 'user-error))

(deftest "`org--graph-read' parses #+name: tables into name-tables, a #+caption: by the #+name: notwithstanding"
  (let* ((p (org--graph-read
             "
#+name: wife
#+caption: How a wife looks
| fillcolor | \"#f8d7e8\" |
| color | \"#aa3366\" |

+ A")))
    (let ((nt (plist-get p :name-tables)))
      (should (hash-table-p nt))
      (should (equal '(("fillcolor" "\"#f8d7e8\"") ("color" "\"#aa3366\""))
                     (gethash "wife" nt))))))

(deftest "`org--graph-read' keeps #+name: tables out of the items, wherever they sit: atop the graph, in a drawer, or in an item"
  (let* ((p (org--graph-read
             "
#+name: married
| color | red |

:PALETTE:
#+name: faint
| style | dotted |
:END:

+ A
  #+name: bold
  | penwidth | 2 |
  + B")))
    (should (equal '("bold" "faint" "married") (sort (hash-table-keys (plist-get p :name-tables)) #'string<)))
    (should (equal '("A") (--map (s-trim (plist-get it :text)) (plist-get p :items))))
    (should (null (plist-get (plist-get (car (plist-get p :items)) :pre) :attr-rows)))))

(deftest "`org--graph-expand-inherit' splices template rows, explicit rows win"
  (let* ((nt (make-hash-table :test #'equal)))
    (puthash "wife" '(("fillcolor" "\"#f8d7e8\"") ("color" "\"#aa3366\"")) nt)
    (should (equal '(("fillcolor" "\"#f8d7e8\"") ("shape" "box") ("color" "\"#ff0000\""))
                   (org--graph-expand-inherit '(("shape" "box") (":inherit" "wife") ("color" "\"#ff0000\"")) nt)))))

(deftest "`org--graph-expand-inherit' expands templates within templates, but a template including itself adds nothing"
  (let* ((nt (make-hash-table :test #'equal)))
    (puthash "base" '(("shape" "box")) nt)
    (puthash "wife" '((":inherit" "base") ("color" "pink")) nt)
    (puthash "loop" '((":inherit" "loop") ("color" "red")) nt)
    (should (equal '(("shape" "box") ("color" "pink")) (org--graph-expand-inherit '((":inherit" "wife")) nt)))
    (should (equal '(("color" "red")) (org--graph-expand-inherit '((":inherit" "loop")) nt)))))

(deftest "#+name: template expands inside #+node: lines too, by a :T: tag"
  (should (s-contains-p "node [ shape=box ]"
                        (org--graph-raw->dot "
#+name: t
| shape | box |

#+node: :t:
+ A" nil))))

(deftest "an :inherit row is no Graphviz attribute: under #+node: an error, in an item's table a record field"
  (should-error (org--graph-raw->dot "
#+name: t
| shape | box |

#+node:
| :inherit | t |
+ A" nil) :type 'user-error)
  (should (s-contains-p "<TD>:inherit</TD>" (org--graph-raw->dot "
#+name: t
| shape | box |

+ A
  | :inherit | t |" nil))))

(deftest "a graph's template sets only Graphviz attributes, and is never named @X, which aligns nodes"
  (should-error (org--graph-raw->dot "
#+name: wife
| :inherit | base |
+ A :wife:" nil) :type 'user-error)
  (should-error (org--graph-raw->dot "
#+name: @pals color=red
+ A" nil) :type 'user-error))

(deftest "#+name: template expands into node attrs via a :T: tag"
  (let* ((dot (org--graph-raw->dot
               "
#+name: wife
| fillcolor | \"#f8d7e8\" |
| color | \"#aa3366\" |

+ Khadija :wife:"
               nil)))
    (should (s-contains-p "fillcolor=\"#f8d7e8\"" dot))
    (should (s-contains-p "color=\"#aa3366\"" dot))))

(define-graph-test "#+name: template expands into a lone-link child's arc via [[B][:married:]], and explicit :edge. rows win"
  :input "
#+name: married
| color | \"#cc3399\" |
| dir   | both        |

+ A
  + [[B][:married:]]
    | :edge.dir   | back    |
+ B"
  :output "digraph {
  A [ label=A ];
  A -> B [ color=\"#cc3399\" dir=back ];
  B [ label=B ];
}")

(deftest "#+name: explicit node attr overrides template via a :T: tag"
  (let* ((dot (org--graph-raw->dot
               "
#+name: base
| color | blue |
| shape | box |

+ A :base:
  | color | red   |"
               nil)))
    ;; explicit color=red wins over template color=blue
    (should (s-contains-p "color=red" dot))
    (should (not (s-contains-p "color=blue" dot)))
    ;; template shape=box is preserved
    (should (s-contains-p "shape=box" dot))))

(deftest "under `:unknown-tags error', a tag naming no #+name: template is a user error, rather than silently contributing nothing"
  (should-error (org--graph-raw->dot "
#+name: real
| color | blue |

+ A :typo:" nil '(:unknown-tags error)) :type 'user-error))

(deftest "a #+graph:, #+node:, or #+edge: line may hold KEY=VALUE pairs, as Graphviz's own edge [ … ] does, beside tags" [graph]
  (should (equal (org--graph-keyword-rows 'edge "fontsize=10 color=gray") '(("fontsize" "10") ("color" "gray"))))
  (should (equal (org--graph-keyword-rows 'node ":faint: label=\"a b\" style=dashed")
                 '((":inherit" "faint") ("label" "a b") ("style" "dashed"))))
  (should (equal (org--graph-raw->dot "
#+graph: rankdir=LR
#+node: shape=box
| style | rounded |
#+edge: color=gray
+ a
  + b" nil)
                 "digraph {
  graph [ rankdir=LR ]
  node [ shape=box style=rounded ]
  edge [ color=gray ]
  a [ label=a ];
  a -> b;
  b [ label=b ];
}")))

(deftest "a #+edge: line holding anything but tags and KEY=VALUE pairs is an error, naming the stray word" [graph]
  (should (string-match-p "‘#\\+edge: fontsize=10 oops’ holds ‘oops’"
                          (cadr (should-error (org--graph-raw->dot "
#+edge: fontsize=10 oops
+ a" nil) :type 'user-error)))))

(deftest "a #+name: line may be a whole template, of KEY=VALUE pairs; several in a row are several, as tables would be" [graph]
  (should (equal (org--graph-raw->dot "
#+name: red color=red
#+name: blue color=blue style=dashed :red:
#+name: tall height=1
| shape | box |
+ a :red:
+ b :blue:
+ c :tall:" nil)
                 "digraph {
  a [ color=red label=a ];
  b [ color=blue style=dashed label=b ];
  c [ height=1 shape=box label=c ];
}")))

(deftest "a one-line #+name: template outside any graph is every graph's, as a table one is" [graph]
  (should (string-search "d [ color=navy shape=box label=d ];"
                         (with-org-test-buffer (:text "#+name: house color=navy shape=box\n")
                           (org--graph-raw->dot "\n+ d :house:" nil)))))

(deftest "a #+name: line is a template's only when it holds a KEY=VALUE pair; Org's names of other things are left be" [graph]
  (should (equal (org--graph-unaffiliate-names "#+name: fig-1\n#+NAME: red color=red :base:")
                 "#+name: fig-1\n#+template: red color=red :base:")))
