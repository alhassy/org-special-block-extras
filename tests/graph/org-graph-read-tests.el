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

(deftest "`org--graph-read' collects #+rank: lines"
  (let* ((p (org--graph-read "
#+rank: A B
#+rank: C D
+ A")))
    (should (equal '("A B" "C D") (plist-get p :ranks)))))

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
    (should (equal '(("fillcolor" "\"#f8d7e8\"") (":id" "x") ("color" "\"#ff0000\""))
                   (org--graph-expand-inherit '((":id" "x") (":inherit" "wife") ("color" "\"#ff0000\"")) nt)))))

(deftest "`org--graph-expand-inherit' expands templates within templates, but a template including itself adds nothing"
  (let* ((nt (make-hash-table :test #'equal)))
    (puthash "base" '(("shape" "box")) nt)
    (puthash "wife" '((":inherit" "base") ("color" "pink")) nt)
    (puthash "loop" '((":inherit" "loop") ("color" "red")) nt)
    (should (equal '(("shape" "box") ("color" "pink")) (org--graph-expand-inherit '((":inherit" "wife")) nt)))
    (should (equal '(("color" "red")) (org--graph-expand-inherit '((":inherit" "loop")) nt)))))

(deftest "#+name: template expands inside #+node: tables too"
  (should (s-contains-p "node [ shape=box ]"
                        (org--graph-raw->dot "
#+name: t
| shape | box |

#+node:
| :inherit | t |
+ A" nil))))

(deftest "#+name: template expands into node attrs via | :inherit |"
  (let* ((dot (org--graph-raw->dot
               "
#+name: wife
| fillcolor | \"#f8d7e8\" |
| color | \"#aa3366\" |

+ Khadija
  | :inherit | wife |"
               nil)))
    (should (s-contains-p "fillcolor=\"#f8d7e8\"" dot))
    (should (s-contains-p "color=\"#aa3366\"" dot))))

(define-graph-test "#+name: template expands into a lone-link child's arc via | :edge.:inherit |, and explicit :edge. rows win"
  :input "
#+name: married
| color | \"#cc3399\" |
| dir   | both        |

+ A
  + [[B]]
    | :edge.:inherit | married |
    | :edge.dir   | back    |
+ B"
  :output "digraph {
  A [ label=A ];
  A -> B [ color=\"#cc3399\" dir=back ];
  B [ label=B ];
}")

(deftest "#+name: explicit node attr overrides template via | :inherit |"
  (let* ((dot (org--graph-raw->dot
               "
#+name: base
| color | blue |
| shape | box |

+ A
  | :inherit  | base  |
  | color | red   |"
               nil)))
    ;; explicit color=red wins over template color=blue
    (should (s-contains-p "color=red" dot))
    (should (not (s-contains-p "color=blue" dot)))
    ;; template shape=box is preserved
    (should (s-contains-p "shape=box" dot))))

(deftest "an `:inherit' row naming no #+name: template is a user error, rather than silently contributing nothing"
  (should-error (org--graph-raw->dot "
#+name: real
| color | blue |

+ A
  | :inherit | typo |" nil) :type 'user-error))
