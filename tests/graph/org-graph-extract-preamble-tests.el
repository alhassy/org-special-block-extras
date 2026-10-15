;;; org-graph-extract-preamble-tests.el --- org--graph-extract-preamble  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; org--graph-extract-preamble

(deftest "`org--graph-extract-preamble' buckets #+graph: table rows"
  (let* ((p (org--graph-extract-preamble
             "
#+graph:
| rankdir | TB |
| bgcolor | #000 |

+ A
  + B")))
    (should (equal '(("rankdir" "TB") ("bgcolor" "#000"))
                   (plist-get p :graph-rows)))
    (should (null (plist-get p :node-rows)))
    (should (s-contains-p "+ A" (plist-get p :list-body)))))

(deftest "`org--graph-extract-preamble' buckets #+node: and #+edge: rows"
  (let* ((p (org--graph-extract-preamble
             "
#+node:
| shape | box |
#+edge:
| color | red |
+ X")))
    (should (equal '(("shape" "box")) (plist-get p :node-rows)))
    (should (equal '(("color" "red")) (plist-get p :edge-rows)))))

(deftest "`org--graph-extract-preamble' collects #+rank: lines"
  (let* ((p (org--graph-extract-preamble "
#+rank: A B
#+rank: C D
+ A")))
    (should (equal '("A B" "C D") (plist-get p :ranks)))))

(deftest "`org--graph-extract-preamble' parses #+name: tables into name-tables"
  (let* ((p (org--graph-extract-preamble
             "
#+name: wife
| fillcolor | \"#f8d7e8\" |
| color | \"#aa3366\" |

+ A")))
    (let ((nt (plist-get p :name-tables)))
      (should (hash-table-p nt))
      (should (equal '(("fillcolor" "\"#f8d7e8\"") ("color" "\"#aa3366\""))
                     (gethash "wife" nt))))))

(deftest "`org--graph-extract-preamble' strips #+name: lines from list-body"
  (let* ((p (org--graph-extract-preamble
             "
#+name: married
| color | red |

+ A
  + B")))
    (should (not (s-contains-p "#+name:" (plist-get p :list-body))))
    (should (not (s-contains-p "| color | red |" (plist-get p :list-body))))
    (should (s-contains-p "+ A" (plist-get p :list-body)))))

(deftest "`org--graph-expand-eval' splices template rows, explicit rows win"
  (let* ((nt (make-hash-table :test #'equal)))
    (puthash "wife" '(("fillcolor" "\"#f8d7e8\"") ("color" "\"#aa3366\"")) nt)
    (should (equal '(("fillcolor" "\"#f8d7e8\"") (":id" "x") ("color" "\"#ff0000\""))
                   (org--graph-expand-eval '((":id" "x") (":eval" "wife") ("color" "\"#ff0000\"")) nt)))))

(deftest "`org--graph-expand-eval' expands templates within templates, but a template including itself adds nothing"
  (let* ((nt (make-hash-table :test #'equal)))
    (puthash "base" '(("shape" "box")) nt)
    (puthash "wife" '((":eval" "base") ("color" "pink")) nt)
    (puthash "loop" '((":eval" "loop") ("color" "red")) nt)
    (should (equal '(("shape" "box") ("color" "pink")) (org--graph-expand-eval '((":eval" "wife")) nt)))
    (should (equal '(("color" "red")) (org--graph-expand-eval '((":eval" "loop")) nt)))))

(deftest "#+name: template expands inside #+node: tables too"
  (should (s-contains-p "node [ shape=box ]"
                        (org--graph-raw->dot "
#+name: t
| shape | box |

#+node:
| :eval | t |
+ A" "directed"))))

(deftest "#+name: template expands into node attrs via | :eval |"
  (let* ((dot (org--graph-raw->dot
               "
#+name: wife
| fillcolor | \"#f8d7e8\" |
| color | \"#aa3366\" |

+ Khadija
  | :eval | wife |"
               "directed")))
    (should (s-contains-p "fillcolor=\"#f8d7e8\"" dot))
    (should (s-contains-p "color=\"#aa3366\"" dot))))

(define-graph-test "#+name: template expands into to-edge attrs via | :to.:eval |, and explicit :to. rows win"
  :input "
#+name: married
| color | \"#cc3399\" |
| dir   | both        |

+ A
  | :to       | B       |
  | :to.:eval | married |
  | :to.dir   | back    |
+ B"
  :output "digraph {
  A -> B [ color=\"#cc3399\" dir=back ];
  A [ label=A ];
  B [ label=B ];
}")

(deftest "#+name: explicit node attr overrides template via | :eval |"
  (let* ((dot (org--graph-raw->dot
               "
#+name: base
| color | blue |
| shape | box |

+ A
  | :eval  | base  |
  | color | red   |"
               "directed")))
    ;; explicit color=red wins over template color=blue
    (should (s-contains-p "color=red" dot))
    (should (not (s-contains-p "color=blue" dot)))
    ;; template shape=box is preserved
    (should (s-contains-p "shape=box" dot))))

(deftest "#+name: unknown template name silently produces empty expansion"
  (let* ((dot (org--graph-raw->dot
               "
#+name: real
| color | blue |

+ A
  | :eval | typo |"
               "directed")))
    ;; node still renders, just without template attrs
    (should (s-contains-p "A [ label=A ];" dot))))
