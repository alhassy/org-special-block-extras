;;; org-graph-attributes-defcustom-tests.el --- org-graph-attributes defcustom  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

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
