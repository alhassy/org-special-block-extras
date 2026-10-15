;;; org-graph-image-map-tests.el --- A rendered graph is clickable: hrefs, image maps, following, section anchors  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

;;; Rendering for the buffer: each node and cluster sans URL gets an href to its item's line

(deftest "rendering for the buffer gives each node and cluster an href to its body line; a node with a URL keeps it"
  (should (equal "digraph {
  Lisp [ label=Lisp href=\"org-graph:2\" ];
  Lisp -> Scheme;
  Scheme [ label=Scheme href=\"org-graph:3\" ];
  Lisp -> \"Common Lisp\";
  \"Common Lisp\" [ URL=\"https://common-lisp.net\" label=\"Common Lisp\" ];
  subgraph cluster_JVM {
    label=JVM
    href=\"org-graph:5\"
    Clojure [ label=Clojure href=\"org-graph:6\" ];
  }
}"
                 (let ((org--graph-hrefs t))
                   (org--graph-raw->dot "
+ Lisp
  + Scheme
  + [[https://common-lisp.net][Common Lisp]]
#+begin_cluster JVM
+ Clojure
#+end_cluster" nil)))))

(deftest "exporting, unlike rendering for the buffer, adds no hrefs"
  (should-not (s-contains-p "href" (org--graph-raw->dot "
+ Lisp" nil))))

;;; Graphviz's cmapx ⟶ an Emacs image map

(deftest "a cmapx's areas become :map entries, scaled, each with its title as help-echo and its href"
  (should (equal '(((poly . [2 4 6 8 10 12]) org-graph-area (help-echo "McCarthy\n1958" pointer hand org-graph-href "org-graph:2"))
                   ((rect (0 . 2) 20 . 22) org-graph-area (help-echo "JVM" pointer hand org-graph-href "*Java & co"))
                   ((circle (4 . 6) . 2) org-graph-area (help-echo nil pointer hand org-graph-href "x")))
                 (org-graph--cmapx->map "<map id=\"%1\" name=\"%1\">
<area shape=\"poly\" id=\"node1\" href=\"org-graph:2\" title=\"McCarthy\\n1958\" alt=\"\" coords=\"1,2,3,4,5,6\"/>
<area shape=\"rect\" id=\"clust1\" href=\"*Java &amp; co\" title=\"JVM\" alt=\"\" coords=\"0,1,10,11\"/>
<area shape=\"circle\" id=\"node2\" href=\"x\" coords=\"2,3,1\"/>
</map>" 2))))

(deftest "a click finds the node under it before the cluster around it, and nothing outside both"
  (let ((map '(((poly . [10 10 20 10 20 20 10 20]) org-graph-area (org-graph-href "node"))
               ((rect (0 . 0) 30 . 30) org-graph-area (org-graph-href "cluster")))))
    (should (equal "node" (plist-get (nth 2 (org-graph--area-at map 15 15)) 'org-graph-href)))
    (should (equal "cluster" (plist-get (nth 2 (org-graph--area-at map 25 5)) 'org-graph-href)))
    (should-not (org-graph--area-at map 40 40))))

;;; Following a click

(defun tests-graph--click (href)
  "A mouse-1 event on an image whose one area, everywhere, has HREF; its origin is the body's first line."
  (let ((origin (save-excursion (goto-char (point-min)) (search-forward "#+begin_graph") (org-graph--body-origin))))
    `(mouse-1 (,(selected-window) org-graph-area (0 . 0) 0 nil 1 (0 . 0)
               (image :type png :map (((rect (0 . 0) 100 . 100) org-graph-area (org-graph-href ,href)))
                      :org-graph-origin ,origin)
               (5 . 5) (100 . 100)))))

(org-deftest "a click on a node leads to its item in the #+begin_graph block"
  :text "
#+begin_graph
+ Lisp
  + Scheme
#+end_graph"
  :at "#+end_graph"
  (set-window-buffer (selected-window) (current-buffer))
  (org-graph-follow-area (tests-graph--click "org-graph:2"))
  (should (looking-at-p "\\+ Scheme")))

(org-deftest "a click on a node with a [[*Heading]] link leads to that section"
  :text "
#+begin_graph
+ [[*Closures][Closure]]
#+end_graph

* Closures
Functions with their environment."
  :at "#+end_graph"
  (set-window-buffer (selected-window) (current-buffer))
  (org-graph-follow-area (tests-graph--click "*Closures"))
  (should (looking-at-p "\\* Closures")))

;;; HTML export: a section link reaches that section's anchor

(deftest "HTML export resolves a node's [[*Heading]] href to that section's anchor, and leaves an unknown one be"
  (let ((html (org-export-string-as "Intro.
@@html:<a xlink:href=\"*Closures\">c</a> <a xlink:href=\"*Nowhere\">n</a>@@
* Closures
Text." 'html t)))
    (should (string-match "<h2 id=\"\\([^\"]+\\)\"" html))
    (should (s-contains-p (format "xlink:href=\"#%s\"" (match-string 1 html)) html))
    (should (s-contains-p "xlink:href=\"*Nowhere\"" html))))

(deftest "non-HTML exports leave section hrefs be"
  (should (equal "href=\"*X\"" (org-graph--resolve-section-hrefs "href=\"*X\"" 'latex nil))))

(provide 'org-graph-image-map-tests)
;;; org-graph-image-map-tests.el ends here
