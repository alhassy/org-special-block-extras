;;; org-graph-cluster-edit-tests.el --- Nested clusters in Emacs: TAB, backgrounds, eldoc, completion  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-export-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(require 'org-graph-block)

(defconst org-graph-cluster-test--text "
#+begin_graph
#+begin_cluster Islam
#+begin_cluster Christianity
+ Jesus :Prophet:
#+begin_cluster Judaism
+ Moses :Prophet:
#+end_cluster
#+end_cluster
+ Mohammad :Prophet:
#+end_cluster
#+end_graph")

(org-deftest "nested clusters pair each #+begin_cluster with its own #+end_cluster, not Org's first one" [graph cluster]
  :text org-graph-cluster-test--text
  :at "Moses"
  (let ((line (lambda (n) (save-excursion (goto-char (point-min)) (forward-line (1- n)) (point)))))
    (should (equal (org-graph--clusters (org-graph--graph-bounds))
                   (list (list (funcall line 3) (funcall line 11) 0)
                         (list (funcall line 4) (funcall line 9) 1)
                         (list (funcall line 6) (funcall line 8) 2))))))

(org-deftest "TAB on an outer #+begin_cluster folds down to its own #+end_cluster; again, unfolds" [graph cluster]
  :text org-graph-cluster-test--text
  :at "#+begin_cluster Islam"
  (should (org-graph--cycle-cluster))
  (should (invisible-p (save-excursion (search-forward "Mohammad") (point))))
  (should-not (invisible-p (save-excursion (search-forward "#+end_graph") (match-beginning 0))))
  (should (org-graph--cycle-cluster))
  (should-not (invisible-p (save-excursion (search-forward "Mohammad") (point)))))

(org-deftest "TAB on an inner #+end_cluster folds its own cluster, leaving the outer's items visible" [graph cluster]
  :text org-graph-cluster-test--text
  :at "#+end_cluster"
  (should (org-graph--cycle-cluster))
  (should (invisible-p (save-excursion (goto-char (point-min)) (search-forward "Moses"))))
  (should-not (invisible-p (save-excursion (goto-char (point-min)) (search-forward "Jesus")))))

(org-deftest "each cluster wears a background of its own, a nested one differing from its parent's" [graph cluster]
  :text org-graph-cluster-test--text
  :at "#+begin_graph"
  (let ((org-graph-cluster-backgrounds '((light "a" "b" "c") (dark "a" "b" "c"))))
    (should (equal (org-graph--cluster-face 0) '(:background "a" :extend t)))
    (should (equal (org-graph--cluster-face 1) '(:background "b" :extend t)))
    (should (equal (org-graph--cluster-face 3) '(:background "a" :extend t)))
    (goto-char (point-min))
    (should (org-graph--match-cluster (point-max)))
    (should (equal (match-beginning 0) (save-excursion (goto-char (point-min)) (search-forward "#+begin_cluster Islam")
                                                       (line-beginning-position))))
    (should (equal org-graph--cluster-index 0))))

(org-deftest "eldoc on #+begin_cluster says what a cluster is; on a :KEY of its line, what the key does" [graph cluster eldoc]
  :text org-graph-cluster-test--text
  :at "#+begin_cluster Islam"
  (beginning-of-line) (forward-char 3)
  (let (shown)
    (org-graph--eldoc (lambda (doc &rest props) (setq shown (cons doc props))))
    (should (string-prefix-p "A box around the items within" (car shown)))
    (should (equal (plist-get (cdr shown) :thing) "#+begin_cluster TITLE :KEY V …"))
    (end-of-line) (insert " :bgcolor") (backward-char 2)
    (org-graph--eldoc (lambda (doc &rest props) (setq shown (cons doc props))))
    (should (equal (plist-get (cdr shown) :thing) ":bgcolor"))))

(org-deftest "C-M-i on a #+begin_cluster line completes cluster attributes, and :node. and :edge. ones; not :file" [graph cluster]
  :text org-graph-cluster-test--text
  :at "#+begin_cluster Islam"
  (end-of-line) (insert " :")
  (let ((candidates (nth 2 (org-graph--header-completion))))
    (should (member ":bgcolor" candidates))
    (should (member ":node.shape" candidates))
    (should-not (member ":file" candidates))))

(org-deftest "each #+name: template wears a background of its own, its name's line through the table below it" [graph faces]
  :text "
#+begin_graph
#+name: token
| shape | box |
| style | filled |
#+name: rule
| style | bold |
#+name: red color=red

+ expr :rule:
#+end_graph"
  :at "#+begin_graph"
  (let ((org-graph-template-backgrounds '((light "a" "b" "c") (dark "a" "b" "c")))
        (line (lambda (n) (save-excursion (goto-char (point-min)) (forward-line (1- n)) (point)))))
    (should (equal (org-graph--templates (org-graph--graph-bounds))
                   (list (list (funcall line 3) (1- (funcall line 6)) 0)
                         (list (funcall line 6) (1- (funcall line 8)) 1)
                         (list (funcall line 8) (1- (funcall line 9)) 2))))
    (org-graph--setup-org-buffer)
    (font-lock-ensure)
    (let ((background (lambda (needle)
                        (save-excursion (goto-char (point-min)) (search-forward needle)
                                        (--some (and (listp it) (plist-get it :background))
                                                (ensure-list (get-text-property (match-beginning 0) 'face)))))))
      (should (equal (funcall background "| filled") "a"))
      (should (equal (funcall background "| bold") "b"))
      (should (equal (funcall background "red color") "c"))
      (should-not (funcall background "+ expr")))))
