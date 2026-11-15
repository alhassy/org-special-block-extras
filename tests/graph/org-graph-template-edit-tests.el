;;; org-graph-template-edit-tests.el --- Templates in Emacs: each wears a background of its own  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-export-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))
(require 'org-graph-block)

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
      (should (equal (funcall background "color=red") "c"))
      (should-not (funcall background "+ expr")))))
