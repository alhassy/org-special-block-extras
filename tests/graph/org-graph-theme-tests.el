;;; org-graph-theme-tests.el --- Themes: org-graph-themes, org-graph-theme, and :theme  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

(deftest "every graph wears org-graph-theme, beneath its own attributes; :theme nil wears Graphviz's look" [graph theme]
  (let ((org-graph-theme 'creamsicle))
    (let ((dot (org--graph-raw->dot "
#+node: color=red
+ A" nil nil)))
      (should (string-match-p "node \\[ shape=box style=filled fillcolor=\"#FFE0C7\" .* color=red \\]" dot))
      (should (string-match-p "edge \\[ color=\"#311602\"" dot)))
    (should-not (string-match-p "FFE0C7" (org--graph-raw->dot "+ A" nil '(:theme nil))))))

(deftest "a :theme names one of org-graph-themes; an unknown one is a user error" [graph theme]
  (let ((org-graph-themes '((ink :node "shape=box" :edge "color=gray30"))))
    (should (string-match-p "node \\[ shape=box \\]" (org--graph-raw->dot "+ A" nil '(:theme ink))))
    (should-error (org--graph-raw->dot "+ A" nil '(:theme nope)) :type 'user-error)))

(deftest "a theme's :container-colors fill containers by depth, cycling; a container's own fillcolor wins" [graph theme]
  (let* ((org-graph-themes '((nest :container-colors "red blue")))
         (dot (org--graph-raw->dot "
+ Outer :container:
  + Inner :container:
    + Innermost :container:
      + a
+ Own :container:
  | fillcolor | gold |
  + b" nil '(:theme nest))))
    (should (string-match-p "label=Outer\n *fillcolor=red" dot))
    (should (string-match-p "label=Inner\n *fillcolor=blue" dot))
    (should (string-match-p "label=Innermost\n *fillcolor=red" dot))
    (should (string-match-p "label=Own\n *fillcolor=gold" dot))))

(provide 'org-graph-theme-tests)
;;; org-graph-theme-tests.el ends here
