;;; org-graph-extract-preamble-tests.el --- org--graph-extract-preamble  -*- lexical-binding: t; -*-

(unless (fboundp 'define-graph-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/graph/org-graph-test-utils.el")))

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
