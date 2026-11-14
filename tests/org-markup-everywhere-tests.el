;;; org-markup-everywhere-tests.el --- Every block honours our links, Org's links, targets, and maths  -*- lexical-binding: t; -*-

;; A block's contents, its title, a graph's labels, and a proof tree's maths all read the same markup: our red:x and
;; [[tooltip:x][tip]], Org's [[https://…]], [[file:…]], [[*Heading]], [[target]], and <<target>>, and $…$ maths.

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(defconst org-markup-tests--prose
  "red:fire, $\\alpha$, [[tooltip:x][a tip]], [[https://orgmode.org][Org]], [[file:notes.org][notes]], [[*Folds][the folds]], and [[Lemma][the lemma]]"
  "A sentence holding each markup a block's prose may hold, pointing at the section and target `org-markup-tests--export' declares.")

(defun org-markup-tests--export (org backend)
  "Returns the body-only export of ORG along BACKEND, beside a <<Lemma>> target and a Folds section for its links to reach."
  (with-temp-buffer
    (insert org "\n\n<<Lemma>> A lemma.\n\n* Folds\n")
    (let ((org-inhibit-startup t))
      (org-mode)
      (org-special-block-extras-mode)
      (org-export-as backend nil nil t '(:with-toc nil :section-numbers nil)))))

(defun org-markup-tests--rendered-p (out backend)
  "Checks that OUT, a BACKEND export of `org-markup-tests--prose', renders each of its markups, leaving no Org syntax."
  (dolist (raw '("red:fire" "[[" "osbe-link:" "*Folds"))
    (should-not (string-search raw out)))
  (dolist (rendered (pcase backend
                      ('html '("<span style=\"color:red;\">fire</span>" "\\(\\alpha\\)" "class=\"tooltip\""
                               "<a href=\"https://orgmode.org\">Org</a>" "<a href=\"notes.html\">notes</a>"
                               "the folds</a>" "<a href=\"#Lemma\">the lemma</a>"))
                      ('latex '("\\color{red}fire" "\\(\\alpha\\)" "\\marginpar" "\\href{https://orgmode.org}{Org}"
                                "\\href{notes.org}{notes}" "{the folds}" "{the lemma}"))))
    (should (string-search rendered out)))
  ;; The section link reaches the section's own anchor.
  (should (string-match-p (if (eq backend 'html) "href=\"#\\([^\"]+\\)\">the folds</a>" "\\\\hyperref\\[sec:[^]]+\\]{the folds}")
                          out)))

(defun org-markup-tests--check (begin body)
  "Checks that a block opened by #+begin_BEGIN, holding BODY, renders `org-markup-tests--prose' in HTML and LaTeX."
  (dolist (backend '(html latex))
    (ert-info ((format "#+begin_%s, exported to %s" begin backend))
      (org-markup-tests--rendered-p
       (org-markup-tests--export (format "
#+begin_%s
%s
#+end_%s" begin body (car (split-string begin))) backend)
       backend))))

(defconst org-markup-tests--own-syntax '(graph tree math documentation)
  "Blocks whose contents are no prose ---a graph, maths, a glossary---; each has its own test.")

(deftest "every block's contents render our links, Org's links, its targets, and maths, as prose does"
  ;; A quote, and a colour block, show their contents only given a citation key, or a colour.
  (dolist (begin (--map (pcase it ('quote "quote Knuth") ('color "color teal") (_ (symbol-name it)))
                        (--remove (memq it org-markup-tests--own-syntax) org--supported-blocks)))
    (org-markup-tests--check begin (format "See %s." org-markup-tests--prose))))

(deftest "a block's title renders our links, Org's links, and maths, as its contents do; quoted, or not"
  ;; Unquoted, a [[link][with words]] is one word of the header, not Lisp's vector.
  (dolist (header (list (format "%S" org-markup-tests--prose) org-markup-tests--prose))
    (dolist (block '(box details note remark))
      (org-markup-tests--check (format "%s %s" block header) "Body."))))

(deftest "a title's <<target>> joins the document, so a [[target]] elsewhere reaches the block"
  (dolist (backend '(html latex))
    (let ((out (org-markup-tests--export "
#+begin_box \"<<Zorn>> Zorn's lemma\"
Every chain has an upper bound.
#+end_box

By [[Zorn][Zorn's lemma]], …" backend)))
      (should-not (string-search "<<Zorn>>" out))
      (should-not (string-search "osbe-link:" out))
      (should (string-match-p (if (eq backend 'html) "id=\"Zorn\"" "\\\\label{") out))
      (should (string-match-p (if (eq backend 'html) "<a href=\"#Zorn\">Zorn's lemma</a>" "\\\\hyperref\\[[^]]+\\]{Zorn's lemma}")
                              out)))))

(deftest "a block's contents' <<target>> is reached by a [[target]] elsewhere, as in prose"
  (let ((out (org-markup-tests--export "
#+begin_details Proof
<<qed>> Hence, the claim.
#+end_details

See [[qed][the conclusion]]." 'html)))
    (should (string-search "id=\"qed\"" out))
    (should (string-search "<a href=\"#qed\">the conclusion</a>" out))))

(deftest "a title of mere words is left be, while one with Org's markup is escaped as the backend needs"
  (should (equal (org--export-inline "Details" 'latex) "Details"))
  (should (equal (org--export-inline 2 'html) 2))
  (should (equal (org--export-inline "50% & *more*" 'latex) "50\\% \\& \\textbf{more}"))
  (should (equal (org--export-inline "a < *b*" 'html) "a &lt; <b>b</b>")))

(deftest "a header's [[link][with words]] is one word, a string, rather than Lisp's vector"
  (should (equal (org--read-header "[[*Folds][the folds]] :n 2") '("[[*Folds][the folds]]" :n 2)))
  (should (equal (org--read-header "[[https://orgmode.org][Org]], too") '("[[https://orgmode.org][Org]]," too))))

(deftest "a proof tree's lines are maths, as a math block's: red:x colours, and [[tooltip:x][tip]] hovers"
  (let ((out (org-markup-tests--export "
#+begin_tree
+ [[tooltip:MP][modus ponens]] :: red:q
  - p
  - p \\to q
#+end_tree" 'html)))
    (should (string-search "{\\color{red}q}" out))
    (should (string-match-p "\\\\class{tooltip osbe-tip-[^}]+}{MP}" out))
    (should (string-search "modus ponens</p></template>" out))))

(deftest "in maths, margin: is tooltip:, as the margin block is the tooltip block, and a web link leads there"
  (let ((org--math-tips nil))
    (should (equal (org--math-markup "[[margin:x][a tip]]" 'latex)
                   (let ((org--math-tips nil)) (org--math-markup "[[tooltip:x][a tip]]" 'latex))))
    (should (equal (org--math-markup "[[https://orgmode.org][\\oplus]]" 'html) "\\href{https://orgmode.org}{\\oplus}"))))

(provide 'org-markup-everywhere-tests)
;;; org-markup-everywhere-tests.el ends here
