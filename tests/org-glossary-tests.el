;;; org-glossary-tests.el --- The doc: link's glossary: entries as blocks or headings, gathered per export  -*- lexical-binding: t; -*-

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "`org--docs-entries' labels an entry by its labels and its snake_case name, sans junk labels"
  (should (equal (org--docs-entries "Category Theory" nil " Typed composition. ")
                 '(("category_theory" "Category Theory" "Typed composition.")
                   ("Category_Theory" "Category Theory" "Typed composition."))))
  ;; A label list may be quoted, as in the documentation block's docstring.
  (should (equal (mapcar #'car (org--docs-entries "Cat" ''(cat cats) "x")) '("cat" "Cat" "cats"))))

(deftest "headings tagged :documentation: are glossary entries, as are documentation blocks"
  (with-temp-buffer
    (org-mode)
    (insert "* Category Theory :documentation:\n:PROPERTIES:\n:DOC_KEYS: cat cats\n:END:\nTyped composition.\n** A subheading, not part of the entry\n"
            "* Prose\n#+begin_documentation Monoid :label mon\nA unital semigroup.\n#+end_documentation\n")
    (let ((entries (org--docs-read-buffer)))
      (should (equal (assoc "cats" entries) '("cats" "Category Theory" "Typed composition.")))
      (should (equal (assoc "category_theory" entries) '("category_theory" "Category Theory" "Typed composition.")))
      (should (equal (assoc "mon" entries) '("mon" "Monoid" "A unital semigroup."))))))

(deftest "a doc: link cites a heading entry, which exports as an ordinary section"
  (let ((html (org-export-string "* Category Theory :documentation:\n:PROPERTIES:\n:DOC_KEYS: cat\n:END:\nTyped composition.\n* Use\nSee doc:cat.")))
    (should (s-contains? "title=\"Typed composition.\">Category Theory</abbr>" html))
    (should (s-contains? "Category Theory" html))))

(deftest "show:GLOSSARY lists every citation in one export, even when it precedes them"
  (let ((html (org-export-string "show:GLOSSARY\n\n#+begin_documentation Monoid :label mon\nA unital semigroup.\n#+end_documentation\n\nSee doc:mon.")))
    ;; Once as the citation, once as the glossary entry.
    (should (= 2 (s-count-matches "title=\"A unital semigroup.\"" html)))))

(deftest "one export's glossary does not leak into the next"
  (org-export-string "#+begin_documentation Monoid :label mon\nA unital semigroup.\n#+end_documentation\nSee doc:mon.")
  (should-not (s-contains? "unital" (org-export-string "Nothing cited here.\n\nshow:GLOSSARY"))))

(deftest "`org-docs-load-libraries' reads a library's blocks and headings, without exporting it"
  (let ((lib (make-temp-file "osbe-glossary" nil ".org"
                             "* Functor :documentation:\nA structure-preserving map.\n* Other\n#+begin_documentation Monoid :label mon\nA unital semigroup.\n#+end_documentation\n"))
        (org--docs-from-libraries nil))
    (unwind-protect
        (cl-letf (((symbol-function 'org-export-as) (lambda (&rest _) (error "Libraries should be read, not exported"))))
          (org-docs-load-libraries (list lib))
          (should (equal (assoc "functor" org--docs-from-libraries) '("functor" "Functor" "A structure-preserving map.")))
          (should (equal (assoc "mon" org--docs-from-libraries) '("mon" "Monoid" "A unital semigroup.")))
          ;; Reloading replaces, rather than duplicates.
          (org-docs-load-libraries (list lib))
          (should (= 1 (--count (equal (car it) "mon") org--docs-from-libraries))))
      (delete-file lib))))
