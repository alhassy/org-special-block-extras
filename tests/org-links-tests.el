;;; org-links-tests.el --- Tests for built-in link types  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "melpa link produces an <a> pointing to melpa.org."
  :call (org-link/melpa "some-pkg" nil 'html)
  :returns "<a href=\"https://melpa.org/#/some-pkg\"><img alt=\"MELPA\"
src=\"https://img.shields.io/badge/some--pkg-Melpa-green?logo=Gnu-Emacs\"></img></a>")

(define-org-call-test "kbd link wraps the label in <kbd> for HTML."
  :call (org-link/kbd "C-x C-s" nil 'html)
  :returns "<abbr class=\"tooltip\" tabindex=\"0\" style=\"border: none; text-decoration: none;\"><kbd
style=\"border-color: red\">C-x C-s</kbd><template><pre>C-x C-s ∷ save-buffer
Save current buffer in visited file if modified.
Variations are described below.

By default, makes the previous version into a backup file
 if previously requested or if this is the first save.
Prefixed with one C-u, marks this version
 to become a backup when the next save is done.
Prefixed with two C-u’s,
 makes the previous version into a backup file.
Prefixed with three C-u’s, marks this version
 to become a backup when the next save is done,
 and makes the previous version into a backup file.

With a numeric prefix argument of 0, never make the previous version
into a backup file.

Note that the various variables that control backups, such
as ‘version-control’, ‘backup-enable-predicate’, ‘vc-make-backup-files’,
and ‘backup-inhibited’, to name just the more popular ones, still
control whether a backup will actually be produced, even when you
invoke this command prefixed with two or three C-u’s.

If a file’s name is FOO, the names of its numbered backup versions are
 FOO.~i~ for various integers i.  A non-numbered backup file is called FOO~.
Numeric backups (rather than FOO~) will be made if value of
 ‘version-control’ is not the atom ‘never’ and either there are already
 numeric versions of the file being backed up, or ‘version-control’ is
 non-nil.
We don’t want excessive versions piling up, so there are variables
 ‘kept-old-versions’, which tells Emacs how many oldest versions to keep,
 and ‘kept-new-versions’, which tells how many newest versions to keep.
 Defaults are 2 old versions and 2 new.
‘dired-kept-versions’ controls Dired’s clean-directory (.) command.
If ‘delete-old-versions’ is nil, system will query user
 before trimming versions.  Otherwise it does it silently.

If ‘vc-make-backup-files’ is nil, which is the default,
 no backup files are made for files managed by version control.
 (This is because the version control system itself records previous versions.)

See the subroutine ‘basic-save-buffer’ for more information.

(fn &amp;optional ARG)</pre></template></abbr>")

(define-org-call-test "kbd link uses \\texttt for LaTeX."
  :call (org-link/kbd "C-x_C-s" nil 'latex)
  :returns "\\texttt{C-x C-s}")

(define-org-call-test "badge link produces a shields.io img tag for HTML."
  :call (org-link/badge "key|value|blue" nil 'html)
  :returns "<img src=\"https://img.shields.io/badge/key-value-blue\">")

(define-org-call-test "badge link returns empty string for LaTeX."
  :call (org-link/badge "key|value|blue" nil 'latex)
  :returns "")

(deftest "doc link wraps the label in an <abbr> tooltip for HTML."
  (let ((org--docs nil)
        (org--docs-from-libraries nil)
        (org--docs-actually-used nil))
    (org-docs-set "testword" "Test Word" "A made-up glossary entry.")
    (let ((result (org-link/doc "testword" nil 'html)))
      (should (string-match-p "<abbr" result))
      (should (string-match-p "Test Word" result)))))

(define-org-call-test "show link evaluates and returns the expression's value."
  :call (org-link/show "(+ 1 2)" nil 'html)
  :returns "3
")

;;; Within Emacs, and the LaTeX glossary

(org-deftest "the badge link's tooltip names each part of its label"
  :text "See badge:key|value|blue here."
  (let ((tooltip (funcall (org-link-get-parameter "badge" :help-echo) nil nil 7)))
    (should (s-contains-p "Key    : key" tooltip))
    (should (s-contains-p "Colour : blue" tooltip))))

(define-org-call-test "the link-here link's tooltip names its anchor"
  :call (with-org-test-buffer (:text "Go link-here:top now.")
          (funcall (org-link-get-parameter "link-here" :help-echo) nil nil 6))
  :returns "This is a local anchor link named “top”")

(deftest "a LaTeX glossary entry refers back to where its doc link declared it"
  (let ((org--docs '(("cat" "Category Theory" "Typed composition.")))
        (org--docs-from-libraries nil)
        (org--docs-actually-used nil))
    (let ((citation (org-link/doc "cat" nil 'latex))
          (glossary (org-link/show "GLOSSARY" nil 'latex)))
      (should (s-contains-p "\\label{o-glossary-declaration-site-cat}" citation))
      (should (s-contains-p "\\pageref{o-glossary-declaration-site-cat}" glossary))
      (should (s-contains-p "\\hyperref[o-glossary-cat]" citation))
      (should (s-contains-p "\\label{o-glossary-cat}" glossary)))))

;;; kbd: links, as displayed in Emacs

(defun org-links-tests--displayed ()
  "The current buffer's text as Emacs displays it: each string-valued `display' property in place of the text it covers."
  (let ((shown "") (pos (point-min)))
    (while (< pos (point-max))
      (let ((display (get-text-property pos 'display))
            (next (next-single-property-change pos 'display nil (point-max))))
        (setq shown (concat shown (if (stringp display) display (buffer-substring-no-properties pos next)))
              pos next)))
    shown))

(org-deftest "kbd: links, in every format, display in Emacs as 〔KEYS〕, sans markup and underscores" [kbd]
  :text "Save with kbd:C-x_C-s, visit with [[kbd:C-x C-f]], run with <kbd: C-c C-c>, learn with [[kbd:C-h t][the tutorial]]."
  (font-lock-ensure)
  (should (equal (org-links-tests--displayed)
                 "Save with 〔C-x C-s〕, visit with 〔C-x C-f〕, run with 〔C-c C-c〕, learn with 〔C-h t〕.")))

(org-deftest "a kbd: link edited into plain text loses its 〔KEYS〕 display" [kbd]
  :text "
Save with kbd:C-x_C-s, often."
  (org-special-block-extras-mode 1)
  (font-lock-ensure)
  (search-forward "kbd:C-x_C-s")
  (replace-match "the menu")
  (font-lock-ensure)
  (should (equal (org-links-tests--displayed) "
Save with the menu, often.")))

(deftest "clicking a kbd: link runs its keys, once confirmed" [kbd]
  (let (ran asked)
    (cl-letf (((symbol-function 'execute-kbd-macro) (lambda (keys) (setq ran keys))))
      (let ((org-link-elisp-confirm-function (lambda (prompt) (setq asked prompt) t)))
        (funcall (org-link-get-parameter "kbd" :follow) "C-x_3" nil))
      (should (equal asked "Run the keys “C-x 3”? "))
      (should (equal ran (read-kbd-macro "C-x 3")))
      ;; Declined, nothing runs.
      (setq ran nil)
      (let ((org-link-elisp-confirm-function (lambda (_) nil)))
        (funcall (org-link-get-parameter "kbd" :follow) "C-x_3" nil))
      (should-not ran))))

(deftest "undoing our display strings leaves foreign display properties be, such as an aligned table's" [kbd]
  (with-temp-buffer
    (insert "| a | b |")
    (put-text-property 1 2 'display '(space :relative-width 1))
    (org--undisplay (point-min) (point-max))
    (should (equal (get-text-property 1 'display) '(space :relative-width 1)))))

;;; package-docs

(deftest "`org--package-docs' lists a feature's public definitions, alphabetically, with their kinds and gists" [package-docs]
  (should (equal (assoc 'org-callouts (org--package-docs 'org-callout-blocks))
                 '(org-callouts "variable" "The callout blocks, as (NAME COLOUR ICON PURPOSE) entries; the colours and purposes are GitHub's.")))
  (should (equal (assoc 'org-deflink (org--package-docs 'org-deflink))
                 '(org-deflink "macro" "Make a new Org-link NAME that exports using form BODY.")))
  (should (equal (assoc 'org-this-link-next (org--package-docs 'org-deflink))
                 '(org-this-link-next "command" "Go to the next Org link that is similar to the link at point.")))
  ;; Private names, those with “--”, are omitted.
  (should-not (assoc 'org--callout (org--package-docs 'org-callout-blocks)))
  (let ((names (mapcar #'car (org--package-docs 'org-deflink))))
    (should (equal names (sort (copy-sequence names) #'string<)))))

(deftest "a package-docs block exports a table whose names are doc: links to their documentation" [package-docs]
  (let ((html (org-export-string "
#+begin_package-docs org-callout-blocks
#+end_package-docs")))
    (should (s-contains? "<th scope=\"col\" class=\"org-left\">Name</th>" html))
    (should (string-match-p "<td class=\"org-left\"><abbr class=\"tooltip\" tabindex=\"0\"><code>org-callouts</code><template><pre>The callout blocks,[^<]*</pre></template></abbr></td>
<td class=\"org-left\">variable</td>" html))))

(deftest "the tooltips' CSS and JS are inlined in the HTML head, so a page fetches nothing for them; issue #53" [tooltip]
  (should (s-contains-p ".tooltip-box" org--tooltip-html-head))
  (should (s-contains-p "addEventListener('click'" org--tooltip-html-head))
  (should-not (string-match-p "<script[^>]* src=\\|<link " org--tooltip-html-head)))

(deftest "a page's head holds the tooltips' CSS and JS once, however many tooltips, and exports leave org-html-head-extra be; issue #45" [tooltip]
  (let ((head-extra org-html-head-extra)
        (page (lambda () (org-export-string-as "
#+title: Tooltips
Press kbd:C-x_C-s, then [[tooltip:A hint][this]], then [[tooltip:Another][that]]." 'html))))
    (funcall page)
    (let ((html (funcall page)))
      (should (= 1 (s-count-matches "<script" html)))
      (should (= 1 (s-count-matches (regexp-quote ".tooltip-box {") html))))
    (should (equal head-extra org-html-head-extra))))

(provide 'org-links-tests)
;;; org-links-tests.el ends here
