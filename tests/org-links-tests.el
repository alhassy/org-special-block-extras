;;; org-links-tests.el --- Tests for built-in link types  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "melpa link produces an <a> pointing to melpa.org."
  :call (org-link/melpa "some-pkg" nil 'html)
  :returns "<a href=\"https://melpa.org/#/some-pkg\"><img alt=\"MELPA\" src=\"https://img.shields.io/badge/some--pkg-Melpa-green?logo=Gnu-Emacs\"></img></a>")

(define-org-call-test "kbd link wraps the label in <kbd> for HTML."
  :call (org-link/kbd "C-x C-s" nil 'html)
  :returns #("<abbr class=\"tooltip\" style=\"border: none; text-decoration: none;\" title=\"C-x C-s ∷ save-buffer<br>Save current buffer in visited file if modified.<br>Variations are described below.<br><br>By default, makes the previous version into a backup file<br> if previously requested or if this is the first save.<br>Prefixed with one C-u, marks this version<br> to become a backup when the next save is done.<br>Prefixed with two C-u’s,<br> makes the previous version into a backup file.<br>Prefixed with three C-u’s, marks this version<br> to become a backup when the next save is done,<br> and makes the previous version into a backup file.<br><br>With a numeric prefix argument of 0, never make the previous version<br>into a backup file.<br><br>Note that the various variables that control backups, such<br>as ‘version-control’, ‘backup-enable-predicate’, ‘vc-make-backup-files’,<br>and ‘backup-inhibited’, to name just the more popular ones, still<br>control whether a backup will actually be produced, even when you<br>invoke this command prefixed with two or three C-u’s.<br><br>If a file’s name is FOO, the names of its numbered backup versions are<br> FOO.<code>i</code> for various integers i.&emsp;A non-numbered backup file is called FOO<code>.<br>Numeric backups (rather than FOO</code>) will be made if value of<br> ‘version-control’ is not the atom ‘never’ and either there are already<br> numeric versions of the file being backed up, or ‘version-control’ is<br> non-nil.<br>We don’t want excessive versions piling up, so there are variables<br> ‘kept-old-versions’, which tells Emacs how many oldest versions to keep,<br> and ‘kept-new-versions’, which tells how many newest versions to keep.<br> Defaults are 2 old versions and 2 new.<br>‘dired-kept-versions’ controls Dired’s clean-directory (.) command.<br>If ‘delete-old-versions’ is nil, system will query user<br> before trimming versions.&emsp;Otherwise it does it silently.<br><br>If ‘vc-make-backup-files’ is nil, which is the default,<br> no backup files are made for files managed by version control.<br> (This is because the version control system itself records previous versions.)<br><br>See the subroutine ‘basic-save-buffer’ for more information.<br><br>(fn &optional ARG)\"><kbd style=\"border-color: red\">C-x C-s</kbd></abbr>" 327 330 (face help-key-binding font-lock-face help-key-binding) 423 426 (face help-key-binding font-lock-face help-key-binding) 504 507 (face help-key-binding font-lock-face help-key-binding) 1065 1068 (face help-key-binding font-lock-face help-key-binding)))

(define-org-call-test "kbd link uses \\texttt for LaTeX."
  :call (org-link/kbd "C-x_C-s" nil 'latex)
  :returns "\\texttt{C-x C-s}")

(define-org-call-test "badge link produces a shields.io img tag for HTML."
  :call (org-link/badge "key|value|blue" nil 'html)
  :returns "<img src=\"https://img.shields.io/badge/key-value-blue?logo=nil\">")

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

(define-org-call-test "org-ospe-html-export-preserving-whitespace replaces newlines with <br>."
  :call (org-ospe-html-export-preserving-whitespace "line1\nline2")
  :returns "line1<br>line2")

(define-org-call-test "show link evaluates and returns the expression's value."
  :call (org-link/show "(+ 1 2)" nil 'html)
  :returns "3
")

;;; Within Emacs, and the LaTeX glossary

(deftest "the badge link's tooltip names each part of its label"
  (with-temp-buffer
    (org-mode)
    (insert "See badge:key|value|blue here.")
    (let ((tooltip (funcall (org-link-get-parameter "badge" :help-echo) nil nil 7)))
      (should (s-contains-p "Key    : key" tooltip))
      (should (s-contains-p "Colour : blue" tooltip)))))

(define-org-call-test "the link-here link's tooltip names its anchor"
  :call (with-temp-buffer
          (org-mode)
          (insert "Go link-here:top now.")
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

(provide 'org-links-tests)
;;; org-links-tests.el ends here
