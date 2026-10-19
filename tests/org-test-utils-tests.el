;;; org-test-utils-tests.el --- The test utilities themselves; e.g., `exporting'

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(deftest "`exporting' works as intended"
  ;; Basic usage
  (exporting "A B C D" :to ascii :equals "A P C T" :modulo ("P" "T"))

  ;; No pollution of global namespace
  ;; (should-not (fboundp 'org-block/shouting))
  ;; (should-not (fboundp 'org-link/shouting))
  (exporting "shouting:hello" :to ascii :using (org-defblock shouting (&optional wat) "docs" (upcase wat)) :equals "HELLO\n")
  (should-not (fboundp 'org-block/shouting))
  (should-not (fboundp 'org-link/shouting))

  ;; :using may be omitted
  (exporting "shouting:hello" :to ascii :equals "<shouting:hello>\n")

  ;; :using may be a (singleton) list
  (exporting "shouting:hello"
    :to ascii
    :using ((org-defblock shouting (&optional wat) "docs" (upcase wat)))
    :equals "HELLO\n")

  ;; :using may be a multi-element list
  (exporting "shout:hello quiet:WORLD"
    :to ascii
    :using ((org-defblock shout (&optional wat) "docs" (upcase wat))
            (org-defblock quiet (&optional wat) "docs" (downcase wat)))
    :equals "HELLO world\n"))

(deftest "`org-test--fill' breaks a long line at a space, never inside a tag nor a <pre>" [fill]
  (let* ((org-test--width 30)
         (filled (org-test--fill "<p>Some words, enough to go past thirty.</p><pre>keep   these   spaces   as   they are</pre> and after")))
    (should (s-contains-p "<pre>keep   these   spaces   as   they are</pre>" filled))
    (should-not (string-match-p "<\n" filled))
    (should (equal (replace-regexp-in-string "\n" " " filled)
                   "<p>Some words, enough to go past thirty.</p><pre>keep   these   spaces   as   they are</pre> and after"))))
