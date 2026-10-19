;;; org-ctrl-c-ctrl-c-for-special-block-tests.el --- Tests for C-c C-c actions of special blocks  -*- lexical-binding: t; -*-

(unless (fboundp 'using)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(defvar org-def-action-tests--said nil
  "What the actions under test said.")

(org-deftest "org-def-action gives C-c C-c within a block its arguments, by name, and its text as raw-contents" [C-c_C-c]
  :text "
#+begin_shout Hello :volume 11
Mind the gap.
#+end_shout"
  :at "Mind"
  (using (org-defblock shout () "Upcase the contents." (upcase contents))
    (unwind-protect
        (progn
          (org-def-action shout (&optional greeting &key (volume 1) (pitch "low"))
            "Say the shout's GREETING at VOLUME and PITCH."
            (setq org-def-action-tests--said (format "%s at %s, %s: %s" greeting volume pitch raw-contents)))
          (org-ctrl-c-ctrl-c)
          (should (equal org-def-action-tests--said "Hello at 11, low: Mind the gap.")))
      (setf (alist-get "shout" org--block-cc-handlers nil 'remove #'equal) nil))))

(org-deftest "C-c C-c hands a block's action its text with :let's shorthands applied, as export renders it" [C-c_C-c]
  :text "
#+begin_shout :let (gap \"chasm\" -> \"⇒\")
Mind the gap -> or else.
#+end_shout"
  :at "Mind"
  (using (org-defblock shout () "Upcase the contents." (upcase contents))
    (unwind-protect
        (progn
          (org-def-action shout () "Say the shout's text." (setq org-def-action-tests--said raw-contents))
          (org-ctrl-c-ctrl-c)
          (should (equal org-def-action-tests--said "Mind the chasm ⇒ or else.")))
      (setf (alist-get "shout" org--block-cc-handlers nil 'remove #'equal) nil))))

(org-deftest "C-c C-c within a table in a block is Org's own, aligning the table, not the block's action" [C-c_C-c]
  :text "
#+begin_shout
| 1 | 2 |
| 3 |
#+end_shout"
  :at "| 3"
  (using (org-defblock shout () "Upcase the contents." (upcase contents))
    (unwind-protect
        (progn
          (setq org-def-action-tests--said nil)
          (org-def-action shout () "Say the shout's text." (setq org-def-action-tests--said raw-contents))
          (org-ctrl-c-ctrl-c)
          (should-not org-def-action-tests--said)
          (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "| 3 |   |")))
      (setf (alist-get "shout" org--block-cc-handlers nil 'remove #'equal) nil))))

(org-deftest "org-def-action's arguments take their defaults as the block's do: inherited header-args, else declared" [C-c_C-c header-args]
  :text "
* Loud
:PROPERTIES:
:header-args:shout: :volume 9
:END:
#+begin_shout
Mind the gap.
#+end_shout"
  :at "Mind"
  (using (org-defblock shout () "Upcase the contents." (upcase contents))
    (unwind-protect
        (progn
          (org-def-action shout (greeting "Hey" volume 1)
            "Say the shout's GREETING at VOLUME; the flattened arglist serves, as for `org-defblock'."
            (setq org-def-action-tests--said (format "%s at %s" greeting volume)))
          (org-ctrl-c-ctrl-c)
          (should (equal org-def-action-tests--said "Hey at 9")))
      (setf (alist-get "shout" org--block-cc-handlers nil 'remove #'equal) nil))))

(org-deftest "an org-def-action body yielding nil leaves C-c C-c to the block's :C-c_C-c switch" [C-c_C-c]
  :text "
#+begin_shout :C-c_C-c (setq org-def-action-tests--said \"the switch\")
Mind the gap.
#+end_shout"
  :at "Mind"
  (using (org-defblock shout () "Upcase the contents." (upcase contents))
    (unwind-protect
        (progn
          (org-def-action shout () "Decline, by yielding nil." nil)
          (setq org-def-action-tests--said nil)
          (org-ctrl-c-ctrl-c)
          (should (equal org-def-action-tests--said "the switch")))
      (setf (alist-get "shout" org--block-cc-handlers nil 'remove #'equal) nil))))

(deftest "org-def-action defines org-action/NAME, documented by its docstring" [C-c_C-c]
  (unwind-protect
      (progn
        (org-def-action shout () "Say the shout." t)
        (should (fboundp 'org-action/shout))
        (should (string-prefix-p "Say the shout." (documentation 'org-action/shout))))
    (setf (alist-get "shout" org--block-cc-handlers nil 'remove #'equal) nil)
    (fmakunbound 'org-action/shout)))

(provide 'org-ctrl-c-ctrl-c-for-special-block-tests)
;;; org-ctrl-c-ctrl-c-for-special-block-tests.el ends here
