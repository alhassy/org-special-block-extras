;;; org-deftag-tests.el --- Tests for org-deftag: tags as functions on Org sections  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

;; A tag that shows exactly what it was given: [heading | color | size | backend].
;; We unhook it at once, so it never rewrites the exports of other tests; the relation calls it directly.
(org-deftag test-shout (&optional color size)
  "Replace the heading with what the tag received."
  (insert (format "[%s | %s | %s | %s]" o-heading color size o-backend)))
(remove-hook 'org-export-before-parsing-functions #'org-deftag/test-shout)

(snap-define-relation org-deftag (input output)
  "Running `org-deftag/test-shout' over the Org INPUT yields OUTPUT, modulo trailing whitespace per line."
  (let ((actual (with-org-test-buffer (:text (lf-string input))
                  (org-deftag/test-shout 'html)
                  (buffer-string)))
        (trim (lambda (s) (replace-regexp-in-string "[ \t]+$" "" s))))
    (should (equal (funcall trim output) (funcall trim actual)))
    (list :output actual)))

;;; Where a tag's arguments come from

(define-org-deftag-test "org-deftag reads arguments from the tag, underscore-separated"
  :input "* Loud :test-shout_red_big:"
  :output "[* Loud | red | big | html]
")

(define-org-deftag-test "org-deftag reads arguments from properties when the tag is bare"
  :input "* Quiet :test-shout:
          :PROPERTIES:
          :COLOR: blue
          :SIZE: small
          :END:"
  :output "[* Quiet | blue | small | html]
")

(define-org-deftag-test "org-deftag reads omitted trailing arguments from properties"
  :input "* Mixed :test-shout_red:
          :PROPERTIES:
          :SIZE: small
          :END:"
  :output "[* Mixed | red | small | html]
")

(define-org-deftag-test "org-deftag prefers the tag over a property of the same name"
  :input "* Both :test-shout_red:
          :PROPERTIES:
          :COLOR: blue
          :END:"
  :output "[* Both | red | nil | html]
")

(define-org-deftag-test "org-deftag binds nil when neither tag nor property gives an argument"
  :input "* Bare :test-shout:"
  :output "[* Bare | nil | nil | html]
")

(define-org-deftag-test "org-deftag ignores case in tags and property names"
  :input "* Shouting :TEST-SHOUT:
          :PROPERTIES:
          :Color: blue
          :END:"
  :output "[* Shouting | blue | nil | html]
")

;;; Which sections, and which tags, are touched

(define-org-deftag-test "org-deftag keeps a section's other tags"
  :input "* Busy :draft:test-shout_red:keep:"
  :output "[* Busy :draft:keep: | red | nil | html]
")

(define-org-deftag-test "org-deftag leaves untagged sections untouched, property drawers and all"
  :input "* Plain
          :PROPERTIES:
          :CUSTOM_ID: plain-id
          :END:
          * Loud :test-shout_red:"
  :output "* Plain
:PROPERTIES:
:CUSTOM_ID: plain-id
:END:
[* Loud | red | nil | html]
")

(define-org-deftag-test "org-deftag does not fire for a tag that merely shares its name as a prefix"
  :input "* Different :test-shouty_red:"
  :output "* Different :test-shouty_red:")

(define-org-deftag-test "org-deftag rewrites every tagged section, at any depth"
  :input "* One :test-shout_red:
          ** Two :test-shout_blue:
          *** Three :test-shout:"
  :output "[* One | red | nil | html]
[** Two | blue | nil | html]
[*** Three | nil | nil | html]
")

;;; The body's anaphors, and the export hook

(deftest "org-deftag exposes every property as o-properties, keyed by downcased keywords"
  (org-deftag test-props ()
    "Replace the heading with its properties."
    (insert (format "%S" o-properties)))
  (unwind-protect
      (with-org-test-buffer (:text "
* Heading :test-props:
:PROPERTIES:
:COLOR: blue
:Custom_Id: x
:END:
")
        (org-deftag/test-props 'html)
        (should (equal "blue" (plist-get (car (read-from-string (buffer-string))) :color)))
        (should (equal "x" (plist-get (car (read-from-string (buffer-string))) :custom_id))))
    (remove-hook 'org-export-before-parsing-functions #'org-deftag/test-props)))

(deftest "org-deftag runs during export, receiving the export backend as o-backend"
  (org-deftag test-backend ()
    "Replace the heading with the backend's name."
    (insert (format "Exported to %s." o-backend)))
  (unwind-protect
      (progn
        (should (s-contains-p "Exported to latex." (org-export-string "* Heading :test-backend:" 'latex)))
        (should (s-contains-p "Exported to html." (org-export-string "* Heading :test-backend:" 'html))))
    (remove-hook 'org-export-before-parsing-functions #'org-deftag/test-backend)))

(deftest "org-deftag returns its function and hooks it into export"
  (let ((org-export-before-parsing-functions nil))
    (should (eq #'org-deftag/test-hooked (org-deftag test-hooked () "Do nothing." nil)))
    (should (memq #'org-deftag/test-hooked org-export-before-parsing-functions))))

;;; Arglists: cl-defun-style, with defaults and &key

(org-deftag test-cl (&optional (color "pink") size &key (note "none"))
  "Replace the heading with COLOR, SIZE, and NOTE."
  (insert (format "[%s | %s | %s]" color size note)))
(remove-hook 'org-export-before-parsing-functions #'org-deftag/test-cl)

(defun org-test--run-tag (tag input)
  "(TAG, INPUT) ↦ the Org INPUT after running the tag function TAG over it, for HTML."
  (with-org-test-buffer (:text (lf-string input))
    (funcall tag 'html)
    (s-trim (buffer-string))))

(define-org-call-test "cl-defun-style org-deftag falls back to &optional defaults"
  :call (org-test--run-tag #'org-deftag/test-cl "* Heading :test-cl:")
  :returns "[pink | nil | none]")

(define-org-call-test "cl-defun-style org-deftag binds &key arguments from properties only, never from tag items"
  :call (org-test--run-tag #'org-deftag/test-cl "* Heading :test-cl_red_big_ignored:
                                                 :PROPERTIES:
                                                 :NOTE: from the drawer
                                                 :END:")
  :returns "[red | big | from the drawer]")

(define-org-call-test "cl-defun-style org-deftag falls back to an &key default when the property is absent"
  :call (org-test--run-tag #'org-deftag/test-cl "* Heading :test-cl_red_big_ignored:")
  :returns "[red | big | none]")

(define-org-call-test "org--deftag-parse-args reads cl-defun-style arglists"
  :call (org--deftag-parse-args '(&optional (color "pink") size &key note))
  :returns ((color "pink" t) (size nil t) (note nil nil)))

(deftest "org--deftag-parse-args rejects a flattened arglist"
  (should-error (org--deftag-parse-args '(color "pink" size nil))))

(deftest "org--deftag-parse-args rejects arguments before &optional or &key"
  (should-error (org--deftag-parse-args '(color &optional size))))

;;; Edge cases of headings: planning lines, colons in titles, and Org's special properties

(org-deftag test-special (&key file tags)
  "Replace the heading with FILE and TAGS, which only properties may give."
  (insert (format "[%s | %s]" file tags)))
(remove-hook 'org-export-before-parsing-functions #'org-deftag/test-special)

(deftest "org-deftag removes a section's property drawer even beneath a planning line"
  (let ((result (org-test--run-tag #'org-deftag/test-shout "* Heading :test-shout_red:
                                                            SCHEDULED: <2026-10-13 Tue>
                                                            :PROPERTIES:
                                                            :SIZE: big
                                                            :END:
                                                            Body.")))
    (should (s-contains-p "| red | big |" result))
    (should-not (s-contains-p ":PROPERTIES:" result))))

(define-org-call-test "org-deftag ignores its name within a heading's title, outside its trailing tags"
  :call (org-test--run-tag #'org-deftag/test-shout "* Ratio a:test-shout:b")
  :returns "* Ratio a:test-shout:b")

(define-org-call-test "org-deftag binds &key arguments named after Org's special properties, such as FILE, only from the drawer"
  :call (org-test--run-tag #'org-deftag/test-special "* Heading :test-special:")
  :returns "[nil | nil]")

(provide 'org-deftag-tests)
;;; org-deftag-tests.el ends here
