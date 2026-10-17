;;; org-color-links-tests.el --- Tests for color blocks  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-call-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-test-utils.el")))

(define-org-call-test "red block wraps text in a red <span> for HTML."
  :call (org-block/red 'html "hello" nil)
  :returns "#+begin_export html
<span style=\"color:red;\">
#+end_export
hello
#+begin_export html
</span>
#+end_export")

(define-org-call-test "blue block uses \\begingroup\\color{blue} for LaTeX."
  :call (org-block/blue 'latex "world" nil)
  :returns "#+begin_export latex
\\begingroup\\color{blue}
#+end_export
world
#+begin_export latex
\\endgroup\\,
#+end_export")

(define-org-call-test "green block wraps text in a green <span> for HTML."
  :call (org-block/green 'html "text" nil)
  :returns "#+begin_export html
<span style=\"color:green;\">
#+end_export
text
#+begin_export html
</span>
#+end_export")

(define-org-call-test "generic color block respects the color main-arg in HTML."
  :call (org-block/color 'html "text" "purple")
  :returns "#+begin_export html
<span style=\"color:purple;\">
#+end_export
text
#+begin_export html
</span>
#+end_export")

(define-org-call-test "generic color block uses \\color for LaTeX."
  :call (org-block/color 'latex "text" "orange")
  :returns "#+begin_export latex
\\begingroup\\color{orange}
#+end_export
text
#+begin_export latex
\\endgroup\\,
#+end_export")

(deftest "All 19 named colour blocks are registered as handler functions."
  (dolist (colour '(black blue brown cyan darkgray gray green lightgray lime
                          magenta olive orange pink purple red teal violet white yellow))
    (should (fboundp (intern (format "org-block/%s" colour))))))

;;; Highlight

(define-org-export-test "a highlight link colours the background of its description in HTML" [highlight]
  :input "
          [[highlight:pink][Read this twice.]]"
  :equals "<p>
           <span style=\"background-color:pink;\">Read this twice.</span></p>
           ")

(define-org-export-test "a highlight link is an inline \\colorbox in LaTeX" [highlight]
  :input "
          [[highlight:pink][Read this twice.]]"
  :to latex
  :equals "\\colorlet{osbe-highlight}{pink}\\colorbox{osbe-highlight}{Read this twice.}
           ")

(define-org-export-test "a highlight block, of a hex colour, is a full-width paragraph box in LaTeX" [highlight]
  :input "
          #+begin_highlight #e5f5e5
          A whole paragraph.
          #+end_highlight"
  :to latex
  :equals "\\definecolor{osbe-highlight}{HTML}{e5f5e5}
\\colorbox{osbe-highlight}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep}{%
A whole paragraph.
\\ifhmode\\unskip\\fi}}
")

(org-deftest "a highlight link shows, in Emacs, with its colour as background" [highlight]
  :text "
Do [[highlight:pink][read this]] twice."
  :at "read"
  (font-lock-ensure)
  (should (equal (get-text-property (point) 'face) '(:background "pink"))))

;;; Palette

(deftest "a palette link is a button unfolding one button per `org-palette-colors' in HTML; nothing elsewhere" [palette]
  (let ((html (org-export-string "\n[[palette:][Pick a background]]")))
    (should (string-match-p ">Pick a background</button><div style=\"display: none\">" html))
    (should (equal (length org-palette-colors) (s-count-matches "document.body.style.background" html))))
  (should (string-match-p ">🎨</button>" (org-export-string "\n[[palette:]]")))
  (should (equal "" (org-export-string "\n[[palette:][Pick a background]]" 'latex))))

(org-deftest "a palette link shows, in Emacs, with a colour name as background, a string as htmlize needs" [palette]
  :text "
Do [[palette:][Pick a background]] now."
  :at "Pick"
  (font-lock-ensure)
  (should (equal (get-text-property (point) 'face) '(:background "white" :box t))))

(org-deftest "in Emacs, a colour link shows its text in its colour: a named one, color: by name or hex; issue #58" [color]
  :text "
[[color:red][this is red text]]
[[color:#ff0066][pinkish text]]
[[red:][hey]]
[[red:This will appear red]]"
  (font-lock-ensure)
  (pcase-dolist (`(,text ,colour) '(("this is red" "red") ("pinkish" "#ff0066") ("hey" "red") ("This will" "red")))
    (goto-char (point-min))
    (search-forward text)
    (should (equal (plist-get (get-text-property (1- (point)) 'face) :foreground) colour))))

(provide 'org-color-links-tests)
;;; org-color-links-tests.el ends here
