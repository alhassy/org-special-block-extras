;;; org-links.el --- Built-in org-deflink link types  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 4.1.1
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1") (lf "26.10.1") (seq "2.0"))
;; Keywords: org, links, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Built-in link types: melpa, html-export-style, kbd, octoicon, link-here,
;; badge, tweet, reddit, github-*, twitter-follow, doc, show; and the documentation block.
;; Also houses the org-docs-* glossary system and org-ospe-html-export-preserving-whitespace.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;; Links
;;;;;; melpa link

(org-deflink melpa
  "Produce a Melpa badge for a given package O-LABEL, which links to the Melpa page.
We try to get the package's version from a constant “⟨O-LABEL⟩-version” if it exists."
  :face '(:box "purple" :foreground "purple")
  :export (format (concat "<a href=\"https://melpa.org/#/%s\">"
                          "<img alt=\"MELPA\" src=\"https://img.shields.io/badge/%s-%s-green?logo=Gnu-Emacs\"></img>"
                          "</a>")
                  o-label
                  (s-replace "_" "__" (s-replace "-" "--" o-label)) ;; shields.io conventions
                  (or (ignore-errors (eval (intern (concat o-label "-version")))) "Melpa")))

;;;;;; html-export-style

(defvar org-html-export-styles
  `((default . "")
 (bigblow . "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup")
    (readtheorg . "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup")
    (rose . "#+HTML_HEAD: <link href=\"https://taopeng.me/org-notes-style/css/notes.css\" rel=\"stylesheet\" type=\"text/css\" />")
    (latexcss . "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"https://latex.now.sh/style.min.css\" />"))
  "An alist of theme-to-setup pairs, symbols-to-strings, used by `org-link/html-export-style'.

  For live examples of many of the themes, see
  https://olmon.gitlab.io/org-themes/.

  In due time, I would like to add more, such as those linked from
  the discussion https://news.ycombinator.com/item?id=23130104.
  A nice, simple, opportunity for someone else to contribute.")
;;
;; Add a bunch more
(cl-loop for theme in '(comfy_inline imagine_light
                                     rethink_inline simple_whiteblue
                                     retro_dark simple_gray solarized_dark
                                     solarized_light stylish_white)
         do (push (cons theme (format "#+SETUPFILE: https://gitlab.com/OlMon/org-themes/-/raw/master/src/%s/%s.theme" theme theme)) org-html-export-styles))

(defun org--html-export-style-names ()
  "The names of the `org-html-export-styles', and \"random\"."
  (cons "random" (--map (symbol-name (car it)) org-html-export-styles)))

(defun org--html-export-style-setup (_backend)
  "Insert the setup of the theme named by the buffer's “html-export-style:THEME” link, if any.

THEME is one of `org-html-export-styles', or random; it is read at export time, so a
buffer's choice never leaks into another's export."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "html-export-style:\\([^ \t\n]+\\)" nil t)
      (let* ((label (match-string 1))
             (theme (if (equal label "random") (car (seq-random-elt org-html-export-styles)) (intern label))))
        (goto-char (point-min))
        (insert (format "\n %s \n" (cdr (or (assoc theme org-html-export-styles)
                                             (error "Error: Unknown html-export-style ∷ %s ∉ '%s"
                                                    label (org--html-export-style-names))))))))))


(org-deflink html-export-style
  "Add a dedicated style theme, from `org-html-export-styles'."
  :face '(:underline "green")
  :keymap ("?" ;; Let use select new choice of style with the “?” key.
           (-let [choice (completing-read "New HTML style: " (org--html-export-style-names))]
             (save-excursion
               (beginning-of-line)
               (while (re-search-forward (rx (seq "html-export-style:" (one-or-more word))) nil t)
                 (replace-match (concat "html-export-style:" choice))))))
  :help-echo (thread-last (org--html-export-style-names)
                          (-partition 4)
                          (--map (s-join "     " it))
                          (s-join "\n")
                          (format "Press “?” to change the theme. \n\n Supported themes include:\n\n%s"))
  ;; The theme is set up by `org--html-export-style-setup'; the link itself exports to nothing.
  :export "")

;;;;;; Kbd
(defun org--html-tooltip (title body &optional style)
  "(TITLE, BODY, STYLE) ↦ BODY as HTML, showing TITLE on hover; e.g., <abbr class=\"tooltip\" title=\"TITLE\">BODY</abbr>."
  (format "<abbr class=\"tooltip\"%s title=\"%s\">%s</abbr>"
          (if style (format " style=\"%s\"" style) "") title body))

(org-deflink kbd
  "Show keysequence O-LABEL in a nice grey button-like font, along with a tooltip of its documentation, if any.

Such links do not get folded in [[bracket]] style, and are rendered as buttons within Emacs.

Moreover, O-LABEL may use ‘_’ in-lieu of spaces or [[bracket]] link notation.

Examples:
    [[kbd:C-x C-s]]
  ≈ <kbd: C-x C-s>
  ≈ kbd:C-x_C-s"
  :display 'full
  :let (the-label  (s-trim (s-replace "_" " " o-label))
        lisp-func  (ignore-errors (key-binding (kbd the-label)))
        tooltip    (or o-description (ignore-errors (documentation lisp-func)) "")
        tooltip?   (not (equal tooltip ""))
        style      (if tooltip? "border-color: red" "")
        keystrokes (format "<kbd style=\"%s\">%s</kbd>" style the-label))
  ;; o-description is always nil when it comes to deciding the :face.
  :face (list :inherit 'custom-button :box (if tooltip? "red" t))
  :help-echo (format "%s ∷ %s\n%s" the-label (or lisp-func "") tooltip)
  :export (cond ((equal o-backend 'latex) (format "\\texttt{%s}" the-label))
                ;; The style is to remove the underlining caused by <abbr>.
                (tooltip? (org--html-tooltip
                           (format "%s ∷ %s<br>%s" the-label (or lisp-func "") (org-ospe-html-export-preserving-whitespace tooltip))
                           keystrokes
                           "border: none; text-decoration: none;"))
                (t keystrokes)))

;;;;;; Octoicon

(defvar
  org--supported-octoicons
  (--map (list (car it) (s-collapse-whitespace (cadr it)))
   (-partition 2
              '(
                home
                "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16
   16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M16 9l-3-3V2h-2v2L8 1 0 9h2l1 5c0 .55.45 1 1 1h8c.55 0
   1-.45 1-1l1-5h2zm-4 5H9v-4H7v4H4L2.81 7.69 8 2.5l5.19 5.19L12
   14z\"></path></svg>"

                link
                "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16
   16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69
   3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10
   5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0
   2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5
   0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55
   13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z\"></path></svg>"

                mail
                "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 14
   16\" width=\"14\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M0 4v8c0 .55.45 1 1 1h12c.55 0 1-.45
   1-1V4c0-.55-.45-1-1-1H1c-.55 0-1 .45-1 1zm13 0L7 9 1 4h12zM1
   5.5l4 3-4 3v-6zM2 12l3.5-3L7 10.5 8.5 9l3.5 3H2zm11-.5l-4-3
   4-3v6z\"></path></svg>"

                report
                "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16
   16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M0 2a1 1 0 011-1h14a1 1 0 011 1v9a1 1 0 01-1 1H7l-4
   4v-4H1a1 1 0 01-1-1V2zm1 0h14v9H6.5L4 13.5V11H1V2zm6
   6h2v2H7V8zm0-5h2v4H7V3z\"></path></svg>"

                tag
                "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 15
   16\" width=\"15\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M7.73 1.73C7.26 1.26 6.62 1 5.96 1H3.5C2.13 1 1 2.13 1
   3.5v2.47c0 .66.27 1.3.73 1.77l6.06 6.06c.39.39 1.02.39 1.41
   0l4.59-4.59a.996.996 0 000-1.41L7.73 1.73zM2.38
   7.09c-.31-.3-.47-.7-.47-1.13V3.5c0-.88.72-1.59
   1.59-1.59h2.47c.42 0 .83.16 1.13.47l6.14 6.13-4.73
   4.73-6.13-6.15zM3.01 3h2v2H3V3h.01z\"></path></svg>"

                clock
                "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 14
   16\" width=\"14\" height=\"16\"><path fill-rule=\"evenodd\"
   d=\"M8 8h3v2H7c-.55 0-1-.45-1-1V4h2v4zM7 2.3c3.14 0 5.7 2.56
   5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 011.3 8c0-3.14 2.56-5.7
   5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14
   7-7-3.14-7-7-7z\"></path></svg>")))

  "An association list of supported OctoIcons, their SVGs on a single line.

Usage: (cadr (assoc 'ICON org--supported-octoicons))")

(defun org--octoicon (name)
  "The SVG of the OctoIcon NAME, a symbol; e.g., `link'.  See `org--supported-octoicons'."
  (cadr (assoc name org--supported-octoicons)))

(org-deflink octoicon
  "Show an OctoIcon; one of the `org--supported-octoicons', such as home, link, or mail."
  :export (progn
            (unless (org--octoicon (intern o-label))
              (error "octoicon:%s ⇒ This label is not supported; try one of %s" o-label (mapcar #'car org--supported-octoicons)))
            (if (equal o-backend 'html) (org--octoicon (intern o-label)) "")))

;;;;;; Link-Here

(org-deflink link-here
  "Export a link to the current location in an Org file."
  :help-echo (format "This is a local anchor link named “%s”" o-label)
  :export (if (equal o-backend 'html)
              (format "<a class=\"anchor\" aria-hidden=\"true\" id=\"%s\" href=\"#%s\">%s</a>"
                      o-label o-label (org--octoicon 'link))
            ""))

;;;;;; Badge (org-make-badge)

(cl-defmacro org-make-badge
    (name &optional social-shields-name social-url social-shields-url )
  "Make a link NAME whose export is presented as an SVG badge.

If the link is intend to be a social badge, then, adhering to
shield.io conventions, the appropriate SOCIAL-SHIELDS-NAME must
be given.

The SOCIAL-URL is a URL that the badge points to and it should
have a single ‘%s’ where the link label argument will go.

Some social badges have a uniform URL, but otherwise, such as
twitter, deviate and so need their own SOCIAL-SHIELDS-URL,
which has a single ‘%s’ for the link's label argument.

----------------------------------------------------------------------

E.g., to create a badge named “go”:

     (org-make-badge \"go\")

Then the following exports nicely from an Org file:

     go:key|value|blue|here|gnu-emacs"
  `(org-deflink ,(intern name)
     "Export a Shields.io badge, with general Syntax:  badge:key|value|colour|url|logo.
Precise details for each argument are shown in the Emacs tooltip for this badge."
     :display 'full
     :let (parts (mapcar #'s-trim (s-split "|" o-label))
           key (nth 0 parts) value (nth 1 parts) color (nth 2 parts) url (nth 3 parts) logo (nth 4 parts))
     :follow (browse-url (or url o-label))
     :help-echo
     ;; `format', not `lf-string': the latter sees only dynamic variables, and these are lexical.
     (format (lf-unindent "%s
          \nGeneral Syntax:  badge:key|value|colour|url|logo
                 Key    : %s
                 Value  : %s
                 Colour : %s
                 URL    : %s
                 Logo   : %s

          This results in an SVG badge “[key∣value]”, where the 'key'
          is coloured grey and the ‘value’ is coloured ‘color’;
          for the HTML backend, and otherwise are silently omitted.
          Descriptions are ignored; i.e., ‘[[badge:label][description]]’
          is the same as ‘[[badge:label]]’.

          ► ‘key’ and ‘value’ have their underscores interpreted as spaces.
             ⇒ ‘__’ is interpreted as an underscore;
               Of course, you can write ‘<badge: ⋯>’, then ‘⋯’ may have multiword, spaced, content.
             ⇒ ‘|’ is not a valid substring, but ‘-, %%, ?’ are okay.

          ► ‘|color|url|logo’ are optional;
             ⇒ If ‘url’ is not present, the resulting badge is not a hyperlink.
             ⇒ if ‘url’ is ‘here’ then we have a local link;
                i.e., the resulting badge behaves like ‘link-here:key’.
             ⇒ ‘color’ may be: ‘brightgreen’ or ‘success’,
                               ‘red’         or ‘important’,
                               ‘orange’      or ‘critical’,
                               ‘lightgrey’   or ‘inactive’,
                               ‘blue’        or ‘informational’,
               or ‘green’, ‘yellowgreen’, ‘yellow’, ‘blueviolet’, ‘ff69b4’, etc.
               Consult https://htmlcolorcodes.com/ to see the HEX codes of other colours.
             ⇒ ‘logo’ examples and how they look can be found at
                https://alhassy.github.io/org-special-block-extras/#Example-Badge-Icons

         See also: https://alhassy.github.io/org-special-block-extras/#Common-Project-Badges")
             o-label key (or value "") (or color "green") (or url "") (or logo ""))
     :export
     (if (equal o-backend 'latex) ""
       (let ((img ,(if social-shields-name
                       (if social-shields-url
                           `(format ,social-shields-url o-label)
                         `(format "<img src=\"https://img.shields.io/%s/%s?style=social\">" ,social-shields-name o-label))
                     '(format "<img src=\"https://img.shields.io/badge/%s-%s-%s?logo=%s\">"
                              (url-hexify-string (s-replace "-" "--" key))
                              (url-hexify-string (s-replace "-" "--" (or value "")))
                              color
                              logo)))
             (target ,(if social-shields-name `(format ,social-url o-label) 'url)))
         (pcase target
           ("here" (format "<a id=\"%s\" href=\"#%s\">%s</a>" key key img))
           ((or "" 'nil) img) ;; e.g., badge:key|value|color||logo or badge:key|value|color
           (_ (format "<a href=\"%s\">%s</a>" target img)))))))

(org-make-badge "badge")

;; Since we're invoking a macro, the twitter-excitement is used lazily; i.e.,
;; consulted when needed instead of being evaluated once.
(defvar org-link-twitter-excitement
  "This looks super neat (•̀ᴗ•́)و:"
  "The string prefixing the URL being shared.")

(org-make-badge
 "tweet"
 "twitter/url?=url="
 (format
  "https://twitter.com/intent/tweet?text=%s:&url=%%s"
  org-link-twitter-excitement)
 "<img src=\"https://img.shields.io/twitter/url?url=%s\">"
 )

;; The social badges: each is a Shields.io badge, its URL built from the link's label.
(cl-loop for (social url name)
         in '(("reddit/subreddit-subscribers" "https://www.reddit.com/r/%s" "reddit")
              ("github" "https://www.github.com/%s")
              ("github/stars" "https://www.github.com/%s/stars")
              ("github/watchers" "https://www.github.com/%s/watchers")
              ("github/followers" "https://www.github.com/%s?tab=followers")
              ("github/forks" "https://www.github.com/%s/fork")
              ("twitter/follow" "https://twitter.com/intent/follow?screen_name=%s"))
         for name′ = (or name (s-replace "/" "-" social))
         do (eval `(org-make-badge ,name′ ,social ,url)))

;;;;;; Doc

(defvar org--docs nil
  "An alist of (LABEL NAME DESCRIPTION) entries; our glossary.

Example setter:
0. (org-docs-set \"os\" \"Emacs\" \"A place wherein I do all of my computing.\")

Example getters:
0. (org-docs-get LABEL)
1. (-let [(name description) (cdr (assoc LABEL org--docs))] ⋯)

See also `org--docs-from-libraries' and `org-docs-load-libraries'.")

(cl-defun org-docs-set (label name description)
  "Add a new documentation-glossary entry, if it is not already present.

We associate LABEL to have title NAME and glossary value DESCRIPTION.

Example usage:
  (org-docs-set \"cat\"
              \"Category Theory\"
              \"A theory of typed  composition; e.g., typed monoids.\")"
  (add-to-list 'org--docs (list label name description)))

(defvar org--docs-from-libraries nil

  "The alist of (label name description) entries loaded from the libraries.

The ‘doc:𝒳’ link will load the libraries, possibly setting this variable to ‘nil’,
then make use of this variable when looking for documentation strings.

Interactively call `org-docs-load-libraries'
to force your documentation libraries to be reloaded.

See also `org-docs-libraries'.")

(defvar org--docs-wordnet nil
  "A hash table caching the English definition, from WordNet's ‘wn’, of each label looked up.")

(defun org--docs-wordnet (word)
  "WORD's English definition, from the command line tool ‘wn’, WordNet; cached, as ‘wn’ is a subprocess."
  (unless org--docs-wordnet (setq org--docs-wordnet (make-hash-table :test #'equal)))
  (or (gethash word org--docs-wordnet)
      (puthash word (shell-command-to-string (format "wn %s -over -synsn" (shell-quote-argument word))) org--docs-wordnet)))

(defvar org-docs-fallback
  (lambda (label)
    (-when-let (documentation
                (or (ignore-errors (documentation (intern label)))
                    (ignore-errors (documentation-property (intern label) 'variable-documentation))
                    (-let [it (org--docs-wordnet label)]
                      (unless (and (s-blank-p it) (not org-export-with-broken-links)) it))))
      (list label label documentation)))

  "The fallback method to retrieving documentation or glossary entries, as (LABEL NAME DOCUMENTATION); else nil.

We try to retrieve the Emacs Lisp function documentation of the
given LABEL, if possible, otherwise we try to retrieve the Emacs
Lisp variable documentation, and if that fails then we look up
the word in the English dictionary.

The English definition is obtained from the command line tool ‘wn’, WordNet.")

(cl-defun org-docs-get (label)
  "Return the name and documentation-glossary values associated with LABEL.

It returns a list of length 2.

Example uses:

  ;; Get the Lisp documentation of `thread-last'
  (org-docs-get \"thread-last\")

  ;; Get the English definition of ‘computing’
  (org-docs-get \"computing\")

We look for LABEL from within the current buffer first, using `org--docs',
and otherwise look among the loaded libraries, using `org--docs-from-libraries',
and, finally, look for the documentation entry using `org-docs-fallback'."
  (cdr (or (assoc label org--docs)
           (assoc label org--docs-from-libraries)
           (funcall org-docs-fallback label)
           (error "Error: No documentation-glossary entry for “%s”!" label))))


(cl-defun org-docs-insert ()
  "Insert a “doc:𝒳” link from user's documentation-glossary database.

It can be tricky to remember what you have, or what documentation entries mention, and so
this command gives a searchable way to insert doc links."
  (interactive)
  (thread-last
    (cl-remove-duplicates (-concat org--docs org--docs-from-libraries) :key #'car :test #'cl-equalp)
    (--map (format "%s ∷ %s" (car it) (cl-third it)))
    (completing-read "Insert doc link ∷ ")
    (s-split "∷")
    car
    (concat "doc:")
    (insert)))

(defvar org-docs-libraries nil
  "List of Org files that have ‘#+begin_documentation’ blocks that should be loaded
   for use with the ‘doc:𝒳’ link type.")

(cl-defun org-docs-load-libraries
    (&optional (libs org-docs-libraries))
  "Load documentation-glossary libraries LIBS.

If no LIBS are provided, simply use those declared
org-docs-libraries.

See `org--docs-from-libraries'."
  (interactive)
  (cl-loop for lib in libs
           do (with-temp-buffer
          (insert-file-contents lib)
                ;; doc only activates after an export
                (-let [org-export-with-broken-links t] (org-html-export-as-html))
                (kill-buffer)
                (delete-window)
                (setq org--docs-from-libraries (-concat org--docs org--docs-from-libraries))
                (setq org--docs nil))))

(defvar org--docs-actually-used nil
  "Which words are actually cited in the current article.

We use this listing to actually print a glossary using
‘show:GLOSSARY’.")

(org-deflink doc
  "Export O-LABEL as itself, or as the provided O-DESCRIPTION,
 along with a tooltip that shows the user's
 documentation-glossary for o-LABEL and using that entry's name
 when no O-DESCRIPTION is provided."
  :let (entry (org-docs-get o-label)
        name (cl-first entry)
        docs (cl-second entry)
        display-name (or o-description name))
  :help-echo (format "[%s] %s :: %s" o-label name docs)
  :face '(custom-button)
  :export (progn
            (add-to-list 'org--docs-actually-used (list o-label name docs))
            (pcase o-backend
              ('html (org--html-tooltip (org-ospe-html-export-preserving-whitespace docs) display-name))
              ;; Make the current word refer to its glossary entry, and declare the location
              ;; that the glossary should refer back to; see the GLOSSARY of `org-link/show'.
              ('latex (format "\\hyperref[o-glossary-%s]{%s}\\label{o-glossary-declaration-site-%s}"
                              o-label display-name o-label))
              (_ display-name))))

;; WHERE ...

(defun org-ospe-html-export-preserving-whitespace (s)
  "Make Org-markup'd string S ready for HTML presentation, preserving whitespace.

This is orthogonal to the  `org-export-string-as' command; i.e.,
(org-export-string-as s 'html :body-only-please) does something
else!  In particular, what this yields is an HTML rendition that
does not account for whitespace, such as indentation and
newlines.
"
  ;; Make it look pretty!
  (thread-last s
               (s-replace-regexp "\\#\\+begin_src [^\n]*\n" "<pre class='tooltip'>")
               (s-replace-regexp "\\( \\)*\\#\\+end_src\n" "</pre>")
               (s-replace-regexp "\\#\\+begin_export [^\n]*\n" "")
               (s-replace-regexp "\\( \\)*\\#\\+end_export" "")
               (s-replace "  " "&emsp;") ; Preserve whitespace
               (s-replace "\n" "<br>")   ; Preserve newlines
               (s-replace-regexp "\\#\\+begin_example<br>" "")
               (s-replace-regexp "\\#\\+end_example<br>" "")
               ;; Translate Org markup
               ;; Only replace /.*/ by <em>.*<em> when it does not have an alphanum,:,/,< before it.
               (s-replace-regexp "\\([^a-z0-9A-Z:/<]\\)/\\(.+?\\)/" "\\1<em>\\2</em>")
               (s-replace-regexp "\\*\\(.+?\\)\\*" "<strong>\\1</strong>")
               (s-replace-regexp "\\~\\([^ ].*?\\)\\~" "<code>\\1</code>")
               (s-replace-regexp "\\$\\(.+?\\)\\$" "<em>\\1</em>")
               (s-replace-regexp "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" "\\2 (\\1)")
               ;; 5+ dashes result in a horizontal line
               (s-replace-regexp "-----+" "<hr>")
               ;; Spacing in math mode
               (s-replace-regexp "\\\\quad" "&#x2000;")
               (s-replace-regexp "\\\\," "&#8194;") ;; en space
               (s-replace-regexp "\\\\;" "&#8195;") ;; em space
               ;; The presence of ‘\"’ in tooltips breaks things, so omit them.
               (s-replace-regexp "\\\"" "''")))

;;;;;;; Documentation

(org-defblock documentation
              (name (error "Documentation block: Name must be provided")
                    label nil show nil color "green")
              "Register the dictionary entries in CONTENTS to the dictionary variable.

The dictionary variable is ‘org--docs’.

A documentation entry may have its LABEL, its primary identifier,
be:
1. Omitted
2. Given as a single symbol
3. Given as a many aliases '(lbl₀ lbl₁ … lblₙ)

The third case is for when there is no canonical label to refer to
an entry, or it is convenient to use multiple labels for the same
entry.

In all of the above cases, two additional labels are included:
The entry name with spaces replaced by underscores, and again but
all lower case.

Documentation blocks are not shown upon export;
unless SHOW is non-nil, in which case they are shown
using the ‘box’ block, with the provided COLOR passed to it.

In the futture, it may be nice to have an option to render tooltips.
That'd require the ‘doc:𝒳’ link construction be refactored via a ‘defun’."
              (unless (consp label) (setq label (list label)))
              (push (s-replace " " "_" name) label)
              (push (downcase (s-replace " " "_" name)) label)
              (cl-loop for l in label
                       do  (add-to-list 'org--docs
                                        (mapcar #'s-trim (list (format "%s" l) name (substring-no-properties raw-contents)))))
              ;; Should the special block show something upon export?
              (if show (org--blockcall box name :background-color color raw-contents) ""))

;;;;;; Show

(org-deflink show
  "Yield the value of the expression O-LABEL, with =GLOSSARY= being a reserved name.

Example uses:

    show:user-full-name

    <show: (* 2 (+ 3 4 (- pi))) >


Note that there is `elisp' links with Emacs, out of the box.
However, they only serve to evaluate Lisp expressions; for example,
to make “link buttons” that do useful things, as follows.

   [[elisp:(find-file user-init-file)][Init]]

In particular, `elisp' links do not export the value of their expression.
That is what we accomplish with this new `show' link type."
  :face '(:underline "green")
  :let (glossary? (equal o-label "GLOSSARY")
        o-value (pp-to-string (if glossary?
                                  (mapcar #'cl-second org--docs-actually-used)
                                (eval (car (read-from-string o-label)))))
        o-expr (if glossary?
                   (concat "GLOSSARY ---i.e., org--docs-actually-used"
                           "\n\nWe erase the glossary not on the first export, but on the second export."
                           "\nThe first export collects all citations, which are used in the second export.")
                 o-label))
  :help-echo (format (concat "Upon export, the following will be placed literally"
                             "\n\t%s"
                             "\nWhich is the value of the expression:\n\t%s")
                     o-value o-expr)
  :export (cond ((not glossary?) o-value)
                ;; Otherwise O-LABEL is glossary, which we print in HTML & LaTeX
                ((equal 'html o-backend)
                 (s-join " "
                         (--map (org--html-tooltip (org-ospe-html-export-preserving-whitespace (cl-third it)) (cl-second it))
                                ;; Ignore duplicates; i.e., entries with the same name/title.
                                (cl-remove-duplicates org--docs-actually-used :key #'cl-second :test #'cl-equalp))))
                ;; Each entry is labelled for `org-link/doc' to refer to, and refers back to where it was cited.
                (t (s-join "\n\n"
                           (cl-loop for (label name doc) in org--docs-actually-used
                                    collect
                                    (format (concat "\\vspace{1em}\\phantomsection"
                                                    "\\textbf{%s}\\quad"
                                                    "\\label{o-glossary-%s}"
                                                    "%s See page "
                                                    "\\pageref{o-glossary-declaration-site-%s}")
                                            name
                                            label
                                            (when doc
                                              (thread-last doc ;; preserve whitespace
                                                           (s-replace "&" "\\&") ;; Hack!
                                                           (s-replace "  " " \\quad ")
                                                           (s-replace "\n" " \\newline{\\color{white}.}")))
                                            label))))))

(provide 'org-links)
;;; org-links.el ends here
