;;; org-links.el --- Built-in org-deflink link types  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.16
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8") (lf "26.10.1") (seq "2.0"))
;; Keywords: org, links, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Built-in link types: melpa, html-export-style, kbd, octoicon, link-here,
;; badge, tweet, reddit, github-*, twitter-follow, doc, show; and the documentation block.
;; Also houses the org-docs-* glossary system and `org--html-tooltip'.
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
(defun org--html-tooltip (text body &optional plain? style)
  "(TEXT, BODY, PLAIN?, STYLE) ↦ BODY as HTML, showing TEXT on hover or tap; e.g., for TEXT \"A /typed/ monoid\",
  <abbr class=\"tooltip\" tabindex=\"0\">BODY<template><p>A <i>typed</i> monoid</p></template></abbr>
TEXT is Org, exported as such; or PLAIN? text, such as a docstring, shown as is.  tooltips.js shows the
<template>.  Markdown, which runs no script, gets TEXT as a plain title attribute instead."
  (if (org-export-markdown-p)
      (format "<abbr title=\"%s\">%s</abbr>"
              (s-replace-all '(("&" . "&amp;") ("\"" . "&quot;") ("<" . "&lt;") (">" . "&gt;") ("\n" . "&#10;")) text)
              body)
    (org--tooltip-abbr body (org--tooltip-html text plain?) (and style (format "style=\"%s\"" style)))))

(defun org--tooltip-abbr (body html &optional attributes)
  "(BODY, HTML, ATTRIBUTES) ↦ BODY, showing HTML on hover or tap; tooltips.js shows the <template>.
  \"°\", \"<p>Hi</p>\", \"id=\\\"x\\\"\"  ↦  <abbr class=\"tooltip\" tabindex=\"0\" id=\"x\">°<template><p>Hi</p></template></abbr>"
  (format "<abbr class=\"tooltip\" tabindex=\"0\"%s>%s<template>%s</template></abbr>"
          (if attributes (concat " " attributes) "") body html))

(defun org--tooltip-html (text &optional plain?)
  "(TEXT, PLAIN?) ↦ HTML of a tooltip's TEXT: its body-only export as Org; or, when PLAIN?, TEXT escaped in a <pre>.
The nested export keeps the outer one's glossary, which `org--docs-prepare' would otherwise reset."
  (if plain?
      (format "<pre>%s</pre>" (org-html-encode-plain-text (string-trim text)))
    (let ((org-export-before-parsing-functions (remq #'org--docs-prepare org-export-before-parsing-functions)))
      (string-trim (org-export-string-as text 'html t)))))

(org-deflink kbd
  "Show keysequence O-LABEL in a nice grey button-like font, along with a tooltip of its documentation, if any.

Such links do not get folded in [[bracket]] style, and are rendered as buttons within Emacs; clicking one
runs its keys, so readers may try them out.  Keys may run any command, so we first ask, as `elisp' links
do, via `org-link-elisp-confirm-function'.

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
  :help-echo (format "%s ∷ %s\n%s\n\nClick to run these keys." the-label (or lisp-func "") tooltip)
  :follow (org--kbd-run the-label)
  :export (cond ((equal o-backend 'latex) (format "\\texttt{%s}" the-label))
                ;; The style is to remove the underlining caused by <abbr>.
                (tooltip? (org--html-tooltip (format "%s ∷ %s\n%s" the-label (or lisp-func "") tooltip) keystrokes
                                             :plain "border: none; text-decoration: none;"))
                (t keystrokes)))

(defun org--kbd-run (keys)
  "Run the key sequence KEYS, such as \"C-x 3 C-x o\", once `org-link-elisp-confirm-function' agrees; else nothing."
  (when (or (not org-link-elisp-confirm-function)
            (funcall org-link-elisp-confirm-function (format "Run the keys “%s”? " keys)))
    (execute-kbd-macro (read-kbd-macro keys))))

(defun org--kbd-prettify (start end _path _bracket?)
  "Show the kbd: link from START to END as 〔C-x C-s〕, in Emacs: its markup becomes the brackets, and each ‘_’ a space.
  kbd:C-x_C-s   [[kbd:C-x C-s]]   <kbd: C-x C-s>   ↦   〔C-x C-s〕
A description, as in [[kbd:C-x C-s][Save]], is hidden too; it is still the tooltip.
Only the markup is displayed differently, so the keys themselves stay editable."
  (save-excursion
    (goto-char start)
    (when (re-search-forward "kbd:[ \t]*" end t)
      (let* ((keys-start (point))
             (keys-end (if (re-search-forward "[]>]" end t) (match-beginning 0) end)))
        (org--display-as start keys-start "〔")
        (cl-loop for underscore from keys-start below keys-end
                 when (eq (char-after underscore) ?_) do (org--display-as underscore (1+ underscore) " "))
        (if (< keys-end end)
            (org--display-as keys-end end "〕")
          ;; A plain kbd:C-x_C-s has no closing markup, so its last key carries the closing bracket.
          (org--display-as (1- keys-end) keys-end (concat (buffer-substring-no-properties (1- keys-end) keys-end) "〕")))))))

(org-link-set-parameters "kbd" :activate-func #'org--kbd-prettify)

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
                     ;; An omitted, or blank, colour is green; an omitted logo is no logo at all.
                     '(format "<img src=\"https://img.shields.io/badge/%s-%s-%s%s\">"
                              (url-hexify-string (s-replace "-" "--" key))
                              (url-hexify-string (s-replace "-" "--" (or value "")))
                              (or (org-string-nw-p color) "green")
                              (if (org-string-nw-p logo) (format "?logo=%s" logo) ""))))
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
              ("github/stars" "https://github.com/%s")      ; Its Star button; /stars is no page
              ("github/watchers" "https://github.com/%s")   ; Its Watch button; /watchers needs a login
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
  "List of Org files whose glossary entries the ‘doc:𝒳’ link type may cite.

An entry is either a block or, “Org file as data”, a heading:

  #+begin_documentation Category Theory :label (cat cats)
  A theory of typed composition; e.g., typed monoids.
  #+end_documentation

  * Category Theory                                  :documentation:
    :PROPERTIES:
    :DOC_KEYS: cat cats
    :END:
    A theory of typed composition; e.g., typed monoids.

See `org--docs-read-buffer'.")

(cl-defun org-docs-load-libraries (&optional (libs org-docs-libraries))
  "Load the glossary entries of the Org files LIBS, by default `org-docs-libraries', into `org--docs-from-libraries'.
The files are read, not exported; so this is quick, and reloading replaces the previous entries."
  (interactive)
  (setq org--docs-from-libraries
        (cl-loop for lib in libs
                 append (with-temp-buffer
                          (insert-file-contents lib)
                          (delay-mode-hooks (org-mode))
                          (org--docs-read-buffer)))))

(defun org--docs-entries (name label description)
  "(NAME, LABEL, DESCRIPTION) ↦ a (LABEL′ NAME DESCRIPTION) glossary entry for each LABEL′ among LABEL, and NAME in snake_case, as is and downcased.
LABEL may be nil, a label, or a list ---possibly quoted--- of labels."
  (when (eq (car-safe label) 'quote) (setq label (cadr label)))
  (let ((snake (s-replace " " "_" (s-trim name))))
    (--map (list it (s-trim name) (s-trim description))
           (-uniq (--map (s-trim (format "%s" it))
                         (-non-nil (append (list (downcase snake) snake) (if (listp label) label (list label)))))))))

(defun org--docs-read-buffer (&optional tree)
  "The (LABEL NAME DESCRIPTION) glossary entries of the current Org buffer, whose `org-element' parse is TREE.
They come from its documentation blocks; its headings tagged :documentation: ---whose title is the NAME,
whose :DOC_KEYS: property holds space-separated labels, and whose own text is the DESCRIPTION; and its
radio targets, each NAMEd by its text and DESCRIBEd by its paragraph:

  A <<<monoid>>> is a set with an associative operation and a unit.   ↦   doc:monoid

Later entries come first, so they win when labels clash; as when the blocks were run in order."
  (let (entries)
    (org-element-map (or tree (org-element-parse-buffer)) '(special-block headline radio-target)
      (lambda (it)
        (pcase (org-element-type it)
          ('radio-target
           (-when-let (paragraph (org-element-lineage it '(paragraph)))
             (push (org--docs-entries (org-element-property :value it) nil
                                      (replace-regexp-in-string
                                       "<<<\\(.*?\\)>>>" "\\1"
                                       (buffer-substring-no-properties (org-element-property :contents-begin paragraph)
                                                                       (org-element-property :contents-end paragraph))))
                   entries)))
          ('special-block
            (when (equal "documentation" (downcase (org-element-property :type it)))
              (save-excursion
                (goto-char (org-element-property :post-affiliated it))
                (-let [(&org-special-block 'main-arg 'kwdargs 'contents) (org-special-block-after-point "documentation")]
                  (unless (s-blank-p main-arg)
                    (push (org--docs-entries main-arg (plist-get kwdargs :label) contents) entries))))))
          (_ (when (member "documentation" (org-element-property :tags it))
               (push (org--docs-entries (org-element-property :raw-value it)
                                        (split-string (or (org-element-property :DOC_KEYS it) ""))
                                        (org--docs-heading-text it))
                     entries))))))
    (apply #'append entries)))

(defun org--docs-heading-text (headline)
  "HEADLINE ↦ its own text, sans planning line, property drawer, and subheadings."
  (-if-let (section (--first (eq (org-element-type it) 'section) (org-element-contents headline)))
      (let ((end (org-element-property :contents-end section)))
        (buffer-substring-no-properties
         (or (-some->> (org-element-contents section)
               (--remove (memq (org-element-type it) '(planning property-drawer)))
               car
               (org-element-property :begin))
             end)
         end))
    ""))

(defun org--docs-prepare (&rest _)
  "Before each export: reset the glossary to the buffer's own entries, and its citations to the buffer's doc: links.
Citations are gathered up front, so show:GLOSSARY lists them all in one export, wherever it sits;
nothing carries over from one export, or buffer, to the next.  Citations within documentation blocks
are not counted, since those blocks do not export their contents."
  (setq org--docs nil org--docs-actually-used nil)
  ;; Parsing the whole buffer is dear, so we only do it when there may be entries or citations.
  (when (save-excursion (goto-char (point-min)) (re-search-forward "doc:\\|documentation" nil t))
    (let ((tree (org-element-parse-buffer)))
      (setq org--docs (org--docs-read-buffer tree))
      (org-element-map tree 'link
        (lambda (link)
          (let ((label (org-element-property :path link))
                (block (org-element-lineage link '(special-block))))
            (when (and (equal (org-element-property :type link) "doc")
                       (not (and block (equal "documentation" (downcase (org-element-property :type block))))))
              ;; An unknown label is reported by the export itself, so we skip it here.
              (-when-let (entry (ignore-errors (org-docs-get label)))
                (add-to-list 'org--docs-actually-used (cons label entry))))))))))

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
              ('html (org--html-tooltip docs display-name (org--docs-plain-p o-label)))
              ;; Make the current word refer to its glossary entry, and declare the location
              ;; that the glossary should refer back to; see the GLOSSARY of `org-link/show'.
              ('latex (format "\\hyperref[o-glossary-%s]{%s}\\label{o-glossary-declaration-site-%s}"
                              o-label display-name o-label))
              (_ display-name))))

;; WHERE ...

(defun org--latex-glossary-text (text &optional plain?)
  "(TEXT, PLAIN?) ↦ LaTeX of a glossary entry's TEXT: its body-only export as Org; or, when PLAIN?, such as a docstring,
TEXT in a verbatim environment, as written ---so its #, $, %, and the like need no escaping."
  (if plain?
      (format "\n\\begin{verbatim}\n%s\n\\end{verbatim}\n" (string-trim text))
    (let ((org-export-before-parsing-functions (remq #'org--docs-prepare org-export-before-parsing-functions)))
      (string-trim (org-export-string-as text 'latex t)))))

(defun org--docs-plain-p (label)
  "LABEL ↦ non-nil when its glossary entry is plain text ---an Elisp docstring, or a WordNet definition--- not Org.
Entries of the buffer, or of `org-docs-libraries', are Org; see `org-docs-get'."
  (not (or (assoc label org--docs) (assoc label org--docs-from-libraries))))

;;;;;;; Documentation

(org-defblock documentation
              (name (error "Documentation block: Name must be provided")
                    label nil show nil color "green"
                    ;; A heading tagged :documentation: is an entry in its own right; see `org--docs-read-buffer'.
                    :tag nil)
              "A glossary entry: CONTENTS defines NAME, cited as doc:LABEL; a heading tagged :documentation: is one too.

The entries live in ‘org--docs’.

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

Alternatively, a heading tagged :documentation: is an entry, which
exports as an ordinary section: its title is the NAME, its :DOC_KEYS:
property holds the labels, and its own text is the entry.

  * Category Theory                                  :documentation:
    :PROPERTIES:
    :DOC_KEYS: cat cats
    :END:
    A theory of typed composition; e.g., typed monoids.

Entries are read afresh before each export, so they may be cited before
they are defined; see `org--docs-prepare'."
              (dolist (entry (org--docs-entries name label (substring-no-properties raw-contents)))
                (add-to-list 'org--docs entry))
              ;; Should the special block show something upon export?
              (if show (org--blockcall box name :background-color color raw-contents) ""))

;;;;;; Package docs ---a table of what a package defines

(defun org--package-docs (feature)
  "FEATURE ↦ ((NAME KIND GIST) …) for its public definitions, alphabetically; public meaning no “--” in NAME.
KIND is one of macro, command, function, user option, or variable; GIST is the docstring's first line, else nil.
  \\='org-deflink  ↦  ((org-deflink \"macro\" \"Make a new Org-link NAME that exports using form BODY.\") …)"
  (require feature)
  (let ((entries (cdr (--first (member (cons 'provide feature) (cdr it)) load-history)))
        (gist (lambda (docs) (-some--> docs (car (split-string it "\n")) (unless (string-empty-p it) it)))))
    (thread-last
      (cl-loop for entry in entries
               for name = (if (symbolp entry) entry (and (eq (car-safe entry) 'defun) (cdr entry)))
               when (and name (not (string-match-p "--" (symbol-name name))))
               collect (if (symbolp entry)
                           (list name (if (custom-variable-p name) "user option" "variable")
                                 (funcall gist (documentation-property name 'variable-documentation t)))
                         (list name (cond ((macrop name) "macro") ((commandp name) "command") (t "function"))
                               (funcall gist (ignore-errors (documentation name t))))))
      (-uniq)
      (--sort (string< (car it) (car other))))))

(org-defblock package-docs (feature nil :link nil :tag nil)
  "A table of the public functions, macros, and variables that FEATURE defines; each documented name is a doc: link,
so that its full documentation is a tooltip.

  #+begin_package-docs org-deflink
  #+end_package-docs"
  (org-parse
   (concat "| Name | Kind | Gist |\n|-\n"
           (mapconcat (-lambda ((name kind gist))
                        (format "| %s | %s | %s |"
                                (if gist (format "[[doc:%s][~%s~]]" name name) (format "~%s~" name))
                                kind
                                ;; A ‘|’ would end the cell early.
                                (replace-regexp-in-string "|" "\\\\vert{}" (or gist "") t t)))
                      (org--package-docs (intern feature)) "\n"))))

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
                           "\n\nEach export gathers the citations afresh, from the doc: links of its buffer.")
                 o-label))
  :help-echo (format (concat "Upon export, the following will be placed literally"
                             "\n\t%s"
                             "\nWhich is the value of the expression:\n\t%s")
                     o-value o-expr)
  :export (cond ((not glossary?) o-value)
                ;; Otherwise O-LABEL is glossary, which we print in HTML & LaTeX
                ((equal 'html o-backend)
                 (s-join " "
                         (--map (org--html-tooltip (cl-third it) (cl-second it) (org--docs-plain-p (cl-first it)))
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
                                            (when doc (org--latex-glossary-text doc (org--docs-plain-p label)))
                                            label))))))

(provide 'org-links)
;;; org-links.el ends here
