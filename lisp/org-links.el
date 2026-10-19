;;; org-links.el --- Built-in org-deflink link types  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8") (lf "26.10.1") (seq "2.0"))
;; Keywords: org, links, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Built-in link types: melpa, kbd, badge, tweet, reddit, github-*, doc; the
;; documentation block; and the #+html_export_style: and #+print_glossary: keywords.  In HTML, each <<X>> exports
;; as a permalink to itself, and each mention of a <<<X>>> as a tooltip of X's paragraph.
;; Also houses the org-docs-* glossary system and `org--html-tooltip'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;; Links
;;;;;; melpa link

(org-deflink melpa (package)
  "Produce a Melpa badge for PACKAGE, which links to the Melpa page.
We try to get the package's version from a constant “⟨PACKAGE⟩-version” if it exists."
  :face '(:box "purple" :foreground "purple")
  :manual "https://alhassy.com/org-special-block-extras#what-does-it-do"
  :export (format (concat "<a href=\"https://melpa.org/#/%s\">"
                          "<img alt=\"MELPA\" src=\"https://img.shields.io/badge/%s-%s-green?logo=Gnu-Emacs\"></img>"
                          "</a>")
                  package
                  (s-replace "_" "__" (s-replace "-" "--" package)) ;; shields.io conventions
                  (or (ignore-errors (eval (intern (concat package "-version")))) "Melpa")))

;;;;;; #+html_export_style: THEME

(defvar org-html-export-styles
  `((default . "")
 (bigblow . "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup")
    (readtheorg . "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup")
    (rose . "#+HTML_HEAD: <link href=\"https://taopeng.me/org-notes-style/css/notes.css\" rel=\"stylesheet\" type=\"text/css\" />")
    (latexcss . "#+HTML_HEAD: <link rel=\"stylesheet\" href=\"https://latex.now.sh/style.min.css\" />"))
  "An alist of theme-to-setup pairs, symbols-to-strings, that “#+html_export_style: THEME” picks from.

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

(defun org--html-export-style-setup (_backend)
  "Insert the setup of the theme named by the buffer's “#+html_export_style: THEME” keyword, if any.

THEME is one of `org-html-export-styles', or random; it is read at export time, so a
buffer's choice never leaks into another's export."
  (save-excursion
    (goto-char (point-min))
    (when (let ((case-fold-search t)) (re-search-forward "^[ \t]*#\\+html_export_style:[ \t]*\\([^ \t\n]+\\)" nil t))
      (let* ((label (match-string 1))
             (theme (if (equal label "random") (car (seq-random-elt org-html-export-styles)) (intern label))))
        (goto-char (point-min))
        (insert (format "\n %s \n" (cdr (or (assoc theme org-html-export-styles)
                                             (error "Error: Unknown #+html_export_style: %s ∉ %s"
                                                    label (cons "random" (--map (symbol-name (car it)) org-html-export-styles)))))))))))

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
  (org-html-head-require 'org--tooltip-html-head)
  (format "<abbr class=\"tooltip\" tabindex=\"0\"%s>%s<template>%s</template></abbr>"
          (if attributes (concat " " attributes) "") body html))

(defun org--tooltip-template (id text &optional plain?)
  "(ID, TEXT, PLAIN?) ↦ the <template> tooltips.js shows for an element of class ID, as maths and graphs keep none within.
  \"osbe-tip-g1\", \"A /typed/ monoid\"  ↦  <template id=\"osbe-tip-g1\"><p>A <i>typed</i> monoid</p></template>"
  (let ((html (org--tooltip-html text plain?)))
    (org-html-head-require 'org--tooltip-html-head)
    (format "<template id=\"%s\">%s</template>" id html)))

(defun org--tooltip-html (text &optional plain?)
  "(TEXT, PLAIN?) ↦ HTML of a tooltip's TEXT: its body-only export as Org; or, when PLAIN?, TEXT escaped in a <pre>.
The nested export keeps the outer one's glossary, which `org--docs-prepare' would otherwise reset."
  (if plain?
      (format "<pre>%s</pre>" (org-html-encode-plain-text (string-trim text)))
    (let ((org-export-before-parsing-functions (remq #'org--docs-prepare org-export-before-parsing-functions)))
      (string-trim (org-export-string-as text 'html t)))))

(org-deflink kbd (keys &optional description backend)
  "Show keysequence KEYS in a nice grey button-like font, along with a tooltip of its documentation, if any.

Such links do not get folded in [[bracket]] style, and are rendered as buttons within Emacs; clicking one
runs its keys, so readers may try them out.  Keys may run any command, so we first ask, as `elisp' links
do, via `org-link-elisp-confirm-function'.

Moreover, KEYS may use ‘_’ in-lieu of spaces or [[bracket]] link notation.

Examples:
    [[kbd:C-x C-s]]
  ≈ <kbd: C-x C-s>
  ≈ kbd:C-x_C-s"
  :display 'full
  :manual "#nice-keystroke-renditions"
  :let (the-label  (s-trim (s-replace "_" " " keys))
        lisp-func  (ignore-errors (key-binding (kbd the-label)))
        tooltip    (or description (ignore-errors (documentation lisp-func)) "")
        tooltip?   (not (equal tooltip ""))
        style      (if tooltip? "border-color: red" "")
        keystrokes (progn (org-html-head-require 'org--kbd-html-head)
                          (format "<kbd style=\"%s\">%s</kbd>" style the-label)))
  ;; The description is always nil when it comes to deciding the :face.
  :face (list :inherit 'custom-button :box (if tooltip? "red" t))
  :help-echo (format "%s ∷ %s\n%s\n\nClick to run these keys." the-label (or lisp-func "") tooltip)
  :follow (org--kbd-run the-label)
  :export (cond ((equal backend 'latex) (format "\\texttt{%s}" the-label))
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

;;;;;; Links show in full while point is in them

(defvar-local org--link-at-point nil
  "The bounds (START . END) of the link that point was in, after the last command; else nil.")

(defun org--link-bounds ()
  "↦ The bounds (START . END) of the link that point is in; else nil."
  (when (get-text-property (point) 'htmlize-link)
    (let ((link (org-element-context)))
      (when (eq (org-element-type link) 'link)
        (cons (org-element-property :begin link)
              (- (org-element-property :end link) (org-element-property :post-blank link)))))))

(defun org--link-refresh ()
  "Re-fontify the links that point left or entered, so Org hides the one left and `org--reveal-link-at-point' the other."
  (let ((bounds (org--link-bounds)))
    (unless (equal bounds org--link-at-point)
      (let ((left org--link-at-point))
        (setq org--link-at-point bounds)
        (dolist (it (list left bounds))
          (when it (font-lock-flush (car it) (min (cdr it) (point-max)))))))))

(defun org--reveal-link-at-point (limit)
  "Font-lock matcher: show the link point is in, from point to LIMIT, in full; e.g., [[X][Y]] rather than Y.
This always returns nil, so it adds no face itself."
  (-let [(start . end) org--link-at-point]
    (when (and start (< start limit) (< (point) end))
      (with-silent-modifications
        (remove-text-properties start (min end (point-max)) '(invisible nil)))))
  nil)

;;;;;; Targets ---each <<X>> a permalink to itself

(defun org--target-readable-reference (reference datum info &optional named-only)
  "A <<X>>, or <<<X>>>, target's HTML id ↦ X, its whitespace as dashes, rather than Org's random orgNNNNNNN; else
as REFERENCE has DATUM's id, under INFO and NAMED-ONLY.  Links to the target ask the same, so they stay in step."
  (if (memq (org-element-type datum) '(target radio-target))
      (replace-regexp-in-string "[[:space:]\"]+" "-" (org-element-property :value datum))
    (funcall reference datum info named-only)))

(defconst org--permalink-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16 16\" width=\"16\" height=\"16\"><path fill-rule=\"evenodd\" d=\"M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z\"></path></svg>"
  "GitHub's “link” OctoIcon, which each <<X>> permalink shows.")

(defun org--target-as-permalink (target _contents info)
  "<<X>> ↦ <a class=\"anchor\" id=\"X\" href=\"#X\">🔗</a>, a permalink to itself, under INFO; Org's own TARGET is a
bare <a id=\"orgNNNNNNN\"></a>, which a reader can neither see nor click."
  (let ((id (org-html--reference target info)))
    (format "<a class=\"anchor\" id=\"%s\" href=\"#%s\" aria-label=\"Permalink to %s\">%s</a>"
            id id id org--permalink-svg)))

(defvar org--docs) ;; The glossary, below.

(defun org--radio-link-as-tooltip (link-transcoder link desc info)
  "A mention of a <<<X>>> radio target ↦ LINK-TRANSCODER's link to it, under DESC and INFO, wearing X's paragraph as a
tooltip; e.g., “A <<<monoid>>> is a unital semigroup.  ⋯ every monoid ⋯”, the latter hovered, shows the former.  The
glossary entry is `org--docs-read-buffer''s; a mention within the paragraph itself, as any other LINK, is left be."
  (let ((html (funcall link-transcoder link desc info)))
    (if-let* (((equal (org-element-property :type link) "radio"))
              (target (org-export-resolve-radio-link link info))
              ((not (eq (org-element-lineage link 'paragraph) (org-element-lineage target 'paragraph))))
              (entry (assoc (downcase (s-replace " " "_" (s-trim (org-element-property :value target)))) org--docs)))
        (org--html-tooltip (cl-third entry) html)
      html)))

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

     go:key|value|blue||gnu-emacs"
  `(org-deflink ,(intern name) (label &optional _description backend)
     "Export a Shields.io badge, with general Syntax:  badge:key|value|colour|url|logo.
Precise details for each argument are shown in the Emacs tooltip for this badge."
     :display 'full
     :manual "https://alhassy.com/org-special-block-extras#what-does-it-do"
     :let (parts (mapcar #'s-trim (s-split "|" label))
           key (nth 0 parts) value (nth 1 parts) color (nth 2 parts) url (nth 3 parts) logo (nth 4 parts))
     :follow (browse-url (or url label))
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
             ⇒ If ‘url’ is not present, the resulting badge is not a hyperlink;
                for a local link, place Org's own <<key>> beside it.
             ⇒ ‘color’ may be: ‘brightgreen’ or ‘success’,
                               ‘red’         or ‘important’,
                               ‘orange’      or ‘critical’,
                               ‘lightgrey’   or ‘inactive’,
                               ‘blue’        or ‘informational’,
               or ‘green’, ‘yellowgreen’, ‘yellow’, ‘blueviolet’, ‘ff69b4’, etc.
               Consult https://htmlcolorcodes.com/ to see the HEX codes of other colours.
             ⇒ ‘logo’ examples and how they look can be found at
                https://alhassy.com/org-special-block-extras#Example-Badge-Icons

         See also: https://alhassy.com/org-special-block-extras#Common-Project-Badges")
             label key (or value "") (or color "green") (or url "") (or logo ""))
     :export
     (if (equal backend 'latex) ""
       (let ((img ,(if social-shields-name
                       (if social-shields-url
                           `(format ,social-shields-url label)
                         `(format "<img src=\"https://img.shields.io/%s/%s?style=social\">" ,social-shields-name label))
                     ;; An omitted, or blank, colour is green; an omitted logo is no logo at all.
                     '(format "<img src=\"https://img.shields.io/badge/%s-%s-%s%s\">"
                              (url-hexify-string (s-replace "-" "--" key))
                              (url-hexify-string (s-replace "-" "--" (or value "")))
                              (or (org-string-nw-p color) "green")
                              (if (org-string-nw-p logo) (format "?logo=%s" logo) ""))))
             (target ,(if social-shields-name `(format ,social-url label) 'url)))
         (pcase target
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
              ("github/forks" "https://www.github.com/%s/fork"))
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

(defvar org--docs-actually-used nil
  "The (LABEL NAME DESCRIPTION) glossary entries the current export cites, which “#+print_glossary:” lists.")

(defun org--docs-prepare (&rest _)
  "Before each export: reset the glossary to the buffer's own entries, and its citations to the buffer's doc: links.
Citations are gathered up front, so “#+print_glossary:” lists them all in one export, wherever it sits;
nothing carries over from one export, or buffer, to the next.  Citations within documentation blocks
are not counted, since those blocks do not export their contents."
  (setq org--docs nil org--docs-actually-used nil)
  ;; Parsing the whole buffer is dear, so we only do it when there may be entries or citations.
  (when (save-excursion (goto-char (point-min)) (re-search-forward "doc:\\|documentation\\|<<<" nil t))
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

(defun org--docs-glossary (keyword-transcoder keyword contents info)
  "“#+print_glossary:” ↦ the cited entries, `org--docs-actually-used', as #+print_bibliography: lists the cited works:
in LaTeX, each entry in full, labelled for its doc: links, and referring back to where it was cited; in HTML, each
entry's name, its description a tooltip.  Any other KEYWORD is KEYWORD-TRANSCODER's, under CONTENTS and INFO."
  (cond
   ((not (equal (org-element-property :key keyword) "PRINT_GLOSSARY")) (funcall keyword-transcoder keyword contents info))
   ((org-export-derived-backend-p (plist-get info :back-end) 'latex)
    (s-join "\n\n"
            (cl-loop for (label name doc) in org--docs-actually-used
                     collect (format (concat "\\vspace{1em}\\phantomsection\\textbf{%s}\\quad"
                                             "\\label{o-glossary-%s}%s See page \\pageref{o-glossary-declaration-site-%s}")
                                     name label (if doc (org--latex-glossary-text doc (org--docs-plain-p label)) "")
                                     label))))
   (t (s-join " " (--map (org--html-tooltip (cl-third it) (cl-second it) (org--docs-plain-p (cl-first it)))
                         ;; Ignore duplicates; i.e., entries with the same name/title.
                         (cl-remove-duplicates org--docs-actually-used :key #'cl-second :test #'cl-equalp))))))

(org-deflink doc (label &optional description backend)
  "Export LABEL as itself, or as the provided DESCRIPTION,
 along with a tooltip that shows the user's
 documentation-glossary for LABEL and using that entry's name
 when no DESCRIPTION is provided."
  :let (entry (org-docs-get label)
        name (cl-first entry)
        docs (cl-second entry)
        display-name (or description name))
  :help-echo (format "[%s] %s :: %s" label name docs)
  :face '(custom-button)
  :manual "#Glossaries"
  :export (progn
            (add-to-list 'org--docs-actually-used (list label name docs))
            (pcase backend
              ('html (org--html-tooltip docs display-name (org--docs-plain-p label)))
              ;; Make the current word refer to its glossary entry, and declare the location
              ;; that the glossary should refer back to; see `org--docs-glossary'.
              ('latex (format "\\hyperref[o-glossary-%s]{%s}\\label{o-glossary-declaration-site-%s}"
                              label display-name label))
              (_ display-name))))

;; WHERE ...

(defun org--latex-glossary-text (text &optional plain?)
  "(TEXT, PLAIN?) ↦ LaTeX of a glossary entry's TEXT: its body-only export as Org; or, when PLAIN?, such as a docstring,
TEXT in a verbatim environment, as written ---so its #, $, %, and the like need no escaping; on beamer's slides, whose
frames break on verbatim text, TEXT with those characters escaped."
  (cond
   ;; A beamer frame breaks on verbatim text, which Org marks it fragile for only when it sees a src block or the like.
   ((and plain? (org-export-beamer-p))
    (-->
     (s-replace-all '(("\\" . "\\textbackslash{}") ("{" . "\\{") ("}" . "\\}") ("#" . "\\#") ("$" . "\\$")
                      ("%" . "\\%") ("&" . "\\&") ("_" . "\\_") ("^" . "\\^{}") ("~" . "\\~{}"))
                    (string-trim text))
     ;; A blank line ends a paragraph; any other newline breaks the line.
     (replace-regexp-in-string "\n[ \t]*\n\\(?:[ \t]*\n\\)*" "\\par " it t t)
     (replace-regexp-in-string "\n" "\\newline " it t t)))
   (plain?
      (format "\n\\begin{verbatim}\n%s\n\\end{verbatim}\n" (string-trim text)))
   (t
    (let ((org-export-before-parsing-functions (remq #'org--docs-prepare org-export-before-parsing-functions)))
      (string-trim (org-export-string-as text 'latex t))))))

(defun org--docs-plain-p (label)
  "LABEL ↦ non-nil when its glossary entry is plain text ---an Elisp docstring, or a WordNet definition--- not Org.
Entries of the buffer, or of `org-docs-libraries', are Org; see `org-docs-get'."
  (not (or (assoc label org--docs) (assoc label org--docs-from-libraries))))

;;;;;;; Documentation

(org-defblock documentation
              (&optional (name (error "Documentation block: Name must be provided")) &key label show (color "green")
               &allow-other-keys
               ;; A heading tagged :documentation: is an entry in its own right; see `org--docs-read-buffer'.
               :tag nil :manual "#Glossaries" &context raw-contents)
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

(provide 'org-links)
;;; org-links.el ends here
