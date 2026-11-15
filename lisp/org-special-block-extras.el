;;; org-special-block-extras.el --- Org's *bold*, [[links]], red:text work in details, graph, math  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8") (lf "26.10.1") (seq "2.0"))
;; Keywords: org, blocks, colors, convenience
;; URL: https://alhassy.com/org-special-block-extras


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org markup all the way down: details, graph, math, callouts, and more.  Graphs from lists, folds from blocks,
;; colors from red:links.  We offer folds, graphs, maths, callouts, colors, tooltips, and badges; and
;; `org-defblock', a `cl-defun' for making our own.
;;
;; Some text is tangential: a run of import statements, the gory steps of a proof, a worked solution to an
;; exercise.  We want it folded away until the reader asks for it; so we declare a block that does so, as we would
;; a Lisp function:
;;
;;     (org-defblock details (&optional (title "Details") &context backend contents)
;;       "Fold CONTENTS away beneath TITLE, until the reader clicks it."
;;       (pcase backend
;;         ('html (format "<details><summary>%s</summary>%s</details>" title contents))
;;         (_     (format "\\begin{quote}\\textbf{%s}\n%s\\end{quote}" title contents))))
;;
;; Now we write
;;
;;     #+begin_details Why does this terminate?
;;     Each step *shrinks* the list, and no list is shorter than the empty one.
;;     #+end_details
;;
;; and HTML folds it away, while LaTeX, sans clicks, sets it apart.  CONTENTS arrives already exported, which is
;; why the Org within ---*bold*, [[links]], red:text--- works as it does anywhere.  Moreover, a section tagged
;; `:details_Hint:' folds its text away under the title Hint.  Our own `details' is just this, plus colors.
;; Atop that one idea, we provide:
;;
;; - Definers: `org-defblock', `org-deflink', and `org-deftag', each declared as a `cl-defun' is.
;; - Switches that every block understands ---ours, yours, or Org's own src and quote blocks---:
;;   `:folded', `:when', `:let', `:boxed', `:demo', and more; see `org-block-switches'.
;; - Blocks: `details', `box', callouts such as `note' and `warning', `parallel' columns, `margin' notes,
;;   `spoiler', `solution', `remark', `quote', `image', proof trees, and `math' with calculational proofs.
;; - Links: colors such as `red:', tooltips by `doc:', `kbd:', `badge:', and permalinks for Org's <<targets>>.
;; - Editing, as for src blocks: C-c C-c runs a block, C-c ' edits it, C-c C-v C-j inserts an argument,
;;   C-c C-v C-d splits it; and `:fontify' says how it looks in the buffer.
;; - Graphs: `#+begin_graph' draws a nested Org list as a Graphviz diagram.  Graphviz is a program of its own,
;;   so the graph block is opt-in, by (require 'org-graph-block); see graphs-README.org.
;;
;; The manual, with every block demonstrated, is https://alhassy.com/org-special-block-extras

;;; Design:

;; Every block is a function from its text to its export; the rest is plumbing that finds blocks and calls it.
;;
;; 1. Declaring.  `org-defblock' turns its body into a handler,
;;
;;        org--NAME-block : backend × contents × main-arg × kwdargs → string
;;
;;    and adds NAME to `org--supported-blocks'.  Unless `:link nil' or `:tag nil' says otherwise, it also
;;    declares the link NAME: and the tag :NAME:, which call that same handler.
;;
;; 2. Exporting.  `org-special-block-extras-mode' hooks `org--rewrite-special-blocks-by-handlers' onto
;;    `org-export-before-parsing-functions': before Org parses the buffer, each block of ours ---read into an
;;    `org-special-block' by `org-special-block-after-point'--- is replaced by its handler's result, which is
;;    rescanned, as it may hold blocks too.  Tags run earlier, so a tagged section is a block by then.
;;
;; 3. Switching.  `org--block-with-switches' wraps every handler's call: a switch rewrites the call, or its
;;    result, whatever the block.  `org-defswitch' declares more; the mode's hooks apply them to Org's own blocks
;;    and, by property drawers, to sections.
;;
;; 4. Heads.  A block's `:latex-head' and `:html-head' reach the document's preamble once, and only if it is used;
;;    see `org--load-latex-head' and `org--load-html-head'.
;;
;; As for code, the definers and their plumbing live in files of their own name, such as org-defblock.el and
;; org-block-switches.el; each family of blocks lives in one file, such as org-callout-blocks.el; and this file
;; requires them all and defines the mode.

;;; Code:

(require 'org-defblock)
(require 'org-thread-blockcall)
(require 'org-links)
(require 'org-color-links)
(require 'org-details-block)
(require 'org-box-block)
(require 'org-callout-blocks)
(require 'org-parallel-block)
(require 'org-margin-blocks)
(require 'org-math-block)
(require 'org-quote-block)
(require 'org-solution-block)
(require 'org-demo)
(require 'org-display-switches)
(require 'org-image-block)
(require 'org-substitution-blocks)
(require 'org-spoiler-block)
(require 'org-remark-block)
(require 'org-proof-block)
(require 'org-deftag)

;;;;; Version

(defconst org-special-block-extras-version (package-get-version))
(defun org-special-block-extras-version ()
  "Print the current version of the package in the minibuffer."
  (interactive)
  (message org-special-block-extras-version))

(defcustom org-special-block-add-html-extra t
  "Whether an HTML page using <kbd> or tooltips gets our CSS and JS for them in its head; see `org--load-html-head'.
They are inlined ---see tooltips.css and tooltips.js--- so an exported page loads nothing from elsewhere;
set this to nil for pages sans them, whose tooltips then never open."
  :type 'boolean
  :group 'org-special-block-extras)

;;;;; HTML head extras

(defconst org--kbd-html-head
  "
          <style>
          /* From: https://endlessparentheses.com/public/css/endless.css */
          /* See also: https://meta.superuser.com/questions/4788/css-for-the-new-kbd-style */
          kbd
          {
            -moz-border-radius: 6px;
            -moz-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            -webkit-border-radius: 6px;
            -webkit-box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            background-color: #f7f7f7;
            border: 1px solid #ccc;
            border-radius: 6px;
            box-shadow: 0 1px 0 rgba(0,0,0,0.2),0 0 0 2px #fff inset;
            color: #333;
            display: inline-block;
            font-family: 'Droid Sans Mono', monospace;
            font-size: 80%;
            font-weight: normal;
            line-height: inherit;
            margin: 0 .1em;
            padding: .08em .4em;
            text-shadow: 0 1px 0 #fff;
            word-spacing: -4px;

            box-shadow: 2px 2px 2px #222; /* MA: An extra I've added. */
          }
          </style>"
  "Styling for <kbd> elements, added to a page that uses <kbd>, by `org--load-html-head'.")

(defconst org--tooltip-files-directory (file-name-directory (or load-file-name buffer-file-name))
  "The directory holding this library, and so, when the package archive ships them, tooltips.css and tooltips.js.")

(defcustom org-special-block-tooltip-cache-directory
  (locate-user-emacs-file "org-special-block-extras/")
  "Where tooltips.css and tooltips.js are kept when fetched, once, since the package archive lacks them; issue #62."
  :type 'directory
  :group 'org-special-block-extras)

(defun org--tooltip-file (file)
  "Returns FILE's contents: ours, else a copy fetched once, by consent, into `org-special-block-tooltip-cache-directory'.
Declining warns that tooltips will not open, and yields \"\"; we ask anew next session."
  ;; TODO: Axe this fetch once MELPA's recipe ships the files: :files (:defaults "lisp/tooltips.*"); issue #62.
  (let ((ours (expand-file-name file org--tooltip-files-directory))
        (cached (expand-file-name file org-special-block-tooltip-cache-directory))
        (url (format "https://raw.githubusercontent.com/alhassy/org-special-block-extras/master/lisp/%s" file)))
    (cl-flet ((slurp (path) (with-temp-buffer (insert-file-contents path) (buffer-string))))
      (cond ((file-readable-p ours) (slurp ours))
            ((file-readable-p cached) (slurp cached))
            ((and (not noninteractive)
                  (y-or-n-p (format "Tooltips need %s, which your package lacks; fetch it, once, from %s? " file url)))
             (make-directory (file-name-directory cached) t)
             (url-copy-file url cached)
             (slurp cached))
            (t (display-warning 'org-special-block-extras (format "%s is missing, so tooltips will not open" file))
               "")))))

(defvar org--tooltip-html-head-cache nil
  "The value of `org--tooltip-html-head' this session, so we read, or ask to fetch, the files once.")

(defun org--tooltip-html-head ()
  "Returns tooltips.css and tooltips.js, inlined, for “.tooltip” elements; added to a page with tooltips by `org--load-html-head'.
Inlined, so an exported page makes no request for them; see https://github.com/alhassy/org-special-block-extras/issues/53.
Read when first needed, at export, so a missing file is asked for then ---not whenever the package loads."
  (or org--tooltip-html-head-cache
      (setq org--tooltip-html-head-cache
            (format "\n<style>\n%s</style>\n<script>\n%s</script>\n"
                    (org--tooltip-file "tooltips.css") (org--tooltip-file "tooltips.js")))))

;;;;; Glossary libraries

(defvar org--docs-libraries-loaded nil
  "Whether `org-special-block-extras-mode' has loaded `org-docs-libraries' this session.")

;;;;; Eldoc: hover documentation for blocks and links

(defun org-block-signature (name)
  "NAME ↦ how its block is called, with defaults; e.g., #+begin_details TITLE=\"Details\" :title-color \"green\"; else nil."
  (-when-let (args (get (intern-soft (format "org--%s-block" name)) 'org-defblock-args))
    (-let [(&plist :main (main default) :keys keys) args]
      (concat (format "#+begin_%s" name)
              ;; A block sans main argument shows none.
              (if main (format " %s" (upcase (symbol-name main))) "")
              (if default (format "=%S" default) "")
              (mapconcat (-lambda ((key default)) (format " :%s%s" key (if default (format " %S" default) "")))
                         keys "")))))

(defun org--block-keys (name)
  "NAME ↦ the keys that act on a NAME block, each beside the gist of what it does, as `org-def-action' and
`org-defblock''s :edit-indirect-mode declare; e.g., for math,
  (\"C-c C-c  Show the maths as an image atop the block; again, to remove it.  See …\"
   \"C-c '    Edit the block in `latex-mode'; C-c ' again writes it back.\")"
  ;; Raw, lest C-c ' read as C-c ’.
  (let ((gist (lambda (f) (car (split-string (or (documentation f t) "") "\n")))))
    (-non-nil
     (list (-some--> (intern-soft (format "org--%s-action" name))
             (and (fboundp it) (concat "C-c C-c  " (funcall gist it))))
           (concat "C-c '    " (-if-let (edit (alist-get name org--block-edit-special-handlers nil nil #'equal))
                                   (funcall gist edit)
                                 ;; Every special block is so edited, by `org-special-block-edit'.
                                 "Edit the block in `org-mode'; C-c ' again writes it back."))
           ;; As for a src block; see `org-special-block-split' and `org-special-block-insert-arg'.
           "C-c C-v C-d  Split the block in two at point; around the region, in three."
           "C-c C-v C-j  Insert an argument, or a switch, completing its name, on the #+begin_ line."))))

(defun org--block-switch-lines (name)
  "Returns a heading, then a line per switch of `org-block-switches' that block NAME does not shadow by a keyword of
its own, each beside the gist of what it does; e.g., \":folded TITLE      The result within a `details' titled …\"."
  (let ((own (--map (intern (format ":%s" (car it)))
                    (plist-get (get (intern-soft (format "org--%s-block" name)) 'org-defblock-args) :keys))))
    (cons "Switches, which every block understands:"
          (cl-loop for (key _ . switch) in org-block-switches
                   for arg = (symbol-name (car (help-function-arglist switch t)))
                   unless (memq key own)
                   collect (format "  %-17s %s" (format "%s %s" key (if (equal arg "_") "t" (upcase (string-remove-prefix "_" arg))))
                                   (car (split-string (documentation switch t) "\n")))))))

(defcustom org-special-block-extras-manual "https://alhassy.com/org-special-block-extras-blocks-and-links"
  "The manual of our blocks and links, whose sections a block's, or link's, `:manual' of #ANCHOR names; see
`org--hover-footer'."
  :type 'string)

(defun org--hover-footer (symbol)
  "SYMBOL ↦ the buttons that close eldoc's word on it: “Docs”, describing SYMBOL; and, given its `:manual', “See the
manual”, browsing there.  E.g., for `org--math-block', “Docs · See the manual”, the latter leading to
https://alhassy.com/org-special-block-extras-blocks-and-links#math."
  (let ((manual (-some--> (get symbol 'org-manual)
                  (if (string-prefix-p "#" it) (concat org-special-block-extras-manual it) it)))
        (button (lambda (label action help) (make-text-button (copy-sequence label) nil
                                                               'action action 'follow-link t 'help-echo help))))
    (concat (funcall button "Docs" (lambda (_) (describe-function symbol)) (format "Describe `%s'" symbol))
            (when manual (concat " · " (funcall button "See the manual" (lambda (_) (browse-url manual)) manual))))))

(defun org--fontify-as-org (text)
  "Returns TEXT with the faces an Org buffer gives it, sans the invisibility and display strings that hide its syntax;
its “# …” lines are comments even within a block, where Org leaves them unfontified, since they explain the rest."
  (with-temp-buffer
    (let ((org-inhibit-startup t)) (org-mode))
    (insert text)
    (font-lock-ensure)
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*# .*" nil t)
      (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-comment-face))
    (let ((plain (buffer-substring-no-properties (point-min) (point-max))))
      (cl-loop for pos = (point-min) then next while (< pos (point-max))
               for next = (next-single-property-change pos 'face nil (point-max))
               for face = (get-text-property pos 'face)
               when face do (put-text-property (1- pos) (1- next) 'face face plain))
      plain)))

(defun org--hover-example (symbol)
  "Returns the `:example' of SYMBOL's block, fontified as Org, its #-comments in `font-lock-comment-face'; else nil.
Each is fontified once, until its text changes; see `org-defblock-attribute'."
  (-when-let (example (-some--> (get symbol 'org-example) (string-trim it)))
    (let ((cached (get symbol 'org-example-fontified)))
      (if (equal (car cached) example) (cdr cached)
        (cdr (put symbol 'org-example-fontified (cons example (org--fontify-as-org example))))))))

(defun org--special-block-eldoc (callback &rest _)
  "Hand eldoc's CALLBACK the signature and gist of the block, or link, at point, and more; else nil.
On a #+begin_NAME or #+end_NAME line, that is the NAME block; on a NAME: link, the NAME block, else its `org-deflink'.
The gist is the first line, which the echo area shows; for a multi-line display, such as eldoc-box, there follow its
`:hover' prose, its `:example', the keys that act on it, the switches it understands, and its “Docs · See the
manual” buttons; see `org--hover-footer'."
  (when (derived-mode-p 'org-mode)
    (let* ((delimiter? (save-excursion (beginning-of-line) (looking-at-p "[ \t]*#\\+\\(begin\\|end\\)_")))
           (name  (if delimiter? (org-special-block-name-at-point)
                    (-some->> (org--link-at-point) (org-element-property :type))))
           (block (and name (intern-soft (format "org--%s-block" name))))
           (link  (and name (not delimiter?) (intern-soft (format "org--%s-link" name))))
           (gist  (lambda (docstring) (car (split-string (or docstring "") "\n"))))
           (hover (lambda (symbol gist keys)
                    (string-join (-non-nil (append (list gist (get symbol 'org-hover)
                                                         ;; Set apart by blank lines, as a code block in prose is.
                                                         (-some--> (org--hover-example symbol) (concat "\n" it "\n")))
                                                   keys
                                                   (list (org--hover-footer symbol))))
                                 "\n"))))
      (cond ((get block 'org-defblock-args)
             (funcall callback (funcall hover block (funcall gist (get block 'variable-documentation))
                                        (and delimiter? (append (org--block-keys name) (org--block-switch-lines name))))
                      :thing (org-block-signature name) :face 'font-lock-function-name-face))
            ((fboundp link)
             (funcall callback (funcall hover link (funcall gist (documentation link t))
                                        (and (get link 'org-deflink-follows) '("C-c C-o  Follow it.")))
                      :thing (format "%s:" name) :face 'font-lock-function-name-face))))))

;;;;; Completing #+begin_ lines

(defconst org--org-block-names '("src" "example" "export" "quote" "verse" "center" "comment")
  "Org's own block names, offered beside ours so completing #+begin_s still reaches src.")

(defun org--block-gist (name)
  "NAME ↦ the first line of the docstring of our block NAME; e.g., \"tip\" ↦ \"Helpful advice for doing things better…\"."
  (-some--> (intern-soft (format "org--%s-block" name)) (get it 'variable-documentation) (car (split-string it "\n"))))

(defun org--special-block-capf ()
  "On a #+begin_ line, complete the block's name, each of ours annotated by its gist; e.g., #+begin_ti ↦ tip, tooltip.
A doc buffer, for a completion popup's documentation pane, holds the block's signature and whole docstring."
  (let ((case-fold-search t))
    (when (looking-back "^[ \t]*#\\+begin_\\([^ \t\n]*\\)" (line-beginning-position))
      (list (match-beginning 1) (match-end 1)
            ;; Once each: quote is both Org's and, given a citation, ours.
            (delete-dups (append (mapcar (lambda (b) (format "%s" b)) org--supported-blocks) org--org-block-names))
            :exclusive 'no
            :annotation-function (lambda (name) (concat "  " (or (org--block-gist name) "Org's own")))
            :company-kind (lambda (_) 'keyword)
            :company-doc-buffer
            (lambda (name)
              (-when-let (doc (-some--> (intern-soft (format "org--%s-block" name)) (get it 'variable-documentation)))
                (with-current-buffer (get-buffer-create " *org-block-doc*")
                  (erase-buffer)
                  (insert (org-block-signature name) "\n\n" doc)
                  (current-buffer))))))))

(defun org--special-block-capf-setup ()
  "In this Org buffer, complete #+begin_ lines before Org's own pcomplete does; and show links in full while point is in them."
  (add-hook 'completion-at-point-functions #'org--special-block-capf -50 t)
  (add-hook 'post-command-hook #'org--link-refresh nil t))

;;;;; The mode

;;;###autoload
(define-minor-mode org-special-block-extras-mode
  "Toggle the blocks, links, and tags of `org-defblock', and every block's switches, in Org.

All relevant Lisp functions are prefixed ‘org-’; e.g., `org-docs-insert'.

On a #+begin_NAME line, or a NAME: link, eldoc shows the block's arguments and gist;
C-c ' edits any special block's contents in an Org buffer, or one of its `:edit-indirect-mode';
C-c C-v C-d splits any special block in two at point; C-c C-v C-j inserts one of its arguments;
and a heading tagged :red:, or :details_Hint:, exports as that block around its text.

HTML exports carry our tooltips' CSS and JS, inlined; see `org-special-block-add-html-extra'.
"
  :global t
  :group 'org-special-block-extras
  :lighter " OSBE"
  (if org-special-block-extras-mode
      (progn
        ;; https://orgmode.org/manual/Advanced-Export-Configuration.html
        (add-hook 'org-export-before-parsing-functions #'org--rewrite-special-blocks-by-handlers)
        ;; [[NAME]] cites a math block's tag NAME: while the tags are still to be read, ere the blocks are rewritten.
        (add-hook 'org-export-before-parsing-functions #'org--math-cite -20)
        (add-hook 'org-execute-file-search-functions #'org--math-ref-follow)
        ;; Switches on any block not of ours ---src, quote, any #+begin_𝒳---, before our own blocks are rewritten;
        ;; and before Babel too, so a src block's :let reaches the code it runs, and its :noexport stops it running.
        ;; The second pass serves the blocks of #+INCLUDE: files, which expand between the two.
        (add-hook 'org-export-before-processing-functions #'org--rewrite-any-block-switches)
        (add-hook 'org-export-before-parsing-functions #'org--rewrite-any-block-switches -10)
        ;; Switches on sections, by property drawer, ere those of the blocks within: a heading's :let reaches them.
        (add-hook 'org-export-before-processing-functions #'org--rewrite-heading-switches -5)
        (add-hook 'org-export-before-parsing-functions #'org--rewrite-heading-switches -15)
        ;; Demos before all else: each renders its block as written, in a document of its own; see `org-demo'.
        (add-hook 'org-export-before-processing-functions #'org--rewrite-demos -20)
        (add-hook 'org-export-before-parsing-functions #'org--rewrite-demos -70)
        (add-hook 'org-export-before-processing-functions #'org--html-export-style-setup)
        ;; A block's :highlight lights its lines; a [[(name)]] link lights its coderef's line, on hover.
        (advice-add 'org-html-src-block :around #'org--highlight-src-block)
        (advice-add 'org-html-example-block :around #'org--highlight-src-block)
        (advice-add 'org-export-format-code :around #'org--highlight-code-line)
        (add-hook 'org-export-filter-final-output-functions #'org--coderef-script)
        (advice-add 'org-html-footnote-reference :around #'org--footnote-as-tooltip)
        ;; In HTML, <<X>> is a permalink #X to itself; <<<X>>> keeps Org's look, yet is #X too.
        (advice-add 'org-html--reference :around #'org--target-readable-reference)
        (advice-add 'org-html-target :override #'org--target-as-permalink)
        ;; Each mention of a <<<X>>> is a tooltip of X's paragraph, its glossary entry.
        (advice-add 'org-html-link :around #'org--radio-link-as-tooltip)
        ;; #+print_glossary: lists the page's citations, as #+print_bibliography: its works.
        (advice-add 'org-html-keyword :around #'org--docs-glossary)
        (advice-add 'org-latex-keyword :around #'org--docs-glossary)
        (add-hook 'org-export-filter-options-functions #'org--footnotes-in-margin)
        (add-hook 'org-export-filter-final-output-functions #'org--load-latex-head)
        (add-hook 'org-export-filter-final-output-functions #'org--load-html-head)
        (add-hook 'org-export-filter-final-output-functions #'org--resolve-section-links)
        (advice-add 'org-fontify-meta-lines-and-blocks :around #'osbe--fontify-blocks-as-org)
        (advice-add 'org-remove-font-lock-display-properties :after #'org--undisplay)
        ;; C-c C-x C-l, and #+startup: latexpreview, show our math blocks as Org's own maths.
        (advice-add 'org--latex-preview-region :after #'org--math-preview-region)
        (advice-add 'org-latex-latex-fragment :around #'org--math-inline)
        (advice-add 'org-html-latex-fragment :around #'org--math-inline)
        ;; M-q within a math block fills its hint at point alone, never its maths.
        (advice-add 'org-fill-paragraph :before-until #'org--math-fill-hint)
        ;; org-cdlatex-mode reads a math block's contents as maths, as it reads $…$.
        (advice-add 'org-inside-LaTeX-fragment-p :around #'org--math-inside-fragment)
        (advice-add 'texmathp :around #'org--math-texmathp)
        ;; TAB on a hint folds it to ={…}, as TAB folds a heading.
        (add-hook 'org-tab-first-hook #'org--math-fold-hint)
        ;; In C-c ' buffers too; and eldoc tells of a hint at point.
        (add-hook 'org-special-block-edit-functions #'org--math-edit-setup)
        (add-hook 'eldoc-documentation-functions #'org--math-hint-eldoc)
        ;; C-TAB within a math block folds, or unfolds, all its hints; elsewhere, Org leaves it unbound, so it is as ever.
        (define-key org-mode-map (kbd "C-<tab>")
                    `(menu-item "" org--math-cycle-hints :filter ,(lambda (command) (and (org--math-block-contents-start) command))))
        ;; Appended, so a block's :fontify face lands atop Org's own fontification of its contents.
        (font-lock-add-keywords 'org-mode '((org--fontify-blocks)) 'append)
        ;; Prepended, so Org's own fontification lands atop a block's :language.
        (font-lock-add-keywords 'org-mode '((org--fontify-block-languages)))
        ;; Appended, so the link point is in shows in full after Org hides its brackets.
        (font-lock-add-keywords 'org-mode '((org--reveal-link-at-point)) 'append)
        ;; Before the rewrite, which consumes the documentation blocks this reads.
        (add-hook 'org-export-before-parsing-functions #'org--docs-prepare -60)
        (add-hook 'eldoc-documentation-functions #'org--special-block-eldoc)
        (add-hook 'org-mode-hook #'org--special-block-capf-setup)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer (when (derived-mode-p 'org-mode) (org--special-block-capf-setup))))
        ;; Ensure user's documentation libraries have loaded
        (unless org--docs-libraries-loaded
          (setq org--docs-libraries-loaded t)
          (org-docs-load-libraries)))
    (remove-hook 'org-export-before-parsing-functions #'org--rewrite-special-blocks-by-handlers)
    (remove-hook 'org-export-before-parsing-functions #'org--math-cite)
    (remove-hook 'org-execute-file-search-functions #'org--math-ref-follow)
    (remove-hook 'org-export-before-processing-functions #'org--rewrite-any-block-switches)
    (remove-hook 'org-export-before-parsing-functions #'org--rewrite-any-block-switches)
    (remove-hook 'org-export-before-processing-functions #'org--rewrite-heading-switches)
    (remove-hook 'org-export-before-parsing-functions #'org--rewrite-heading-switches)
    (remove-hook 'org-export-before-processing-functions #'org--rewrite-demos)
    (remove-hook 'org-export-before-parsing-functions #'org--rewrite-demos)
    (advice-remove 'org-html-footnote-reference #'org--footnote-as-tooltip)
    (advice-remove 'org-html--reference #'org--target-readable-reference)
    (advice-remove 'org-html-target #'org--target-as-permalink)
    (advice-remove 'org-html-link #'org--radio-link-as-tooltip)
    (advice-remove 'org-html-keyword #'org--docs-glossary)
    (advice-remove 'org-latex-keyword #'org--docs-glossary)
    (remove-hook 'org-export-filter-options-functions #'org--footnotes-in-margin)
    (remove-hook 'org-export-filter-final-output-functions #'org--load-latex-head)
    (remove-hook 'org-export-filter-final-output-functions #'org--load-html-head)
    (remove-hook 'org-export-filter-final-output-functions #'org--resolve-section-links)
    (remove-hook 'org-export-before-processing-functions #'org--html-export-style-setup)
    (advice-remove 'org-html-src-block #'org--highlight-src-block)
    (advice-remove 'org-html-example-block #'org--highlight-src-block)
    (advice-remove 'org-export-format-code #'org--highlight-code-line)
    (remove-hook 'org-export-filter-final-output-functions #'org--coderef-script)
    (advice-remove 'org-fontify-meta-lines-and-blocks #'osbe--fontify-blocks-as-org)
    (advice-remove 'org-remove-font-lock-display-properties #'org--undisplay)
    (advice-remove 'org--latex-preview-region #'org--math-preview-region)
    (advice-remove 'org-latex-latex-fragment #'org--math-inline)
    (advice-remove 'org-html-latex-fragment #'org--math-inline)
    (advice-remove 'org-fill-paragraph #'org--math-fill-hint)
    (advice-remove 'org-inside-LaTeX-fragment-p #'org--math-inside-fragment)
    (advice-remove 'texmathp #'org--math-texmathp)
    (remove-hook 'org-tab-first-hook #'org--math-fold-hint)
    (remove-hook 'org-special-block-edit-functions #'org--math-edit-setup)
    (remove-hook 'eldoc-documentation-functions #'org--math-hint-eldoc)
    (define-key org-mode-map (kbd "C-<tab>") nil)
    (font-lock-remove-keywords 'org-mode '((org--fontify-blocks)))
    (font-lock-remove-keywords 'org-mode '((org--fontify-block-languages)))
    (font-lock-remove-keywords 'org-mode '((org--reveal-link-at-point)))
    (remove-hook 'org-export-before-parsing-functions #'org--docs-prepare)
    (remove-hook 'eldoc-documentation-functions #'org--special-block-eldoc)
    (remove-hook 'org-mode-hook #'org--special-block-capf-setup)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (remove-hook 'completion-at-point-functions #'org--special-block-capf t)
        (remove-hook 'post-command-hook #'org--link-refresh t)))))

;;;;; Exporting sans the mode

(defun org--warn-when-mode-off (&rest _)
  "Before an export with `org-special-block-extras-mode' off, warn of the first of our blocks in the buffer, if any.
Our links are live once the package is loaded, yet our blocks only while the mode is on; so a #+begin_rremark would
otherwise export, silently, as Org's own \\begin{rremark}, while rremark:foo is red.  See issue #59."
  (unless org-special-block-extras-mode
    (let ((case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        ;; A plain #+begin_quote is Org's own, as only a quote with arguments is ours; see `org--quote-block'.
        (when (re-search-forward (format "^[ \t]*#\\+begin_\\(%s\\_>\\|quote[ \t]+[^ \t\n]\\)"
                                         (regexp-opt (--remove (equal it "quote") (--map (format "%s" it) org--supported-blocks))))
                                 nil t)
          (display-warning 'org-special-block-extras
                           (format "#+begin_%s exports as Org's own special block, since `org-special-block-extras-mode' \
is off; turn it on, say by (add-hook 'org-mode-hook #'org-special-block-extras-mode)"
                                   (car (split-string (match-string 1))))))))))

;; Hooked once the package is loaded, as our links are, rather than by the mode it warns about.
(add-hook 'org-export-before-parsing-functions #'org--warn-when-mode-off)

(provide 'org-special-block-extras)

;;; org-special-block-extras.el ends here
