;;; org-display-switches.el --- Switches for how a block is shown: :filename, :copy, :tab, :highlight, :log  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.11.14
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Switches, as `org-defswitch' makes them, for how a block looks ---or, by :log, what it transpiles to:
;;
;;     #+begin_src emacs-lisp :tangle init.el :filename t :highlight "2"   ;; Titled init.el, its 2nd line lit
;;     #+begin_src haskell :tab t   …   #+begin_src python :tab t           ;; One tabset of two languages
;;
;; Every src and example block has :copy t by default, a button copying its text in HTML; see
;; `org-babel-default-header-args:src'.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'dash)
(require 's)
(require 'org)
(require 'ox)
(require 'org-block-switches)
(require 'org-block-export)
(require 'org-defblock)
(require 'org-demo)

;;;; :filename

(org-defswitch :filename (file &key render contents backend)
  "The result, beneath FILE, its file name; for a src block's :filename t, the file it tangles to.
In HTML, a tab atop the block; in LaTeX, a small typewriter line; in Markdown, a line of code."
  :depth -40
  (let ((name (org--export-inline (format "~%s~" file) backend))
        (block (funcall render contents)))
    (cond ((org-export-markdown-p) (format "~%s~\n%s" file block))
          ((eq backend 'html)
           (format "#+begin_export html\n<div class=\"osbe-filename\" style=\"%s\">%s</div>\n#+end_export\n%s"
                   (concat "display: inline-block; font-size: 0.85em; padding: 0.1em 0.8em; margin-bottom: -0.6em;"
                           " border-radius: 6px 6px 0 0; background-color: #f0f0f0; border: 1px solid #ddd;")
                   name block))
          ((eq backend 'latex) (format "#+begin_export latex\n\\noindent{\\small %s}\\par\\nopagebreak\n#+end_export\n%s"
                                       name block))
          (t block))))

;;;; :copy

(defvar org-babel-default-header-args:src '((:copy . t))
  "The header arguments every src block inherits, whatever its language; e.g., a copy button.
So #+PROPERTY: header-args:src :copy nil turns the button off, for a whole file.
They are weaker than the language's own, `org-babel-default-header-args:LANG'; see `org--block-header-args'.")

(defvar org-babel-default-header-args:example '((:copy . t))
  "The header arguments every example block inherits; e.g., a copy button, as src blocks have.")

(defconst org--copy-button-script
  (concat "var p = this.parentElement.querySelector('pre') || this.parentElement, c = p.cloneNode(true);"
          " c.querySelectorAll('.linenr').forEach(function (n) { n.remove(); });"
          " var b = this; navigator.clipboard.writeText(c.textContent.replace(/\\n$/, '')).then(function () {"
          " b.textContent = 'Copied!'; setTimeout(function () { b.textContent = 'Copy'; }, 1500); });")
  "The JavaScript a :copy button runs when clicked: it copies its block's text, sans line numbers, and says so.")

(org-defswitch :copy (_ &key render contents backend)
  "The result, with a button, in HTML, that copies its text ---a src block's code, sans line numbers.
Other backends, Markdown included, have no buttons, so they get the block as ever."
  :depth -30
  (if (or (not (eq backend 'html)) (org-export-markdown-p)) (funcall render contents)
    (format (concat "#+begin_export html\n<div class=\"osbe-copy\" style=\"position: relative;\">\n"
                    "<button class=\"osbe-copy-button\" type=\"button\" aria-label=\"Copy to clipboard\" style=\"%s\""
                    " onclick=\"%s\">Copy</button>\n#+end_export\n%s\n#+begin_export html\n</div>\n#+end_export")
            (concat "position: absolute; top: 0.4em; right: 0.4em; z-index: 1; font-size: 0.75em; cursor: pointer;"
                    " padding: 0.1em 0.6em; border-radius: 4px; border: 1px solid #ccc; background: #fafafa; opacity: 0.8;")
            org--copy-button-script
            (funcall render contents))))

;;;; :tab

(defconst org--tab-script
  (concat "<script>(function () { if (window.osbeTabs) return; window.osbeTabs = true;"
          " function tabs () { document.querySelectorAll('.osbe-tab').forEach(function (t) {"
          " if (t.osbeTabbed) return; var group = [t], n = t.nextElementSibling;"
          " while (n && n.classList.contains('osbe-tab')) { group.push(n); n = n.nextElementSibling; }"
          " var bar = document.createElement('div'); bar.className = 'osbe-tabs'; t.parentNode.insertBefore(bar, t);"
          " group.forEach(function (panel, i) { panel.osbeTabbed = true; var title = panel.querySelector('.osbe-tab-title');"
          " var b = document.createElement('button'); b.type = 'button'; b.innerHTML = title.innerHTML; title.style.display = 'none';"
          " b.style.cssText = 'border: none; border-bottom: 2px solid transparent; background: none; padding: 0.3em 0.9em; cursor: pointer; font: inherit;';"
          " b.onclick = function () { group.forEach(function (p, j) { p.style.display = i === j ? '' : 'none';"
          " bar.children[j].style.borderBottomColor = i === j ? 'currentColor' : 'transparent';"
          " bar.children[j].style.fontWeight = i === j ? 'bold' : 'normal'; }); };"
          " bar.appendChild(b); }); bar.children[0].onclick(); }); }"
          " if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', tabs); else tabs(); })();</script>")
  "The JavaScript that gathers each run of adjacent :tab blocks into one tabset; sans it, each is shown titled.")

(org-defswitch :tab (title &key render contents backend)
  "The result as one tab, TITLE, of the tabset that adjacent :tab blocks make; for a src block's :tab t, its language.
  #+begin_src haskell :tab t  …  #+begin_src python :tab t  ⇒  one tabset, Haskell and Python
In HTML, a tabset, by `org--tab-script'; elsewhere, Markdown included, each block beneath its TITLE in bold."
  :depth -65
  (let ((block (funcall render contents))
        (title (format "%s" title)))
    (if (and (eq backend 'html) (not (org-export-markdown-p)))
        (format (concat "#+begin_export html\n<div class=\"osbe-tab\">\n<div class=\"osbe-tab-title\"><strong>%s</strong></div>\n"
                        "%s\n#+end_export\n%s\n#+begin_export html\n</div>\n#+end_export")
                (org--export-inline title backend) org--tab-script block)
      (format "*%s*\n%s" title block))))

;;;; :highlight

(defun org--highlighted-lines (spec)
  "Returns the lines, numbers and coderef labels, SPEC highlights: \"2,4-6\", 3, or a list of them and labels.
  \"2,4-6\"  ⇒  (2 4 5 6);  (1 loop)  ⇒  (1 \"loop\"), the line marked (ref:loop)"
  (--mapcat (cond ((numberp it) (list it))
                  ((and (stringp it) (string-match-p "\\`[0-9, \t-]+\\'" it))
                   (--mapcat (pcase (mapcar #'string-to-number (split-string it "-" t "[ \t]+"))
                               (`(,from ,to) (number-sequence from to))
                               (`(,line) (list line)))
                             (split-string it "," t "[ \t]+")))
                  (t (list (format "%s" it))))
            (ensure-list spec)))

(org-defswitch :highlight (lines &key render contents)
  "The result with LINES highlighted, in HTML: \"2,4-6\", numbers as shown, or as counted from 1 sans -n; or (loop),
coderef labels, the lines marked (ref:loop).  Org's own -n numbers and (ref:) labels work as ever.
Only src and example blocks have lines; see `org--highlight-src-block'."
  :depth 40
  (format "#+attr_osbe: :highlight %s\n%s" (prin1-to-string lines) (funcall render contents)))

(defvar org--highlighted nil
  "While a src or example block exports to HTML, the lines it highlights; see `org--highlighted-lines'.")

(defun org--highlight-src-block (transcode block contents info)
  "Returns TRANSCODE's HTML for BLOCK, under CONTENTS and INFO, with each line of its #+attr_osbe: :highlight lit.
A lit line opens with a bar spanning the block, translucent, so it neither hides nor reflows the code."
  (let* ((spec (plist-get (org-export-read-attribute :attr_osbe block) :highlight))
         (org--highlighted (and spec (org--highlighted-lines (read spec))))
         (html (funcall transcode block contents info)))
    (if org--highlighted (s-replace "<pre class=\"" "<pre style=\"position: relative;\" class=\"" html) html)))

(defun org--highlight-code-line (format-code code fun &optional num-lines ref-alist)
  "Calls FORMAT-CODE on CODE, FUN, NUM-LINES, and REF-ALIST, with each line `org--highlighted' marked, at its start,
by a bar spanning the block; see `org-export-format-code'."
  (if (not org--highlighted) (funcall format-code code fun num-lines ref-alist)
    (let ((line 0))
      (funcall format-code code
               (lambda (loc line-num ref)
                 (cl-incf line)
                 (let ((html (funcall fun loc line-num ref)))
                   (if (or (memq (or line-num line) org--highlighted) (and ref (member ref org--highlighted)))
                       (concat "<span class=\"osbe-highlight\" style=\"position: absolute; left: 0; right: 0; height: 1lh;"
                               " background-color: rgba(255, 213, 0, 0.25); pointer-events: none;\"></span>"
                               html)
                     html)))
               num-lines ref-alist))))

;;;; :log ---what a block transpiles to, shown apart

(defvar org--log-inhibited nil
  "Non-nil while C-c C-c renders a :log block alone, so it shows its rendering rather than logging it again.")

(defun org--log-rendering (rendering name backend log)
  "Logs RENDERING, block NAME's BACKEND output, as LOG says: t, appending it to the buffer *Org Block Log*; else
LOG is a file, which holds it afresh."
  (if (not (eq log t)) (write-region rendering nil (expand-file-name (format "%s" log)) nil 'silent)
    (with-current-buffer (get-buffer-create "*Org Block Log*")
      (goto-char (point-max))
      (insert (format "%s#+begin_%s ⟶ %s, %s\n%s\n" (if (bobp) "" "\n") name backend (format-time-string "%T") rendering)))))

(org-defswitch :log (log &key render contents backend name)
  "The result, logged as what it transpiles to along BACKEND: for LOG t, in the buffer *Org Block Log*; else in the
file LOG.  The export is as ever; C-c C-c within the block, save a src block, shows the rendering at once.
  #+begin_box Lemma :log t           ;; Its HTML, upon an HTML export, appended to *Org Block Log*
  #+begin_graph :log graph.tex       ;; Its LaTeX, upon a LaTeX export, in graph.tex
Outermost of all switches, it logs what the others make of the block too; see `org--block-log-cc'."
  :depth -95
  (let ((result (funcall render contents)))
    (unless org--log-inhibited
      (org--log-rendering (string-trim (org-export-string-as (format "\n%s\n" result)
                                                             (or org-export-current-backend backend) t))
                          name (or org-export-current-backend backend) log))
    result))

(defun org--block-log-cc ()
  "Shows what the :log block around point transpiles to, along each of `org-demo-backends'; nil when there is none.
A src block's C-c C-c stays Babel's, running its code."
  (-when-let* ((block (org-element-lineage (org-element-at-point) (remq 'src-block org--switch-block-types) t))
               (site (org--block-switch-site block)))
    (when (plist-get (nth 5 site) :log)
      (let ((source (buffer-substring (org-element-property :begin block)
                                      (save-excursion (goto-char (org-element-property :end block))
                                                      (skip-chars-backward " \t\n") (point))))
            (org--log-inhibited t))
        (display-message-or-buffer
         (mapconcat (lambda (backend) (format "#+begin_%s ⟶ %s\n%s" (nth 3 site) backend (org-demo--export source backend)))
                    org-demo-backends "\n\n")
         "*Org Block Log*"))
      t)))

;; After the dispatcher, so a block's own action, or :C-c_C-c, wins.
(add-hook 'org-ctrl-c-ctrl-c-hook #'org--block-log-cc 10)

;;;; Coderefs hovered: Org's own [[(name)]] links

(defun org--coderef-script (output backend _info)
  "Returns HTML OUTPUT with Org's own `org-html-scripts' in its <head>, when a [[(name)]] link needs them yet it lacks
them; so hovering such a link lights its line, as Org's manual promises, whatever `org-html-head-include-scripts'."
  (if (and (org-export-derived-backend-p backend 'html)
           (string-search "CodeHighlightOn(" output)
           (not (string-search "function CodeHighlightOn" output))
           (string-match "</head>" output))
      (concat (substring output 0 (match-beginning 0)) org-html-scripts "\n" (substring output (match-beginning 0)))
    output))

(provide 'org-display-switches)
;;; org-display-switches.el ends here
