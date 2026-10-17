;;; org-quote-block.el --- Quotations, cited by a bibliography key  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.16
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.github.io/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A quote block given a key is a cited quotation; sans one, it is Org's own quote.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)

;;;;;; Quotations, cited

;; Org's own quote, given a citation; a plain #+begin_quote is handed back to Org, by `org-builtin-block'.
(org-defblock quote (&optional (key "") &key (pages "") :latex-head ("csquotes"))
  "Quote CONTENTS, cited as the bibliography's KEY, at PAGES; e.g.,

  #+begin_quote zermelo1908 :pages 20-22
  Set theory is that branch of mathematics whose task is …
  #+end_quote

In LaTeX, csquotes' \\blockcquote[PAGES]{KEY}{CONTENTS}, citing by the document's own bibliography ---biblatex,
BibTeX, or org-cite--- whose \\addbibresource or \\bibliography is ours to give; csquotes itself is loaded for us,
by its :latex-head.  Elsewhere, a quotation attributed to KEY, at PAGES.  Sans a KEY, Org's own quote, as ever.
As a link, an inline quotation: [[quote:zermelo1908][…]] is \\textcquote{zermelo1908}{…}, and
[[quote:][to be]] is “to be”.  A section tagged :quote: is a quote block of its text."
  ;; The header reads 20-22 as a symbol, and a key may be one too; we want their names.
  (let* ((key (format "%s" key))
         (pages (format "%s" pages))
         (at (if (org-string-nw-p pages) (format ", %s" pages) ""))
         (latex-pages (if (org-string-nw-p pages) (format "[%s]" pages) "")))
    (cond
     ;; Links: inline quotations.
     ((and contents-occur-as-link-description (not (org-string-nw-p key)))
      (pcase backend ('latex (format "``%s''" contents)) (_ (format "“%s”" contents))))
     (contents-occur-as-link-description
      (cond ((org-export-markdown-p) (format "“%s” (*%s*%s)" contents key at))
            ((equal backend 'latex) (format "\\textcquote%s{%s}{%s}" latex-pages key contents))
            (t (format "<q>%s</q> (<cite>%s</cite>%s)" contents key at))))
     ;; Blocks: Org's own quote sans a key, which the rewriter leaves be; else a cited block quotation.
     ((not (org-string-nw-p key)) (org-parse (org-builtin-block "quote" raw-contents)))
     ((org-export-markdown-p) (org-parse (org-builtin-block "quote" (format "%s\n\n--- /%s/%s" raw-contents key at))))
     ((equal backend 'latex) (format "\\blockcquote%s{%s}{%s}" latex-pages key contents))
     (t (format "<blockquote>%s<footer>&mdash; <cite>%s</cite>%s</footer></blockquote>" contents key at)))))

(provide 'org-quote-block)
;;; org-quote-block.el ends here
