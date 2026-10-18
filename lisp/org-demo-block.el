;;; org-demo-block.el --- Org source beside what it exports to  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Musa Al-hassy

;; Author: Musa Al-hassy <alhassy@gmail.com>
;; Version: 26.10.18
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "28.2") (org "9.8"))
;; Keywords: org, blocks, convenience
;; URL: https://alhassy.com/org-special-block-extras

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; An org-demo block shows its contents both as Org source and as the result, side by side or in sequence.
;;
;; Full documentation at https://alhassy.com/org-special-block-extras

;;; Code:

(require 'org-defblock)
(require 'org-box-block)
(require 'org-parallel-block)

(org-defblock org-demo (nil nil source "Source" result "Result"
                      source-color "cyan" result-color "cyan"
                      style "parallel"
                            sep (if (equal backend 'html) "@@html:<p><br>@@" "\n\n\n\n")
                            :manual "#Switches-for-every-block")
              "Output the CONTENTS of the block as both parsed Org and unparsed.

Label the source text by SOURCE and the result text by RESULT

finally, the source-result fragments can be shown in a STYLE
that is either “parallel” (default) or “sequential”.

SEP is the separator; e.g., a rule ‘<hr>'.

In Markdown, the source is a fenced org block above its result, each under its label in bold.
"
              (if (org-export-markdown-p)
                  (org-parse (format "*%s*\n#+begin_src org\n%s\n#+end_src\n\n*%s*\n\n%s"
                                     source (org-escape-code-in-string raw-contents) result raw-contents))
                  (-let [text (concat
                               ;; Source; in LaTeX an Org example block, which Org typesets, and for which beamer's
                               ;; frame is marked fragile, as raw verbatim text would not be.
                               (thread-last (if (equal backend 'html)
                                                (org-export (format "<div ><pre class=\"src src-org\">%s</pre></div>"
                                                                    raw-contents))
                                              (format "#+begin_example\n%s\n#+end_example"
                                                      (org-escape-code-in-string raw-contents)))
                                            (org--blockcall box source :background-color source-color)
                                            org-export)
                               ;; Separator
                               sep
                               ;; Result
                               (thread-last raw-contents
                                            (org--blockcall box result :background-color result-color)
                                            org-export))]

                    (if (equal style "parallel")
                        (org--blockcall parallel "2" :bar nil text)
                      (concat "#+end_export\n" text "\n#+begin_export")))))

(provide 'org-demo-block)
;;; org-demo-block.el ends here
