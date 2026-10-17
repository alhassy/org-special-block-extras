;;; org-solution-block.el --- Answers folded away, behind a reprimand  -*- lexical-binding: t; -*-

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

;; A solution block is a details block, holding a box of reprimand, holding the answer in details.
;;
;; Full documentation at https://alhassy.github.io/org-special-block-extras

;;; Code:

(require 'org-defblock)
(require 'org-box-block)
(require 'org-details-block)
(require 'org-thread-blockcall)

;;;;;; solution
(org-defblock solution
              (title "Solution" reprimand "Did you actually try? Maybe see the ‘hints’ above!"
                     really "Solution, for real")
              "Show the answers to a problem, but with a reprimand in case no attempt was made."
              (org-thread-blockcall raw-contents
                                    (details really :title-color "red")
                                    (box reprimand :background-color "blue")
                                    (details title)))

(provide 'org-solution-block)
;;; org-solution-block.el ends here
