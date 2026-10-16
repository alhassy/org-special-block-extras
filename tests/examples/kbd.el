;;; kbd.el --- Snapshot of the `kbd' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "kbd"
  :input "
1. It becomes <kbd> tags, but final symbol non-ascii *may* be ignored

   kbd:C-u_80_-∀

2. [ [ It ] ] becomes <kbd> tags

   [[kbd:C-u_80_-]]

3. Using syntax <it> becomes <kbd> tags, and surrounding space is trimmed

   <kbd: C-u 80 - >

4. It has a tooltip documenting the underlying Lisp function, when possible

   <kbd: M-s h .>"
  :html "<ol class=\"org-ol\">
<li><p>
It becomes &lt;kbd&gt; tags, but final symbol non-ascii <b>may</b> be ignored
</p>

<p>
<kbd style=\"\">C-u 80 -</kbd>∀
</p></li>

<li><p>
[ [ It ] ] becomes &lt;kbd&gt; tags
</p>

<p>
<kbd style=\"\">C-u 80 -</kbd>
</p></li>

<li><p>
Using syntax &lt;it&gt; becomes &lt;kbd&gt; tags, and surrounding space is trimmed
</p>

<p>
<kbd style=\"\">C-u 80 -</kbd>
</p></li>

<li><p>
It has a tooltip documenting the underlying Lisp function, when possible
</p>

<p>
<abbr class=\"tooltip\" tabindex=\"0\" style=\"border: none; text-decoration: none;\"><kbd
style=\"border-color: red\">M-s h .</kbd><template><pre>M-s h . ∷ highlight-symbol-at-point
Highlight each instance of the symbol at point.
Uses the next face from ‘hi-lock-face-defaults’ without prompting,
unless you use a prefix argument.
Uses ‘find-tag-default-as-symbol-regexp’ to retrieve the symbol at point.

If REGEXP contains upper case characters (excluding those preceded by ‘\\’)
and ‘search-upper-case’ is non-nil, the matching is case-sensitive.

This uses Font lock mode if it is enabled; otherwise it uses overlays,
in which case the highlighting will not update as you type.  The Font
Lock mode is considered \"enabled\" in a buffer if its ‘major-mode’
causes ‘font-lock-specified-p’ to return non-nil, which means
the major mode specifies support for Font Lock.</pre></template></abbr>
</p></li>
</ol>"
  :latex "\\begin{enumerate}
\\item It becomes <kbd> tags, but final symbol non-ascii \\textbf{may} be ignored

\\texttt{C-u 80 -}∀

\\item {[} [ It ] ] becomes <kbd> tags

\\texttt{C-u 80 -}

\\item Using syntax <it> becomes <kbd> tags, and surrounding space is trimmed

\\texttt{C-u 80 -}

\\item It has a tooltip documenting the underlying Lisp function, when possible

\\texttt{M-s h .}
\\end{enumerate}")
