;;; show.el --- Snapshot of the `show' showcase example  -*- lexical-binding: t; -*-

(unless (fboundp 'define-org-example-test)
  (load-file (expand-file-name "~/org-special-block-extras/tests/org-examples.el")))

(define-org-example-test "show"
  :input "
The ~doc~ link shows documentation, but to see the value we can use ~show~ as in:

                        doc:user-full-name
                       show:user-full-name

This allows us to have “generic” sentences:

                      My name is show:user-full-name,
                and I am using Emacs show:emacs-version ^_^
            My favourite number is <show: (* 2 (+ 3 4 (- pi))) >
   
To see values of expressions in other programming languages, use Org Babel inline source expressions."
  :html "<p>
The <code>doc</code> link shows documentation, but to see the value we can use <code>show</code> as in:
</p>

<p>
 <abbr class=\"tooltip\" title=\"Return the full name of the user logged in, as a string.<br>If the full name corresponding to Emacs’s userid is not known,<br>return ''unknown''.<br><br>If optional argument UID is an integer, return the full name<br>of the user with that uid, or nil if there is no such user.<br>If UID is a string, return the full name of the user with that login<br>name, or nil if there is no such user.<br><br>If the full name includes commas, remove everything starting with<br>the first comma, because the 'gecos' field of the '/etc/passwd' file<br>is in general a comma-separated list.<br><br>(fn &optional UID)\">user-full-name</abbr>
\"Musa Al-hassy\"

</p>

<p>
This allows us to have “generic” sentences:
</p>

<p>
	  My name is \"Musa Al-hassy\"
,
    and I am using Emacs \"29.4\"
 ^_^
My favourite number is 7.716814692820414

</p>

<p>
To see values of expressions in other programming languages, use Org Babel inline source expressions.
</p>"
  :latex "The \\texttt{doc} link shows documentation, but to see the value we can use \\texttt{show} as in:

 \\hyperref[o-glossary-user-full-name]{user-full-name}\\label{o-glossary-declaration-site-user-full-name}
\"Musa Al-hassy\"

This allows us to have “generic” sentences:

	  My name is \"Musa Al-hassy\"
,
    and I am using Emacs \"29.4\"
 \\^{}\\_\\^{}
My favourite number is 7.716814692820414

To see values of expressions in other programming languages, use Org Babel inline source expressions.")
