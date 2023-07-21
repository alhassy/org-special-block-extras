## I defined the following defblock

For example,

     (org-defblock shout ()
        "Say things in uppercase"
        (upcase contents))

## I am using it as follows

    #+begin_shout
    Hello, can you hear me?
    #+end_shout

## In the HMTL backend, I expect to see

    HELLO, CAN YOU HEAR ME?

## However, what I actually see

    ...

## I suspect the issue is at ...

Or can be solved by ...

## Please tick the following, by replacing `[ ]` with `[X]`

1. [ ] I am aware of the extensive documentation at http://alhassy.com/org-special-block-extras/
2. [ ] I have read the documentation of doc:org-defblock
3. [ ] I have checked some boxes without reading them.
4. [ ] I am aware that `(org-defblock X ⋯)` gives me a Lisp function `org-block/X`
       that I can play with; e.g., `C-h o org-block/shout` shows a function with its
       docstring and arguments. We can press `C-u C-x C-e` at the end of the
       closing parens of `(org-block/shout 'html  "*Hello*, /world/!")` to see
       what _Org sees_ when it rewrites a `shout` block with the given string as its contents.
