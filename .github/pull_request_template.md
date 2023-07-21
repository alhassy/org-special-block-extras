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

## Please tick the following

1. [ ] I am aware of the extensive documentation at http://alhassy.com/org-special-block-extras/
2. [ ] I have read the documentation of doc:org-defblock
3. [ ] I have checked some boxes without reading them.
