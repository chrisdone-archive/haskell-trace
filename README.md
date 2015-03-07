haskell-trace
=====

Add tracing to modules.

## Setup

You must `cabal install haskell-trace` in your sandbox if you have a
sandbox, or otherwise install it so that the module
`Language.Haskell.Trace` is available to be imported.

Load `haskell-trace.el` in your Emacs.

## Use

Run `M-x haskell-trace-minor-mode` in a module. It will add the
following to the top of the file:

``` haskell
{-# OPTIONS -F -pgmF haskell-trace #-}

```

And it will open a buffer (not focused, switch to it when you're
ready) in your session named `*session-name:trace*` (where your
session-name is specific to your Cabal project).

Now run some code in the REPL or `cabal run` your project. The buffer
will automatically revert any lines appended to the file. Inside your
trace buffer you will see something like:

    /home/chris/Packages/haskell-trace/src/Fib.hs:(9,9)-(9,29)
    /home/chris/Packages/haskell-trace/src/Fib.hs:(9,9)-(9,17)
    /home/chris/Packages/haskell-trace/src/Fib.hs:(9,9)-(9,11)

You can click on the lines to jump to that source span in the file, or
use `n` (next) and `p` (previous) to automatically highlight the span
in the appropriate buffer. It will jump between files as necessary.

You can run `g` to 'refresh' the buffer, but in this case it simply
clears the log.

## Filters

If you want to pay attention to only a specific portion of your
module and ignore everything else, you can select any portion of text
and run `M-x haskell-trace-make-filter` and it will add that filter to
the `OPTIONS` pragma at the top of the module, so it will look like
this:

``` haskell
{-# OPTIONS -F -pgmF haskell-trace -optF --filter=(9,0)-(9,29) #-}
```

And it will add an overlay to that selection to highlight which part
of the source will be traced. This is not just an output filter but no
trace wrappers will be added to any expressions in the preprocessing
stage to code outside of your selections.

You can remove filters later by manually deleting the text (I'll
automate this later). And run `M-x haskell-trace-refresh` to update
the highlights.

## Caveats

The haskell-src-exts parser doesn't know about operator precedence, so
if you get type errors in operator code when trying to load a traced
module, you can just add parentheses to disambiguate it.
