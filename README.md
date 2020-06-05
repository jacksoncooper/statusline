`statusline` is a first Haskell program to get a feel for Haskell's build tools
and experiment with the IO type and modules in `base`. It emits system
information to populate `sway-bar` in an extremely environment-specific and
unportable way.

Things I'd like to implement:

1. Emit JSON as specified by `swaybar-protocol`.
2. Add another thread to listen for pause and continue signals.
3. Use `Text.ParserCombinators.ReadP` in `base` instead of your own parser
   combinators.
4. Use only non-partial functions and be able to recover from parsing failures.
   I'm looking at you `init`.
5. Figure out what's going on with record exporting and how that affects the
   privacy of a datatype and its constructors.
