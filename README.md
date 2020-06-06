`statusline` is a first Haskell program to get a feel for Haskell's build tools
and experiment with the IO type and modules in `base`. It emits system
information to populate `sway-bar` in an extremely environment-specific and
unportable way.

To implement:

1. Emit JSON as specified by `swaybar-protocol`.
2. Add another thread to listen for pause and continue signals.
3. Use `Text.ParserCombinators.ReadP` in `base`.
4. Recover gracefully from file input/output errors.
5. Extend the existing parser combinators to parse the output of
   `amixer get Master`.
