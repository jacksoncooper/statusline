`statusline` is a toy Haskell program to get a feel for Haskell's build tools
and experiment with the `IO` type and modules in `base`. It emits system
information to populate `sway-bar` in an extremely environment-specific and
unportable way.

![Screenshot of statusline output in sway-bar.](https://i.imgur.com/OVxaxVR.png?2)

To implement:

1. Emit JSON as specified by `swaybar-protocol`.
2. Add another thread to listen for stop and continue signals from
   `swaybar-protocol`.
3. Use `Text.ParserCombinators.ReadP` in `base`.
4. Clean up parser combinators to avoid redundancy.
