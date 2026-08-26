# Copilot instructions for Fluxum

Use [`docs/guides/STYLE.md`](../docs/guides/STYLE.md) as the canonical coding and
review standard.

When generating or reviewing OCaml:

- prefer Jane Street Core and Async;
- use precise polymorphic-variant errors and preserve their information;
- use exhaustive matches for protocol states and meaningful control flow;
- use `Option`/`Result` combinators and locally scoped monadic syntax for genuine
  value pipelines;
- express Boolean branches with `match`, not `if`/`else`;
- reject malformed exchange input explicitly;
- use no `Obj`, unchecked casts, blocking production I/O, or unexplained mutable
  state;
- require focused tests, plus benchmark evidence for performance claims.

In reviews, call out violations with the relevant rule and distinguish required
correctness changes from optional readability suggestions.
