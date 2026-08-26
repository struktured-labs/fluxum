# Fluxum agent instructions

These instructions apply to the entire repository.

Before changing OCaml, read and follow
[`docs/guides/STYLE.md`](docs/guides/STYLE.md). Treat its requirements as review
criteria, not suggestions.

In particular:

- Use Jane Street Core and Async first.
- Model recoverable failures with precise polymorphic variants.
- Use exhaustive pattern matching for semantic control flow, including Boolean
  branches; do not hide protocol states behind catch-all cases.
- Use `Option` and `Result` combinators, scoped `Let_syntax`, or monadic
  operators when the code is genuinely a value pipeline. Do not mechanically
  expand those pipelines into nested matches, or mechanically compress
  meaningful branches into combinators.
- Do not use `Obj` or representation-dependent tricks.
- Keep production I/O nonblocking and Async-native.
- Document and benchmark mutation or allocation-oriented hot-path changes.
- Add focused tests for success, every meaningful failure variant, and protocol
  state transitions.

Run `dune build` and the narrowest relevant `dune runtest` alias before handing
off a change. Run the full test suite when the change crosses module or adapter
boundaries.
