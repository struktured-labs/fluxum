## What changed

<!-- Describe behavior and motivation. -->

## Verification

<!-- List exact build, test, and benchmark commands run. -->

## Fluxum review ritual

- [ ] Jane Street Core/Async first; no blocking production I/O.
- [ ] Recoverable errors remain precise polymorphic variants.
- [ ] Semantic and Boolean control flow uses exhaustive matches.
- [ ] `Option`/`Result` combinators or monadic syntax express genuine value
      pipelines without hiding protocol states.
- [ ] No `Obj`, unchecked casts, or representation-dependent tricks.
- [ ] Mutable or hot-path code documents ownership and invariants; performance
      claims include benchmark evidence.
- [ ] Tests cover success, meaningful failure variants, malformed boundaries,
      and relevant Async/session transitions.
- [ ] `dune build` and relevant test aliases pass.

Canonical rules: [`docs/guides/STYLE.md`](../docs/guides/STYLE.md).
