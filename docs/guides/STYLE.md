# Fluxum OCaml style

This is the canonical implementation and review standard for Fluxum. It applies
to new code and to code being materially changed; unrelated legacy code does not
need to be rewritten solely for style.

## Core and Async first

- Prefer Jane Street Core, Base, and Async APIs over the OCaml standard library
  or another dependency.
- Keep production I/O nonblocking. Represent asynchronous work with
  `Deferred.t`, streams with `Pipe.Reader.t`, and concurrent independent work
  with the appropriate Async combinator.
- Keep the active monad obvious. Scope `Let_syntax` locally when more than one
  monad appears in a module.

## Types and errors

- Make invalid states difficult to construct. Prefer precise records and
  variants over strings, flags, or sentinel values.
- Use polymorphic variants for recoverable errors and compose their rows at
  module boundaries.
- Preserve the underlying error instead of collapsing it to a string or raising
  an exception.
- Handle all meaningful cases explicitly. Avoid catch-all patterns for closed
  protocol or lifecycle variants when a new constructor should force a review.
- Never use `Obj`, unchecked casts, or representation-dependent tricks.

## Matches, combinators, and monadic syntax

Use the construct that exposes the meaning of the computation:

- Use `match` for semantic branches: protocol messages, session states, error
  classes, state transitions, and cases with meaningfully different behavior.
- Express Boolean control flow as an exhaustive `match` on `true` and `false`,
  rather than an `if`/`else` block.
- Use `Option.map`, `bind`, `fold`, `value_map`, `Let_syntax`, or scoped monadic
  operators when an optional value is transformed, chained, or reduced.
- Apply the same principle to `Result`: use `Result.Let_syntax` for sequential
  validation or transformation, and explicit matching when error branches cause
  different behavior.
- Prefer `let%bind` and `let%map` for a multi-step pipeline. Short infix
  pipelines are fine when their monad is locally obvious.
- Do not use a combinator merely to avoid writing a match. Do not use nested
  matches merely to avoid a combinator. Optimize for explicit semantics and
  local readability.

For example, sequential validation should share one small primitive:

```ocaml
let require_nonempty tag value =
  match String.is_empty value with
  | true -> Error (`Invalid_value (tag, value))
  | false -> Ok ()

let validate ~sender ~target ~optional_value =
  let open Result.Let_syntax in
  let%bind () = require_nonempty 49 sender in
  let%bind () = require_nonempty 56 target in
  Option.value_map optional_value
    ~default:(Ok ())
    ~f:(require_nonempty 122)
```

A protocol state machine should stay visibly exhaustive:

```ocaml
match session_state with
| `Disconnected -> return (Error `Not_connected)
| `Logging_on -> return (Error `Not_logged_on)
| `Established live -> send live message
| `Failed error -> return (Error error)
```

## Mutation and performance

- Prefer immutable values by default.
- Mutation is appropriate for measured hot paths or explicitly owned session
  state. Document the ownership, invariants, and reason for mutation.
- Do not trade protocol correctness, exhaustiveness, or typed errors for a
  speculative speedup.
- Benchmark performance claims with representative inputs. Record both latency
  and allocation effects when they drive the design.
- Keep encoding and decoding paths bounded and total over untrusted exchange
  input. Reject malformed, duplicate, missing, and out-of-range fields
  explicitly.

## Interfaces and implementation

- Put public contracts in `.mli` files. Keep helpers private unless another
  module has a real consumer.
- Use `_exn` only when failure truly represents a violated programmer invariant;
  provide a typed non-raising path for data or exchange failures.
- Prefer small named helpers when they remove repeated policy or validation, not
  merely to shorten a function.
- Preserve historical symbols and currencies for backward compatibility.

## Tests and review

Every material change should be reviewed for:

- success behavior and every meaningful typed failure;
- exhaustive protocol and lifecycle transitions;
- malformed and boundary inputs at parsing or exchange boundaries;
- deterministic unit tests without live credentials;
- Async cancellation, closure, reconnect, and partial-write behavior where
  applicable;
- benchmark evidence for hot-path claims;
- absence of blocking I/O, `Obj`, unchecked casts, and accidental exception
  paths.

Format with `dune fmt` where the repository target supports it. At minimum, run
`dune build` and the narrowest relevant test alias. Run `dune runtest` for changes
that cross shared-module or exchange-adapter boundaries.
