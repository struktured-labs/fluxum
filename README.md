# fluxum

Core abstractions for exchange-agnostic trading APIs built on Jane Street/Core/Async.

Fluxum defines a generic `Exchange` interface (lib/exchange_intf.ml) with normalized types (lib/types.ml).

Use the `Fluxum.Make` functor (lib/fluxum.ml) with a concrete exchange adapter to expose normalized operations and streams, while still allowing access to native, exchange-specific features.

## Gemini Adapter

The Gemini library implements the full REST and WebSocket APIs. The adapter (lib/exchange/gemini/fluxum_adapter.ml) implements the generic `Exchange_intf.S` for Gemini and provides a small `Builder` to construct native order requests from normalized inputs.

Example (pseudo-code):

```ocaml
module E = Gemini.Fluxum_adapter
module F = Fluxum.Make(E)(Gemini.Fluxum_adapter.Builder)

let cfg = (module (val Gemini.Cfg.or_default None : Gemini.Cfg.S)) in
let%bind nonce = Gemini.Nonce.File.pipe ~init:Gemini.Session.default_filename () in
let e = Gemini.Fluxum_adapter.create ~cfg ~nonce ~symbols:["BTCUSD"] () in

let%bind result = F.place_order_norm e ~symbol:"BTCUSD" ~side:Types.Side.Buy ~kind:(Types.Order_kind.Limit 10000.) ~qty:0.1 in
match result with
| Ok order -> printf "%s\n" (Sexp.to_string_hum (Types.Order.sexp_of_t order))
| Error err -> printf "error: %s\n" (Sexp.to_string_hum (Types.Error.sexp_of_t err))
```

Streams:

- `F.Streams.trades e` yields normalized `Types.Trade.t` from private order events.
- `F.Streams.book_updates e` yields normalized `Types.Book_update.t` from public market data.

## Build

- Prereqs: OCaml 5.1+, `opam`, `dune`, and deps in `fluxum.opam`.
- Install deps: `opam install . --deps-only`
- Build: `dune build`
- Run CLI (Gemini commands): `dune exec fluxum -- --help`

## Notes

- Specialized features remain available via `F.Native.*` for exchange-specific types and operations.
- `ledger` and `session` abstractions can layer on `Types` and the normalized streams; the Gemini-specific implementations are still available under lib/exchange/gemini/ and can be adapted similarly.
