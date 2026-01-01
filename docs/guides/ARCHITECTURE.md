# Fluxum Architecture Guide

This document describes the architecture and design principles of Fluxum.

## Table of Contents

- [Overview](#overview)
- [Core Principles](#core-principles)
- [Module Structure](#module-structure)
- [Common Order Book](#common-order-book)
- [Exchange Adapters](#exchange-adapters)
- [WebSocket Streaming](#websocket-streaming)
- [REST API Pattern](#rest-api-pattern)
- [Type Safety](#type-safety)
- [Concurrency Model](#concurrency-model)

## Overview

Fluxum is built with three main goals:

1. **Unified Interface**: Common API across all exchanges
2. **Type Safety**: Compile-time guarantees via OCaml's type system
3. **Performance**: Non-blocking I/O with Jane Street Async

```
┌─────────────────────────────────────────────────────────────┐
│                      Application Layer                       │
│          (Trading Bots, Analytics, Arbitrage, etc.)         │
└─────────────────────────────────────────────────────────────┘
                              ▲
                              │
┌─────────────────────────────────────────────────────────────┐
│                      Fluxum Library                          │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Consolidated Order Book  │  Order Book Incremental  │  │
│  └──────────────────────────────────────────────────────┘  │
│  ┌──────────────────────────────────────────────────────┐  │
│  │           Exchange Common (Order_book_base)          │  │
│  └──────────────────────────────────────────────────────┘  │
│  ┌─────────┬─────────┬────────┬─────────┬──────────────┐  │
│  │ Binance │ Kraken  │ Gemini │  MEXC   │ Hyperliquid  │  │
│  │ Adapter │ Adapter │Adapter │ Adapter │   Adapter    │  │
│  └─────────┴─────────┴────────┴─────────┴──────────────┘  │
└─────────────────────────────────────────────────────────────┘
                              ▲
                              │
┌─────────────────────────────────────────────────────────────┐
│              Exchange WebSocket & REST APIs                  │
│     (Binance.com, Kraken.com, Gemini.com, etc.)            │
└─────────────────────────────────────────────────────────────┘
```

## Core Principles

### 1. Functional Programming

- **Immutability**: Order books are immutable; updates create new instances
- **Pure Functions**: Core logic has no side effects
- **Pattern Matching**: Exhaustive matching for all variants
- **Type Inference**: Minimal type annotations needed

```ocaml
(* Immutable update *)
let book = Order_book.create ~symbol:"BTC/USD" in
let book' = Order_book.set book ~side:`Bid ~price:50000. ~size:1.0 in
(* book unchanged, book' is new instance *)
```

### 2. Modularity

- **Functors**: Reusable components parameterized by types
- **Interfaces**: Clear separation of interface (.mli) and implementation (.ml)
- **Composition**: Small modules composed into larger systems

```ocaml
(* Functor pattern for order books *)
module Make (Config : sig
  type symbol
  type metadata
  val default_metadata : unit -> metadata
end) : sig
  module Book : sig
    type t
    val create : symbol:Config.symbol -> t
    (* ... *)
  end
end
```

### 3. Async I/O

- **Non-blocking**: All I/O uses Async
- **Pipes**: Streaming data via Pipe.Reader/Writer
- **Deferreds**: Asynchronous computations

```ocaml
(* Async pipeline *)
let%bind pipe = websocket_connect url in
let%bind processed_pipe =
  pipe
  |> Pipe.map ~f:parse_message
  |> Pipe.filter_map ~f:validate
in
Pipe.iter processed_pipe ~f:handle_update
```

## Module Structure

### Library Organization

```
lib/
├── fluxum.ml             # Main library module
├── types.ml              # Common types (Symbol, Side, Order, etc.)
├── exchange_intf.ml      # Exchange interface signature
├── order_book_intf.ml    # Order book interface
├── consolidated_order_book.ml  # Multi-exchange aggregation
├── exchange/
│   ├── common/
│   │   ├── order_book_base.ml   # Base order book implementation
│   │   └── crypto.ml             # Common crypto utilities
│   ├── binance/
│   │   ├── binance.ml            # Module exports
│   │   ├── cfg.ml                # Configuration
│   │   ├── common.ml             # Types (Side, Order_type, etc.)
│   │   ├── signature.ml          # HMAC-SHA256 signing
│   │   ├── rest.ml               # REST client infrastructure
│   │   ├── ws.ml                 # WebSocket client
│   │   ├── order_book.ml         # Order book implementation
│   │   ├── v3.ml                 # API v3 endpoints
│   │   └── test/                 # Tests
│   ├── kraken/
│   │   └── ...                   # Similar structure
│   └── ...                       # Other exchanges
└── cli_args.ml           # CLI argument parsing
```

### Exchange Module Pattern

Each exchange follows a consistent structure:

```ocaml
module <Exchange> = struct
  module Cfg : sig
    module type S = sig
      val base_url : string
      val ws_url : string
    end

    module Production : S
    module Testnet : S
  end

  module Common : sig
    type side = BUY | SELL
    type order_type = LIMIT | MARKET | ...
  end

  module Signature : sig
    val sign : api_secret:string -> params:(string * string) list -> string
  end

  module Rest : sig
    module Error : sig
      type t = ...
    end

    module Make : functor (...) -> sig
      val call : ... -> 'response Deferred.t
    end
  end

  module Ws : sig
    val connect : ... -> 'a Pipe.Reader.t Deferred.t
  end

  module Order_book : sig
    type metadata = ...
    module Book : sig
      type t
      val pipe : ... -> t Pipe.Reader.t Deferred.t
    end
  end

  module V<N> : sig
    module <Endpoint> : sig
      type request = ...
      type response = ...
      val call : ... -> response Deferred.t
    end
  end
end
```

## Common Order Book

The `Exchange_common.Order_book_base` module provides a unified order book implementation.

### Design

```ocaml
module Order_book_base.Make (Config : sig
  type symbol
  val sexp_of_symbol : symbol -> Sexp.t
  val symbol_of_sexp : Sexp.t -> symbol
  val compare_symbol : symbol -> symbol -> int

  type metadata
  val sexp_of_metadata : metadata -> Sexp.t
  val metadata_of_sexp : Sexp.t -> metadata
  val default_metadata : unit -> metadata
end) : sig
  module Book : sig
    type t  (* Abstract type *)

    (* Construction *)
    val create : symbol:Config.symbol -> t

    (* Updates *)
    val set : ?timestamp:float -> ?metadata:Config.metadata ->
      t -> side:[`Bid | `Ask] -> price:float -> size:float -> t

    val set_many : ?timestamp:float -> ?metadata:Config.metadata ->
      t -> ([`Bid | `Ask] * float * float) list -> t

    (* Queries *)
    val best_bid : t -> Price_level.t
    val best_ask : t -> Price_level.t
    val mid_price : t -> float
    val spread : t -> float

    (* Analytics *)
    val vwap_buy : t -> volume:float -> float option
    val vwap_sell : t -> volume:float -> float option
    val total_volume_n : t -> side:[`Bid | `Ask] -> n:int -> float
  end

  module Books : sig
    type t  (* Multi-symbol order book collection *)

    val empty : t
    val set_book : t -> Book.t -> t
    val book : t -> Config.symbol -> Book.t option
  end
end
```

### Implementation Details

#### Price Levels

```ocaml
type price_level = {
  price: float;
  volume: float;
}
```

Stored in sorted maps:
- **Bids**: Descending price order (best bid first)
- **Asks**: Ascending price order (best ask first)

```ocaml
module Bid_price_map = Map.Make(struct
  type t = float
  let compare a b = Float.compare b a  (* Reverse order *)
end)

module Ask_price_map = Map.Make(struct
  type t = float
  let compare = Float.compare
end)
```

#### Book Structure

```ocaml
type t = {
  symbol: Config.symbol;
  epoch: int;                    (* Update counter *)
  update_time: float;            (* Unix timestamp *)
  metadata: Config.metadata;      (* Exchange-specific *)
  bids: price_level Bid_price_map.t;
  asks: price_level Ask_price_map.t;
}
```

#### Update Algorithm

```ocaml
let set ?timestamp ?metadata book ~side ~price ~size =
  let timestamp = Option.value timestamp ~default:(Time_ns.now () |> ...) in
  let metadata = Option.value metadata ~default:book.metadata in
  let epoch = book.epoch + 1 in

  (* Update appropriate side *)
  let bids, asks = match side with
    | `Bid ->
        let bids =
          if Float.(size = 0.) then Map.remove book.bids price
          else Map.set book.bids ~key:price ~data:{price; volume=size}
        in
        (bids, book.asks)
    | `Ask ->
        let asks =
          if Float.(size = 0.) then Map.remove book.asks price
          else Map.set book.asks ~key:price ~data:{price; volume=size}
        in
        (book.bids, asks)
  in

  { symbol = book.symbol; epoch; update_time = timestamp; metadata; bids; asks }
```

### Exchange Integration

Each exchange implements the base:

```ocaml
(* Binance *)
module Binance.Order_book = struct
  type metadata = { last_update_id: int64 }

  module Book_base = Exchange_common.Order_book_base.Make(struct
    type symbol = string
    let sexp_of_symbol s = Sexp.Atom s
    let symbol_of_sexp = function Sexp.Atom s -> s | _ -> failwith "invalid"
    let compare_symbol = String.compare

    type nonrec metadata = metadata [@@deriving sexp]
    let default_metadata () = { last_update_id = 0L }
  end)

  module Book = struct
    include Book_base.Book

    (* Add exchange-specific functionality *)
    let pipe (module Cfg : Cfg.S) ~symbol () =
      (* WebSocket connection returning Book.t Pipe.Reader.t *)
      ...
  end
end
```

## Exchange Adapters

### REST API Pattern

Uses functors for code reuse:

```ocaml
module Rest = struct
  module Error = struct
    type t =
      | Http_error of int * string
      | Json_parse_error of string
      | Api_error of string * string
    [@@deriving sexp]
  end

  (* Generic endpoint *)
  module Make (T : sig
    val name : string
    val endpoint : string
    val http_method : [`GET | `POST | `DELETE]
    val requires_auth : bool

    type request [@@deriving sexp]
    val request_to_params : request -> (string * string) list

    type response [@@deriving sexp, of_yojson]
  end) = struct
    let call ~cfg ?api_key ?api_secret request =
      let params = T.request_to_params request in
      let params = if T.requires_auth then
        (* Add signature *)
        let nonce = generate_nonce () in
        let params = params @ [("nonce", nonce)] in
        let signature = Signature.sign ~api_secret ~params in
        params @ [("signature", signature)]
      else params
      in

      let%bind response = Http.call
        ~method_:T.http_method
        ~url:(cfg.base_url ^ T.endpoint)
        ~params
        ~headers:(if T.requires_auth then [("API-Key", api_key)] else [])
        ()
      in

      match Yojson.Safe.from_string response |> T.response_of_yojson with
      | Ok data -> return (Ok data)
      | Error msg -> return (Error (Error.Json_parse_error msg))
  end
end
```

### Endpoint Definition

```ocaml
module Account = struct
  module T = struct
    let name = "account"
    let endpoint = "account"
    let http_method = `GET
    let requires_auth = true

    type request = unit [@@deriving sexp]
    let request_to_params () = []

    type balance = {
      asset: string;
      free: float [@key "free"];
      locked: float;
    } [@@deriving sexp, of_yojson]

    type response = {
      balances: balance list;
      canTrade: bool;
    } [@@deriving sexp, of_yojson]
  end

  include T
  include Rest.Make_no_arg(T)
end
```

## WebSocket Streaming

### Connection Management

```ocaml
let connect ~url ~on_message =
  let%bind conn = Websocket_curl.connect url in

  (* Read loop *)
  let rec read_loop () =
    let%bind msg = Websocket_curl.read conn in
    on_message msg;
    read_loop ()
  in

  (* Spawn background reader *)
  don't_wait_for (read_loop ());

  return conn
```

### Message Processing

```ocaml
let process_messages ~conn ~parse ~validate =
  let r, w = Pipe.create () in

  let on_message msg =
    match parse msg with
    | Ok parsed ->
        (match validate parsed with
         | Ok valid -> Pipe.write_without_pushback w valid
         | Error _ -> (* Log and skip *))
    | Error _ -> (* Log parse error *)
  in

  let%bind _conn = connect ~url ~on_message in
  return r
```

## Type Safety

### JSON Serialization

Using `ppx_deriving_yojson`:

```ocaml
type order_response = {
  orderId: int64 [@key "orderId"];
  symbol: string;
  status: order_status;
  price: float;
  [@@@default None] executedQty: float option;
} [@@deriving sexp, yojson]

(* Automatic generation: *)
val order_response_of_yojson : Yojson.Safe.t -> (order_response, string) result
val yojson_of_order_response : order_response -> Yojson.Safe.t
```

### Polymorphic Variants

```ocaml
type side = [`Buy | `Sell] [@@deriving sexp]

(* Pattern matching ensures exhaustiveness *)
let side_to_string = function
  | `Buy -> "BUY"
  | `Sell -> "SELL"
```

### Phantom Types

```ocaml
type 'a price_map
type bid
type ask

val bid_map : bid price_map
val ask_map : ask price_map
(* Prevents mixing bid/ask maps at compile time *)
```

## Concurrency Model

### Async Pipelines

```ocaml
(* Multi-exchange aggregation *)
let consolidated_book ~exchanges =
  let pipes = List.map exchanges ~f:(fun ex ->
    exchange_pipe ex
  ) in

  let merged = Pipe.interleave pipes in

  Pipe.fold merged ~init:empty_book ~f:(fun book update ->
    update_consolidated_book book update
  )
```

### Background Processing

```ocaml
(* Spawn background task *)
don't_wait_for (
  let%bind () = long_running_task () in
  update_state new_value;
  return ()
);

(* Continue main flow *)
```

### Error Handling

```ocaml
Monitor.try_with (fun () ->
  risky_operation ()
)
>>| function
| Ok result -> handle_success result
| Error exn -> handle_error exn
```

## Performance Considerations

### Allocation

- Immutable updates create new books, but share unchanged maps
- Structural sharing in Map reduces allocation
- Use `Pipe.write_without_pushback` when possible

### Batching

```ocaml
(* Batch multiple updates *)
let book = Book.set_many book [
  (`Bid, 50000., 1.0);
  (`Bid, 49999., 0.5);
  (`Ask, 50001., 1.5);
]
```

### Caching

```ocaml
(* Memoize expensive calculations *)
let mid_price_cached = lazy (calculate_mid_price book)
```

## See Also

- [Order Book Guide](ORDER_BOOK.md)
- [WebSocket Guide](WEBSOCKET.md)
- [Authentication Guide](AUTHENTICATION.md)
- [Testing Guide](TESTING.md)
