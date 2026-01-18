(** Jupiter Solana DEX Aggregator Fluxum Adapter

    Jupiter is a DEX aggregator, not a traditional exchange.
    Key differences:
    - No persistent order book (quotes are computed on-demand)
    - Swaps route through multiple DEXs for best pricing
    - Trading requires Solana wallet signing (not API keys)

    Public market data:
    - get_ticker: Uses quote API to get current prices
    - get_order_book: Synthetic from quote (best bid/ask)

    Trading:
    - Requires Solana wallet and transaction signing
    - Use quote + swap endpoints to build transactions
*)

open Core
open Async

module Types = Fluxum.Types
module Exchange_intf = Fluxum.Exchange_intf

module V1 = Rest

(* Suppress unused warnings *)
[@@@warning "-32-69"]

module Adapter = struct
  type t =
    { cfg : (module Cfg.S)
    ; symbols : (string * string) list  (* (base_mint, quote_mint) pairs *)
    }

  let create ~cfg ?(symbols = []) () =
    { cfg; symbols }

  module Venue = struct
    let t = Types.Venue.Jupiter
  end

  module Native = struct
    module Order = struct
      type id = string  (* Transaction signature *)
      type request = Rest.Types.quote
      type response = Rest.Types.swap_transaction
      type status = unit  (* No order tracking on DEX *)
    end

    module Trade = struct
      type t = unit  (* Trades are on-chain, not via API *)
    end

    module Balance = struct
      type t = unit  (* Balances are on Solana, use RPC *)
    end

    module Book = struct
      type update = unit
      type snapshot = Rest.Types.quote * Rest.Types.quote  (* sell quote, buy quote *)
    end

    module Ticker = struct
      type t = Rest.Types.quote  (* Quote represents current pricing *)
    end

    module Public_trade = struct
      type t = unit  (* Trades are on-chain *)
    end

    module Symbol_info = struct
      type t = Rest.Types.token_info
    end

    module Error = struct
      type t = Rest.Error.t
    end
  end

  (* Trading operations - require Solana wallet *)
  let place_order (_ : t) (_ : Native.Order.request) =
    Deferred.return (Error (`Api_error "Jupiter swaps require Solana wallet signing"))

  let cancel_order (_ : t) (_ : Native.Order.id) =
    Deferred.return (Error (`Api_error "Jupiter trades are atomic swaps, no cancellation"))

  let balances (_ : t) =
    Deferred.return (Error (`Api_error "Balances require Solana RPC, not Jupiter API"))

  let get_order_status (_ : t) (_ : Native.Order.id) =
    Deferred.return (Error (`Api_error "Use Solana RPC to track transaction status"))

  let get_open_orders (_ : t) ?symbol:_ () =
    Deferred.return (Error (`Api_error "Jupiter is a DEX aggregator, no persistent orders"))

  let get_order_history (_ : t) ?symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Order history is on Solana blockchain"))

  let get_my_trades (_ : t) ~symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Trade history is on Solana blockchain"))

  let cancel_all_orders (_ : t) ?symbol:_ () =
    Deferred.return (Error (`Api_error "Jupiter trades are atomic swaps"))

  (* Public market data *)
  let get_symbols (_ : t) () =
    (* Jupiter doesn't have a symbols endpoint - any SPL token can be traded *)
    Deferred.return (Error (`Api_error "Jupiter supports all SPL tokens, use token list API"))

  let get_ticker (t : t) ~symbol () =
    (* Parse symbol as "BASE-QUOTE" or use default mints *)
    let (base_mint, quote_mint) = match String.lsplit2 symbol ~on:'-' with
      | Some ("SOL", "USDC") -> (Rest.Tokens.sol, Rest.Tokens.usdc)
      | Some ("SOL", "USDT") -> (Rest.Tokens.sol, Rest.Tokens.usdt)
      | Some ("JUP", "USDC") -> (Rest.Tokens.jup, Rest.Tokens.usdc)
      | Some ("BONK", "USDC") -> (Rest.Tokens.bonk, Rest.Tokens.usdc)
      | _ ->
        (* Assume symbol is "baseMint-quoteMint" *)
        match String.lsplit2 symbol ~on:'-' with
        | Some (base, quote) -> (base, quote)
        | None -> (symbol, Rest.Tokens.usdc)
    in
    (* Get quote for 1 unit of base token (varies by decimals) *)
    let amount = 1_000_000_000 in  (* 1 SOL in lamports *)
    Rest.quote t.cfg ~input_mint:base_mint ~output_mint:quote_mint ~amount ()

  let get_order_book (t : t) ~symbol ?limit:_ () =
    let (base_mint, quote_mint) = match String.lsplit2 symbol ~on:'-' with
      | Some ("SOL", "USDC") -> (Rest.Tokens.sol, Rest.Tokens.usdc)
      | Some ("SOL", "USDT") -> (Rest.Tokens.sol, Rest.Tokens.usdt)
      | Some ("JUP", "USDC") -> (Rest.Tokens.jup, Rest.Tokens.usdc)
      | Some ("BONK", "USDC") -> (Rest.Tokens.bonk, Rest.Tokens.usdc)
      | _ ->
        match String.lsplit2 symbol ~on:'-' with
        | Some (base, quote) -> (base, quote)
        | None -> (symbol, Rest.Tokens.usdc)
    in
    let amount = 1_000_000_000 in
    Rest.order_book t.cfg ~base_mint ~quote_mint ~amount

  let get_recent_trades (_ : t) ~symbol:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Recent trades are on Solana blockchain"))

  module Streams = struct
    let trades (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r

    let book_updates (_ : t) =
      let r, _w = Pipe.create () in
      Deferred.return r
  end

  module Normalize = struct
    let order_response (_ : Native.Order.response) : Types.Order.t =
      { venue = Venue.t
      ; id = ""
      ; symbol = ""
      ; side = Types.Side.Buy
      ; kind = Types.Order_kind.Market
      ; qty = 0.
      ; filled = 0.
      ; status = Types.Order_status.New
      ; created_at = None
      ; updated_at = None
      }

    let order_status (() : Native.Order.status) : Types.Order_status.t =
      Types.Order_status.Filled  (* Swaps are atomic *)

    let order_from_status (() : Native.Order.status) : Types.Order.t =
      order_response (Rest.Types.{
        swapTransaction = "";
        lastValidBlockHeight = 0;
        prioritizationFeeLamports = None;
      })

    let trade (() : Native.Trade.t) : Types.Trade.t =
      { venue = Venue.t
      ; symbol = ""
      ; side = Types.Side.Buy
      ; price = 0.
      ; qty = 0.
      ; fee = None
      ; trade_id = None
      ; ts = None
      }

    let balance (() : Native.Balance.t) : Types.Balance.t =
      { venue = Venue.t
      ; currency = ""
      ; total = 0.
      ; available = 0.
      ; locked = 0.
      }

    let book_update (() : Native.Book.update) : Types.Book_update.t =
      { venue = Venue.t
      ; symbol = ""
      ; side = Types.Book_update.Side.Bid
      ; levels = []
      ; ts = None
      ; is_snapshot = true
      }

    let symbol_info (info : Native.Symbol_info.t) : Types.Symbol_info.t =
      { venue = Venue.t
      ; symbol = info.symbol
      ; base_currency = info.symbol
      ; quote_currency = "USD"
      ; status = "active"
      ; min_order_size = 0.
      ; tick_size = None
      ; quote_increment = None
      }

    let ticker (quote : Native.Ticker.t) : (Types.Ticker.t, string) Result.t =
      (* Calculate price from quote amounts *)
      let open Result.Let_syntax in
      let%bind in_amount = Fluxum.Normalize_common.Float_conv.qty_of_string quote.inAmount in
      let%bind out_amount = Fluxum.Normalize_common.Float_conv.qty_of_string quote.outAmount in
      let price = out_amount /. in_amount in
      let symbol = sprintf "%s-%s"
        (String.prefix quote.inputMint 4)
        (String.prefix quote.outputMint 4)
      in
      Ok ({ venue = Venue.t
      ; symbol
      ; last_price = price
      ; bid_price = price  (* Same as mid in aggregator *)
      ; ask_price = price
      ; high_24h = 0.
      ; low_24h = 0.
      ; volume_24h = 0.
      ; quote_volume = None
      ; price_change = None
      ; price_change_pct = None
      ; ts = None
      } : Types.Ticker.t)

    let order_book ((sell_quote, buy_quote) : Native.Book.snapshot) : (Types.Order_book.t, string) Result.t =
      (* Convert quotes to synthetic order book *)
      let open Result.Let_syntax in
      let%bind sell_out = Fluxum.Normalize_common.Float_conv.qty_of_string sell_quote.outAmount in
      let%bind sell_in = Fluxum.Normalize_common.Float_conv.qty_of_string sell_quote.inAmount in
      let%bind buy_in = Fluxum.Normalize_common.Float_conv.qty_of_string buy_quote.inAmount in
      let%bind buy_out = Fluxum.Normalize_common.Float_conv.qty_of_string buy_quote.outAmount in
      let sell_price = sell_out /. sell_in in
      let buy_price = buy_in /. buy_out in
      let bid = { Types.Order_book.Price_level.
        price = buy_price;
        volume = buy_out;
      } in
      let ask = { Types.Order_book.Price_level.
        price = sell_price;
        volume = sell_in;
      } in
      Ok ({ venue = Venue.t
      ; symbol = ""
      ; bids = [bid]
      ; asks = [ask]
      ; ts = None
      ; epoch = 0
      } : Types.Order_book.t)

    let public_trade (() : Native.Public_trade.t) : Types.Public_trade.t =
      { venue = Venue.t
      ; symbol = ""
      ; price = 0.
      ; qty = 0.
      ; side = None
      ; trade_id = None
      ; ts = None
      }

    let error (e : Native.Error.t) : Types.Error.t =
      match e with
      | `Http (code, msg) ->
        Types.Error.Exchange_specific { venue = Venue.t; code = Int.to_string code; message = msg }
      | `Json_parse msg ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "json"; message = msg }
      | `Api_error msg ->
        Types.Error.Exchange_specific { venue = Venue.t; code = "api"; message = msg }
      | `Unauthorized ->
        Types.Error.Auth_failed
  end
end

module Builder = struct
  module E = Adapter

  let make_order_request ~symbol:_ ~side:_ ~kind:_ ~qty:_ =
    (* Jupiter trading requires wallet signing, not API *)
    Rest.Types.{
      inputMint = "";
      inAmount = "0";
      outputMint = "";
      outAmount = "0";
      otherAmountThreshold = "0";
      swapMode = "ExactIn";
      slippageBps = 50;
      priceImpactPct = "0";
      routePlan = [];
      contextSlot = None;
      timeTaken = None;
    }
end
