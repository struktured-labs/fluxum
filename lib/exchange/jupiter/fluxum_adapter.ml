(** Jupiter Solana DEX Aggregator Adapter

    Partial implementation of Exchange_intf.S for Jupiter DEX aggregator.

    {b Status:} MARKET DATA ONLY

    {b Features:}
    - ✅ REST market data (ticker via quote API)
    - ✅ Synthetic order book from quotes
    - ✅ Token list and routing information
    - ❌ Trading operations (requires Solana wallet signing)

    {b Architecture:}
    - DEX aggregator, not traditional exchange
    - Routes through 20+ Solana DEXs (Raydium, Orca, Whirlpool, etc.)
    - No persistent order book - quotes computed on-demand
    - Split trades across multiple routes for best execution
    - Solana blockchain for settlement

    {b Authentication:}
    - Public endpoints: No authentication required
    - Trading: Requires Solana wallet private key (not implemented)
    - No traditional API keys

    {b Rate Limits:}
    - Free tier: 600 requests/minute
    - Pro tier: Higher limits available
    - No documented WebSocket rate limits

    {b Symbol Format:}
    - Solana mint addresses: ["So11111..."] (SOL), ["EPjFW..."] (USDC)
    - Trading pairs represented as (input_mint, output_mint)
    - Use token list API for human-readable symbols

    {b Known Limitations:}
    - ❌ No REST trading (requires blockchain integration)
    - ❌ No WebSocket support (REST only)
    - ❌ No order book depth (only top of book from quotes)
    - Quotes expire quickly (5-30 seconds)
    - Slippage varies with liquidity depth
    - Price impact calculation required for large orders

    {b Trading Implementation Plan:}
    - Phase 1: Market data ✅ (complete)
    - Phase 2: Solana wallet integration (pending)
    - Phase 3: Transaction building and signing (pending)
    - Phase 4: Swap execution and monitoring (pending)

    @see <https://station.jup.ag/docs/apis/swap-api> Jupiter Swap API Documentation
    @see <https://station.jup.ag/docs/apis/price-api-v2> Jupiter Price API Documentation
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

    module Candle = struct
      type t = unit  (* Jupiter doesn't provide candles *)
    end

    module Symbol_info = struct
      type t = Rest.Types.token_info
    end

    module Error = struct
      type t = Rest.Error.t
    end

    (** Account operations - deposits/withdrawals (stubs)
        Note: Jupiter is a Solana DEX aggregator - deposits/withdrawals happen on-chain *)
    module Deposit_address = struct
      type t = unit
    end

    module Deposit = struct
      type t = unit
    end

    module Withdrawal = struct
      type t = unit
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

  let get_candles (_ : t) ~symbol:_ ~timeframe:_ ?since:_ ?until:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Jupiter doesn't provide historical candle data"))

  (** {2 Account Operations - Deposits/Withdrawals (Stubs)}
      Note: Jupiter is a Solana DEX aggregator - deposits/withdrawals happen on-chain *)

  let get_deposit_address (_ : t) ~currency:_ ?network:_ () =
    Deferred.return (Error (`Api_error "Jupiter is a Solana DEX - use wallet address for deposits"))

  let withdraw (_ : t) ~currency:_ ~amount:_ ~address:_ ?tag:_ ?network:_ () =
    Deferred.return (Error (`Api_error "Jupiter is a Solana DEX - use Solana wallet for transfers"))

  let get_deposits (_ : t) ?currency:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Jupiter is a Solana DEX - use blockchain explorer"))

  let get_withdrawals (_ : t) ?currency:_ ?limit:_ () =
    Deferred.return (Error (`Api_error "Jupiter is a Solana DEX - use blockchain explorer"))

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
    let order_response (_ : Native.Order.response) : (Types.Order.t, string) Result.t =
      Error "Jupiter is a Solana DEX aggregator - use swap/quote APIs instead of order tracking"

    let order_status (_ : Native.Order.status) : (Types.Order_status.t, string) Result.t =
      Error "Jupiter is a Solana DEX aggregator - use swap/quote APIs instead of order tracking"

    let order_from_status (_ : Native.Order.status) : (Types.Order.t, string) Result.t =
      Error "Jupiter is a Solana DEX aggregator - use swap/quote APIs instead of order tracking"

    let trade (_ : Native.Trade.t) : (Types.Trade.t, string) Result.t =
      Error "Jupiter is a Solana DEX aggregator - use swap/quote APIs instead of trade tracking"

    let balance (_ : Native.Balance.t) : (Types.Balance.t, string) Result.t =
      Error "Jupiter is a Solana DEX aggregator - balances are on-chain, use Solana wallet APIs"

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

    let candle (_ : Native.Candle.t) : (Types.Candle.t, string) Result.t =
      Error "Jupiter doesn't provide candle data"

    (** Account operations normalization (stubs) *)
    let deposit_address (_ : Native.Deposit_address.t) : (Types.Deposit_address.t, string) Result.t =
      Error "Jupiter is a Solana DEX - use wallet address"

    let deposit (_ : Native.Deposit.t) : (Types.Deposit.t, string) Result.t =
      Error "Jupiter is a Solana DEX - use blockchain explorer"

    let withdrawal (_ : Native.Withdrawal.t) : (Types.Withdrawal.t, string) Result.t =
      Error "Jupiter is a Solana DEX - use blockchain explorer"

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
