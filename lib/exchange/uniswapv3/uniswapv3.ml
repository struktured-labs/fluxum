(** Uniswap V3 Exchange Module *)

open Core
open Async

module Cfg = Cfg
module Uni_types = Types
module Rest = Rest
module Pool_adapter = Pool_adapter
module Order_book = Order_book
module Ledger = Ledger
module Session = Session
module Swap_router = Swap_router
module Unified_adapter = Unified_adapter
module Fluxum_adapter = Fluxum_adapter

let cfg = Cfg.production

let pools_command : string * Command.t =
  ("pools", Command.async
    ~summary:"List top Uniswap V3 pools by volume"
    (let%map_open.Command first = flag "--first" (optional_with_default 10 int)
        ~doc:"INT Number of pools (default: 10)"
     in
     fun () ->
       Rest.pools ~cfg ~first () >>| function
       | Ok pools ->
         printf "Top %d Uniswap V3 Pools:\n\n" (List.length pools);
         printf "%-44s %-8s %-8s %8s %15s %10s\n"
           "Pool ID" "Token0" "Token1" "Fee" "Volume USD" "Tx Count";
         printf "%s\n" (String.make 100 '-');
         List.iter pools ~f:(fun (p : Uni_types.pool) ->
           let fee_pct = Float.of_int p.feeTier /. 10000.0 in
           printf "%-44s %-8s %-8s %7.2f%% %15.0f %10d\n"
             p.id p.token0.symbol p.token1.symbol fee_pct p.volumeUSD p.txCount)
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.sexp_of_error e))))

let pool_info_command : string * Command.t =
  ("pool-info", Command.async
    ~summary:"Detailed info for a specific pool"
    (let%map_open.Command pool_id = flag "--pool-id" (required string)
        ~doc:"STRING Pool address (e.g. 0x8ad599c3...)"
     in
     fun () ->
       Rest.pool_by_id ~cfg ~pool_id >>| function
       | Ok p ->
         printf "Pool: %s\n" p.id;
         printf "  Pair: %s / %s\n" p.token0.symbol p.token1.symbol;
         printf "  Token0: %s (%s, %d decimals)\n" p.token0.name p.token0.id p.token0.decimals;
         printf "  Token1: %s (%s, %d decimals)\n" p.token1.name p.token1.id p.token1.decimals;
         printf "  Fee tier: %d (%.2f%%)\n" p.feeTier (Float.of_int p.feeTier /. 10000.0);
         printf "  Liquidity: %s\n" p.liquidity;
         printf "  sqrtPrice: %s\n" p.sqrtPrice;
         printf "  Current tick: %d\n" p.tick;
         printf "  Volume USD: %.2f\n" p.volumeUSD;
         printf "  Tx count: %d\n" p.txCount;
         (match Pool_common.Concentrated.price_from_sqrt_price_x96
                  ~sqrt_price_x96:p.sqrtPrice ~decimals0:p.token0.decimals ~decimals1:p.token1.decimals with
          | Ok price ->
            printf "  Spot price (%s/%s): %.8f\n" p.token0.symbol p.token1.symbol price;
            (match Float.(price > 0.) with
             | true -> printf "  Spot price (%s/%s): %.8f\n" p.token1.symbol p.token0.symbol (1.0 /. price)
             | false -> ())
          | Error msg -> printf "  Price error: %s\n" msg)
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.sexp_of_error e))))

let price_command : string * Command.t =
  ("price", Command.async
    ~summary:"Spot price for a pool"
    (let%map_open.Command pool_id = flag "--pool-id" (required string)
        ~doc:"STRING Pool address"
     in
     fun () ->
       Rest.pool_by_id ~cfg ~pool_id >>| function
       | Ok p ->
         (match Pool_common.Concentrated.price_from_sqrt_price_x96
                  ~sqrt_price_x96:p.sqrtPrice ~decimals0:p.token0.decimals ~decimals1:p.token1.decimals with
          | Ok price ->
            printf "%s/%s: %.8f\n" p.token0.symbol p.token1.symbol price;
            (match Float.(price > 0.) with
             | true -> printf "%s/%s: %.8f\n" p.token1.symbol p.token0.symbol (1.0 /. price)
             | false -> ())
          | Error msg -> eprintf "Price error: %s\n" msg)
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.sexp_of_error e))))

let quote_command : string * Command.t =
  ("quote", Command.async
    ~summary:"Get swap quote with price impact"
    (let%map_open.Command pool_id = flag "--pool-id" (required string)
        ~doc:"STRING Pool address"
     and amount = flag "--amount" (required float)
        ~doc:"FLOAT Amount of input token"
     and token_in = flag "--from" (required string)
        ~doc:"STRING Input token symbol"
     and token_out = flag "--to" (required string)
        ~doc:"STRING Output token symbol"
     in
     fun () ->
       Rest.pool_by_id ~cfg ~pool_id >>| function
       | Ok pool ->
         (match Pool_adapter.quote pool ~amount_in:amount ~token_in ~token_out with
          | Ok q ->
            printf "Quote: %.6f %s -> %.6f %s\n" amount token_in q.amount_out token_out;
            printf "  Effective price: %.8f\n" q.effective_price;
            printf "  Price impact: %.4f%%\n" q.price_impact_pct;
            printf "  Fee: %.6f\n" q.fee_amount
          | Error msg -> eprintf "Quote error: %s\n" msg)
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.sexp_of_error e))))

let recent_swaps_command : string * Command.t =
  ("recent-swaps", Command.async
    ~summary:"Recent swaps for a pool"
    (let%map_open.Command pool_id = flag "--pool-id" (required string)
        ~doc:"STRING Pool address"
     and first = flag "--first" (optional_with_default 20 int)
        ~doc:"INT Number of swaps (default: 20)"
     in
     fun () ->
       Rest.recent_swaps ~cfg ~pool_id ~first () >>| function
       | Ok swaps ->
         printf "Recent Swaps (%d):\n\n" (List.length swaps);
         printf "%-68s %12s %12s %12s\n" "Swap ID" "Amount0" "Amount1" "USD";
         printf "%s\n" (String.make 110 '-');
         List.iter swaps ~f:(fun (s : Uni_types.swap) ->
           printf "%-68s %12s %12s %12.2f\n" s.id s.amount0 s.amount1 s.amountUSD)
       | Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.sexp_of_error e))))

let orderbook_command : string * Command.t =
  ("orderbook", Command.async
    ~summary:"Virtual order book from tick data"
    (let%map_open.Command pool_id = flag "--pool-id" (required string)
        ~doc:"STRING Pool address"
     and depth = flag "--depth" (optional_with_default 10 int)
        ~doc:"INT Number of levels per side (default: 10)"
     in
     fun () ->
       let%bind pool_result = Rest.pool_by_id ~cfg ~pool_id in
       let%bind tick_result = Rest.pool_ticks ~cfg ~pool_id () in
       match pool_result, tick_result with
       | Ok pool, Ok ticks ->
         let book = Order_book.Book.of_pool_and_ticks ~pool ~ticks in
         let symbol = sprintf "%s/%s" pool.token0.symbol pool.token1.symbol in
         printf "Order Book: %s (pool: %s)\n\n" symbol pool_id;
         let asks = Order_book.Book.best_n_asks book ~n:depth () |> List.rev in
         printf "  ASKS:\n";
         List.iter asks ~f:(fun (level : Exchange_common.Order_book_base.Price_level.t) ->
           printf "    %.8f  |  %.6f\n" level.price level.volume);
         let mid = Order_book.Book.mid_price book in
         let spread = Order_book.Book.spread book in
         printf "  --- Mid: %.8f  Spread: %.8f ---\n" mid spread;
         printf "  BIDS:\n";
         let bids = Order_book.Book.best_n_bids book ~n:depth () in
         List.iter bids ~f:(fun (level : Exchange_common.Order_book_base.Price_level.t) ->
           printf "    %.8f  |  %.6f\n" level.price level.volume);
         Deferred.unit
       | Error e, _ ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.sexp_of_error e));
         Deferred.unit
       | _, Error e ->
         eprintf "Error: %s\n" (Sexp.to_string_hum (Rest.sexp_of_error e));
         Deferred.unit))

let session_command : string * Command.t =
  ("session", Command.async
    ~summary:"Full session with all event streams"
    (let%map_open.Command pool_id = flag "--pool-id" (required string)
        ~doc:"STRING Pool address to monitor"
     in
     fun () ->
       let%bind session = Session.create ~cfg ~symbols:[pool_id] () in
       printf "Uniswap V3 session started for pool: %s\n" pool_id;
       printf "Press Ctrl+C to stop...\n\n";
       let events = Session.events session in
       let market_pipes = Session.Events.market_data events in
       (match Map.find market_pipes pool_id with
        | Some pipe ->
          don't_wait_for (
            Pipe.iter pipe ~f:(fun json ->
              printf "[market] %s\n" (Yojson.Safe.to_string json);
              Deferred.unit))
        | None -> ());
       let book_pipes = Session.Events.order_books events in
       (match Map.find book_pipes pool_id with
        | Some pipe ->
          don't_wait_for (
            Pipe.iter pipe ~f:(fun result ->
              (match result with
               | Ok book ->
                 let bid = Order_book.Book.best_bid book in
                 let ask = Order_book.Book.best_ask book in
                 printf "[book] bid=%.8f ask=%.8f mid=%.8f spread=%.8f\n"
                   bid.price ask.price
                   (Order_book.Book.mid_price book)
                   (Order_book.Book.spread book)
               | Error err ->
                 printf "[book] error: %s\n" err);
              Deferred.unit))
        | None -> ());
       Deferred.never ()))

let swap_command : string * Command.t =
  ("swap", Command.async
    ~summary:"Execute a token swap via SwapRouter02"
    (let%map_open.Command pool_id = flag "--pool-id" (required string)
        ~doc:"STRING Pool address"
     and amount = flag "--amount" (required float)
        ~doc:"FLOAT Amount of input token"
     and token_in = flag "--from" (required string)
        ~doc:"STRING Input token symbol"
     and token_out = flag "--to" (required string)
        ~doc:"STRING Output token symbol"
     and slippage = flag "--slippage" (optional_with_default 0.5 float)
        ~doc:"FLOAT Max slippage in percent (default: 0.5)"
     and dry_run = flag "--dry-run" no_arg
        ~doc:" Show quote without executing"
     in
     fun () ->
       let cfg = Cfg.of_env () in
       (* Validate wallet config *)
       (match cfg.wallet_address, cfg.private_key_hex with
        | None, _ ->
          eprintf "Error: UNISWAP_WALLET_ADDRESS not set\n";
          Deferred.unit
        | _, None ->
          eprintf "Error: UNISWAP_PRIVATE_KEY not set\n";
          Deferred.unit
        | Some wallet, Some _key ->
          let%bind pool_result = Rest.pool_by_id ~cfg ~pool_id in
          (match pool_result with
           | Error e ->
             eprintf "Error fetching pool: %s\n" (Sexp.to_string_hum (Rest.sexp_of_error e));
             Deferred.unit
           | Ok pool ->
             (* Resolve token addresses from symbols *)
             let token_in_addr, token_out_addr, decimals_in, decimals_out =
               match String.equal (String.uppercase token_in) (String.uppercase pool.token0.symbol) with
               | true ->
                 pool.token0.id, pool.token1.id, pool.token0.decimals, pool.token1.decimals
               | false ->
                 pool.token1.id, pool.token0.id, pool.token1.decimals, pool.token0.decimals
             in
             (* Get and show quote first *)
             (match Pool_adapter.quote pool ~amount_in:amount ~token_in ~token_out with
              | Ok q ->
                printf "Swap Quote:\n";
                printf "  Pool: %s (%s/%s, fee: %.2f%%)\n" pool_id pool.token0.symbol pool.token1.symbol
                  (Float.of_int pool.feeTier /. 10000.0);
                printf "  Input:  %.6f %s (%s)\n" amount token_in token_in_addr;
                printf "  Output: %.6f %s (%s)\n" q.amount_out token_out token_out_addr;
                printf "  Effective price: %.8f\n" q.effective_price;
                printf "  Price impact: %.4f%%\n" q.price_impact_pct;
                printf "  Max slippage: %.2f%%\n" slippage;
                let min_out = q.amount_out *. (1.0 -. slippage /. 100.0) in
                printf "  Min output: %.6f %s\n" min_out token_out;
                printf "  Fee: %.6f\n\n" q.fee_amount;
                (match dry_run with
                 | true ->
                   printf "[Dry run - no transaction submitted]\n";
                   Deferred.unit
                 | false ->
                   let slippage_bps = Float.to_int (slippage *. 100.0) in
                   let amount_in_hex = Ethereum.Abi.uint256_of_float ~decimals:decimals_in amount in
                   let amount_out_min_hex = Ethereum.Abi.uint256_of_float ~decimals:decimals_out min_out in
                   let params : Swap_router.exact_input_single_params = {
                     token_in = token_in_addr;
                     token_out = token_out_addr;
                     fee = pool.feeTier;
                     recipient = wallet;
                     amount_in = amount_in_hex;
                     amount_out_minimum = amount_out_min_hex;
                     sqrt_price_limit_x96 = "0";
                   } in
                   let _ = slippage_bps in
                   printf "Submitting swap transaction...\n";
                   let%bind result = Swap_router.exact_input_single ~cfg ~params in
                   (match result with
                    | Ok tx_hash ->
                      printf "Transaction submitted: %s\n" tx_hash;
                      printf "Waiting for confirmation...\n";
                      let%bind receipt = Swap_router.wait_for_receipt ~cfg ~tx_hash () in
                      (match receipt with
                       | Ok (Some _receipt) ->
                         printf "Transaction confirmed!\n";
                         Deferred.unit
                       | Ok None ->
                         printf "No receipt yet (timeout)\n";
                         Deferred.unit
                       | Error e ->
                         eprintf "Error waiting for receipt: %s\n"
                           (Sexp.to_string_hum (Swap_router.sexp_of_error e));
                         Deferred.unit)
                    | Error e ->
                      eprintf "Swap failed: %s\n"
                        (Sexp.to_string_hum (Swap_router.sexp_of_error e));
                      Deferred.unit))
              | Error msg ->
                eprintf "Quote error: %s\n" msg;
                Deferred.unit)))))

let command : Command.t =
  Command.group ~summary:"Uniswap V3 DEX Commands"
    [ pools_command
    ; pool_info_command
    ; price_command
    ; quote_command
    ; recent_swaps_command
    ; orderbook_command
    ; session_command
    ; swap_command
    ]
