(** Smoke test for Ethereum wallet read-only operations.

    Hits a public RPC against addresses chosen for stability:
    - Vitalik's wallet (held continuously since 2015, will not move)
    - USDC mainnet contract (most-queried ERC-20; Circle migration would
      give weeks of warning)

    Single-threaded to avoid concurrent rate-limiting on free tiers.

    Skips gracefully (logs and exits 0) when the network is unavailable so
    [dune test] doesn't fail on offline CI runs. Set [SMOKE_REQUIRE_NETWORK=1]
    to make missing-network a hard failure. *)

open Core
open Async

let vitalik = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"

let usdc = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
let usdt = "0xdAC17F958D2ee523a2206206994597C13D831ec7"
let weth = "0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2"

(* 1rpc.io handles Multicall3 batched calls cleanly without rate-limits;
   llamarpc 429s on follow-up batched calls (per fluxit empirical finding
   2026-04-27). *)
let default_rpc_url = "https://1rpc.io/eth"

let rpc_url () =
  match Sys.getenv "ETH_RPC_URL" with
  | Some u -> u
  | None -> default_rpc_url

let strict_mode () =
  match Sys.getenv "SMOKE_REQUIRE_NETWORK" with
  | Some _ -> true
  | None -> false

let format_rpc_error = function
  | `Network m -> sprintf "network: %s" m
  | `Rpc m -> sprintf "rpc: %s" m
  | `Json_parse m -> sprintf "parse: %s" m

let format_log_error = function
  | `Network m -> sprintf "network: %s" m
  | `Rpc m -> sprintf "rpc: %s" m
  | `Json_parse m -> sprintf "parse: %s" m
  | `Max_logs_per_call_exceeded m -> sprintf "log limit: %s" m

let on_error name err_str =
  match strict_mode () with
  | true ->
    eprintf "FAIL [%s]: %s\n" name err_str;
    exit 1
  | false ->
    printf "SKIP [%s]: %s (set SMOKE_REQUIRE_NETWORK=1 to fail)\n" name err_str;
    Deferred.unit

let on_network_error name err = on_error name (format_rpc_error err)
let on_log_error name err = on_error name (format_log_error err)

let test_eth_balance () =
  let rpc_url = rpc_url () in
    Ethereum.Wallet.eth_balance ~rpc_url ~address:vitalik
    >>= function
    | Error e -> on_network_error "eth_balance(vitalik)" e
    | Ok amt ->
      let display = Ethereum.Erc20.Token_amount.to_display amt in
      let f = Ethereum.Erc20.Token_amount.to_float amt in
        printf "OK [eth_balance(vitalik)]: %s (~%.4f ETH)\n" display f;
        match Float.( >= ) f 0. with
        | true -> Deferred.unit
        | false ->
          eprintf "FAIL [eth_balance(vitalik)]: negative balance %f\n" f;
          exit 1

let test_erc20_balance () =
  let rpc_url = rpc_url () in
    Ethereum.Wallet.erc20_balance ~rpc_url ~address:vitalik ~contract:usdc
    >>= function
    | Error e -> on_network_error "erc20_balance(vitalik, USDC)" e
    | Ok amt ->
      let display = Ethereum.Erc20.Token_amount.to_display amt in
      (* Symbol fetch can transiently fail on free-tier RPCs (rate-limited);
         the library gracefully degrades to None. Accept None OR Some "USDC"
         as valid; only fail on Some "<wrong>". *)
      let symbol_ok =
        match amt.symbol with
        | None | Some "USDC" -> true
        | Some _ -> false
      in
      let decimals_ok = amt.decimals = 6 in
        printf "OK [erc20_balance(vitalik, USDC)]: %s (decimals=%d, symbol=%s)\n"
          display
          amt.decimals
          (Option.value amt.symbol ~default:"none");
        match symbol_ok && decimals_ok with
        | true -> Deferred.unit
        | false ->
          eprintf "FAIL [erc20_balance]: expected USDC|none/6dp, got %s/%d\n"
            (Option.value amt.symbol ~default:"none")
            amt.decimals;
          exit 1

let test_token_amount_arithmetic () =
  (* Pure unit test — doesn't hit the network. *)
  let one_eth =
    Ethereum.Erc20.Token_amount.create
      ~raw_hex:"0xde0b6b3a7640000"  (* 10^18 wei *)
      ~decimals:18
      ~symbol:"ETH"
      ()
  in
  let s = Ethereum.Erc20.Token_amount.to_string_decimal one_eth in
  let f = Ethereum.Erc20.Token_amount.to_float one_eth in
    printf "OK [token_amount]: 1 ETH formatted as %S, float %f\n" s f;
    match String.equal s "1" && Float.equal f 1. with
    | true -> Deferred.unit
    | false ->
      eprintf "FAIL [token_amount]: expected (\"1\", 1.0), got (%S, %f)\n" s f;
      exit 1

let test_erc20_balances_multicall () =
  let rpc_url = rpc_url () in
  let contracts = [usdc; usdt; weth] in
    Ethereum.Wallet.erc20_balances ~rpc_url ~address:vitalik ~contracts
    >>= function
    | Error e ->
      on_network_error "erc20_balances(multicall3, vitalik, [USDC;USDT;WETH])" e
    | Ok amts ->
      let count_ok = List.length amts = 3 in
      let symbols =
        List.map amts ~f:(fun a -> Option.value a.symbol ~default:"?")
      in
      let decimals_list = List.map amts ~f:(fun a -> a.decimals) in
        printf
          "OK [erc20_balances(multicall3)]: count=%d, symbols=[%s], decimals=[%s]\n"
          (List.length amts)
          (String.concat ~sep:";" symbols)
          (String.concat ~sep:";"
             (List.map decimals_list ~f:Int.to_string));
        List.iter amts ~f:(fun a ->
          printf
            "  - %s: %s\n"
            (Option.value a.symbol ~default:"?")
            (Ethereum.Erc20.Token_amount.to_string_decimal a));
        (* Symbols come from the same Multicall3 batch, so they're either
           all-Some or all-None (no per-token races). Accept either. *)
        let symbol_pattern_ok =
          match symbols with
          | ["USDC"; "USDT"; "WETH"] | ["?"; "?"; "?"] -> true
          | _ -> false
        in
        let decimals_pattern_ok =
          match decimals_list with
          | [6; 6; 18] -> true
          | _ -> false
        in
          match count_ok && symbol_pattern_ok && decimals_pattern_ok with
          | true -> Deferred.unit
          | false ->
            eprintf
              "FAIL [erc20_balances]: expected [USDC;USDT;WETH]|[?;?;?] / [6;6;18], got mismatch\n";
            exit 1

(* Uniswap Universal Router — touches USDC in nearly every block via swaps.
   Picked for guaranteed non-zero transfer count in a small window so the
   dedup/sort invariant test actually exercises real data. Any persistently
   active address would do; this one is stable infrastructure unlikely to
   change. *)
let active_test_address = "0x000000000004444c5dc75cb358380d2e3de08a90"

let test_recent_transfers_invariants () =
  let rpc_url = rpc_url () in
  let%bind end_block_r = Ethereum.Rpc.eth_block_number ~rpc_url in
    match end_block_r with
    | Error e -> on_network_error "recent_transfers(setup)" e
    | Ok end_block ->
      (* 50-block window keeps the test under any free-tier log-cap while
         still typically yielding dozens of transfers for an active address. *)
      let from_block = Int.max 0 (end_block - 50) in
        Ethereum.Wallet.recent_transfers
          ~rpc_url
          ~address:active_test_address
          ~contract:usdc
          ~from_block
          ~to_block:end_block
          ()
        >>= function
        | Error e ->
          on_log_error "recent_transfers(active_test_address, USDC, last 50 blocks)" e
        | Ok transfers ->
          let n = List.length transfers in
          let keys =
            List.map transfers ~f:(fun (t : Ethereum.Wallet.Transfer.t) ->
              (t.block_number, t.tx_hash, t.log_index))
          in
          let unique_keys =
            List.dedup_and_sort
              keys
              ~compare:(fun (b1, h1, l1) (b2, h2, l2) ->
                match Int.compare b1 b2 with
                | 0 ->
                  (match String.compare h1 h2 with
                   | 0 -> Int.compare l1 l2
                   | c -> c)
                | c -> c)
          in
          let unique_count = List.length unique_keys in
          let sorted_ok =
            List.is_sorted
              transfers
              ~compare:(fun (a : Ethereum.Wallet.Transfer.t) b ->
                match Int.compare a.block_number b.block_number with
                | 0 -> Int.compare a.log_index b.log_index
                | n -> n)
          in
          let direction_ok =
            List.for_all transfers ~f:(fun (t : Ethereum.Wallet.Transfer.t) ->
              match t.direction with
              | `Incoming
              | `Outgoing
              | `Self -> true)
          in
            printf
              "OK [recent_transfers]: %d transfers, %d unique, sorted=%b, directions_ok=%b\n"
              n
              unique_count
              sorted_ok
              direction_ok;
            match n = unique_count && sorted_ok && direction_ok with
            | true -> Deferred.unit
            | false ->
              eprintf
                "FAIL [recent_transfers]: invariants broken (n=%d unique=%d sorted=%b directions_ok=%b)\n"
                n
                unique_count
                sorted_ok
                direction_ok;
              exit 1

let main () =
  let%bind () = test_token_amount_arithmetic () in
  let%bind () = test_eth_balance () in
  let%bind () = test_erc20_balance () in
  let%bind () = test_erc20_balances_multicall () in
  let%bind () = test_recent_transfers_invariants () in
    printf "All smoke checks done.\n";
    Deferred.unit

let () =
  Command.async
    ~summary:"Ethereum wallet read-only smoke test"
    (Command.Param.return main)
  |> Command_unix.run
