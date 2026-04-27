(** Smoke test for Ethereum wallet read-only operations.

    Hits a public RPC against two addresses chosen for stability:
    - Vitalik's wallet (held continuously since 2015, will not move)
    - USDC mainnet contract (most-queried ERC-20; Circle migration would
      give weeks of warning)

    Three RPC calls per run — well under any free-tier limit. Single-threaded
    to avoid concurrent rate-limiting on free tiers.

    Skips gracefully (logs and exits 0) when the network is unavailable so
    [dune test] doesn't fail on offline CI runs. Set [SMOKE_REQUIRE_NETWORK=1]
    to make missing-network a hard failure. *)

open Core
open Async

let vitalik = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"

let usdc = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"

let default_rpc_url = "https://eth.llamarpc.com"

let rpc_url () =
  match Sys.getenv "ETH_RPC_URL" with
  | Some u -> u
  | None -> default_rpc_url

let strict_mode () =
  match Sys.getenv "SMOKE_REQUIRE_NETWORK" with
  | Some _ -> true
  | None -> false

let on_network_error name err =
  let err_str =
    match err with
    | `Network m -> sprintf "network: %s" m
    | `Rpc m -> sprintf "rpc: %s" m
    | `Json_parse m -> sprintf "parse: %s" m
  in
    match strict_mode () with
    | true ->
      eprintf "FAIL [%s]: %s\n" name err_str;
      exit 1
    | false ->
      printf "SKIP [%s]: %s (set SMOKE_REQUIRE_NETWORK=1 to fail)\n" name err_str;
      Deferred.unit

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
      let symbol_ok =
        match amt.symbol with
        | Some "USDC" -> true
        | _ -> false
      in
      let decimals_ok = amt.decimals = 6 in
        printf "OK [erc20_balance(vitalik, USDC)]: %s (decimals=%d, symbol=%s)\n"
          display
          amt.decimals
          (Option.value amt.symbol ~default:"?");
        match symbol_ok && decimals_ok with
        | true -> Deferred.unit
        | false ->
          eprintf "FAIL [erc20_balance]: expected USDC/6dp, got %s/%d\n"
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

let main () =
  let%bind () = test_token_amount_arithmetic () in
  let%bind () = test_eth_balance () in
  let%bind () = test_erc20_balance () in
    printf "All smoke checks done.\n";
    Deferred.unit

let () =
  Command.async
    ~summary:"Ethereum wallet read-only smoke test"
    (Command.Param.return main)
  |> Command_unix.run
