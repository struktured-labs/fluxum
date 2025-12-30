(** Binance Integration Tests - Testnet/Sandbox

    Tests against Binance testnet (testnet.binance.vision).
    Requires BINANCE_TESTNET_API_KEY and BINANCE_TESTNET_API_SECRET env vars.

    Run with:
      BINANCE_TESTNET_API_KEY=xxx BINANCE_TESTNET_API_SECRET=yyy dune exec lib/exchange/binance/test/integration_tests.exe
*)

open Core
open Async
open Integration_test_framework

let test_symbol = "BTCUSDT"

(** Create testnet config *)
let get_config () =
  match Sys.getenv "BINANCE_TESTNET_API_KEY", Sys.getenv "BINANCE_TESTNET_API_SECRET" with
  | Some api_key, Some api_secret ->
    Ok (module struct
      let api_key = api_key
      let api_secret = api_secret
      let base_url = "testnet.binance.vision"
      let ws_url = "wss://testnet.binance.vision/ws"
    end : Binance.Cfg.S)
  | _ ->
    Error "BINANCE_TESTNET_API_KEY and BINANCE_TESTNET_API_SECRET must be set"

(** Public endpoint tests (no auth required) *)
let public_tests = [
  ("server_time", fun () ->
    match get_config () with
    | Error _ as e -> return e
    | Ok _config ->
      (* Note: server_time doesn't need auth, use production config *)
      let config = Binance.Cfg.of_string "production" in
      Binance.V3.Server_time.request config () >>| function
      | `Ok response ->
        (* Verify timestamp is recent (within last minute) *)
        let now_ms = Time_float_unix.now () |> Time_float_unix.to_span_since_epoch |> Time_float_unix.Span.to_ms |> Int64.of_float in
        let diff_ms = Int64.(abs (now_ms - response.serverTime)) in
        if Int64.(diff_ms < 60_000L) then
          Ok ()
        else
          Error (sprintf "Server time too far off: %Ld ms" diff_ms)
      | `Bad_request msg -> Error (sprintf "Bad request: %s" msg)
      | `Not_found -> Error "Not found"
      | `Unauthorized msg -> Error (sprintf "Unauthorized: %s" msg)
      | `Forbidden msg -> Error (sprintf "Forbidden: %s" msg)
      | `Too_many_requests msg -> Error (sprintf "Too many requests: %s" msg)
      | `Service_unavailable msg -> Error (sprintf "Service unavailable: %s" msg)
      | `Json_parse_error { message; body } -> Error (sprintf "JSON parse error: %s (body: %s)" message body)
      | `Api_error { code; msg } -> Error (sprintf "API error %d: %s" code msg));

  ("exchange_info", fun () ->
    let config = Binance.Cfg.of_string "production" in
    Binance.V3.Exchange_info.request config () >>| function
    | `Ok response ->
      (* Verify we got some symbols *)
      if List.length response.Binance.V3.Exchange_info.T.symbols > 0 then
        Ok ()
      else
        Error "No symbols in exchange info"
    | #Binance.Rest.Error.t as err ->
      Error (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)));

  ("depth", fun () ->
    let config = Binance.Cfg.of_string "production" in
    Binance.V3.Depth.request config { symbol = test_symbol; limit = Some 10 } >>| function
    | `Ok response ->
      if List.length response.bids > 0 && List.length response.asks > 0 then
        Ok ()
      else
        Error "Empty order book"
    | #Binance.Rest.Error.t as err ->
      Error (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)));

  ("ticker_24hr", fun () ->
    let config = Binance.Cfg.of_string "production" in
    Binance.V3.Ticker_24hr.request config { symbol = Some test_symbol } >>| function
    | `Ok _response -> Ok ()
    | #Binance.Rest.Error.t as err ->
      Error (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)));
]

(** Account endpoint tests (requires auth) *)
let account_tests = [
  ("account_info", fun () ->
    match get_config () with
    | Error e -> return (Error e)
    | Ok config ->
      Binance.V3.Account.request config () >>| function
      | `Ok response ->
        (* Verify we got balances *)
        if List.length response.balances > 0 then
          Ok ()
        else
          Error "No balances in account"
      | #Binance.Rest.Error.t as err ->
        Error (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)));
]

(** Trading endpoint tests (requires auth, uses testnet) *)
let trading_tests = [
  ("place_and_cancel_market_order", fun () ->
    match get_config () with
    | Error e -> return (Error e)
    | Ok config ->
      (* Place a small market buy order *)
      let order_request = Binance.V3.New_order.T.{
        symbol = test_symbol;
        side = Binance.Common.Side.Buy;
        type_ = Binance.Common.Order_type.Market;
        timeInForce = None;
        quantity = None;
        quoteOrderQty = Some "10";  (* $10 worth *)
        price = None;
        newClientOrderId = None;
        recvWindow = None;
      } in

      Binance.V3.New_order.request config order_request >>= function
      | `Ok order_response ->
        (* Try to query the order *)
        let query_request = Binance.V3.Query_order.T.{
          symbol = test_symbol;
          orderId = Some order_response.orderId;
          origClientOrderId = None;
          recvWindow = None;
        } in

        Binance.V3.Query_order.request config query_request >>| (function
          | `Ok _query_response -> Ok ()
          | #Binance.Rest.Error.t as err ->
            Error (sprintf "Query order failed: %s" (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err))))
      | #Binance.Rest.Error.t as err ->
        Error (sprintf "Place order failed: %s" (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err))));

  ("query_open_orders", fun () ->
    match get_config () with
    | Error e -> return (Error e)
    | Ok config ->
      Binance.V3.Open_orders.request config { symbol = Some test_symbol; recvWindow = None } >>| function
      | `Ok _orders -> Ok ()
      | #Binance.Rest.Error.t as err ->
        Error (Sexp.to_string_hum (Binance.Rest.Error.sexp_of_t err)));
]

(** Main test runner *)
let run_all_tests () =
  let suite = Suite.create ~name:"Binance Integration Tests" () in
  let rate_limiter = Rate_limiter.create ~requests_per_second:5 in

  (* Check if testnet credentials are available *)
  match Config.from_env ~exchange:"binance" ~testnet:true with
  | None ->
    printf "\n⚠ Skipping Binance integration tests - testnet credentials not available\n";
    printf "Set BINANCE_TESTNET_API_KEY and BINANCE_TESTNET_API_SECRET to run these tests\n\n";
    return false
  | Some _config ->
    (* Run public tests *)
    Runner.run_tests ~suite ~exchange:"Binance" ~tests:public_tests ~rate_limiter >>= fun () ->

    (* Run account tests *)
    Runner.run_tests ~suite ~exchange:"Binance (Account)" ~tests:account_tests ~rate_limiter >>= fun () ->

    (* Run trading tests *)
    Runner.run_tests ~suite ~exchange:"Binance (Trading)" ~tests:trading_tests ~rate_limiter >>= fun () ->

    (* Print summary *)
    let all_passed = Suite.print_summary suite in
    return all_passed

let () =
  Command.async
    ~summary:"Run Binance integration tests against testnet"
    Command.Param.(return (fun () -> run_all_tests () >>| fun success -> if not success then exit 1))
  |> Command_unix.run
