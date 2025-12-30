(** Gemini Integration Tests - Sandbox

    Tests against Gemini sandbox (api.sandbox.gemini.com).
    Requires GEMINI_SANDBOX_API_KEY and GEMINI_SANDBOX_API_SECRET env vars.

    Run with:
      GEMINI_SANDBOX_API_KEY=xxx GEMINI_SANDBOX_API_SECRET=yyy dune exec lib/exchange/gemini/test/integration_tests.exe
*)

open Core
open Async
open Integration_test_framework

let test_symbol = "btcusd"

(** Create sandbox config *)
let get_config () =
  match Sys.getenv "GEMINI_SANDBOX_API_KEY", Sys.getenv "GEMINI_SANDBOX_API_SECRET" with
  | Some api_key, Some api_secret ->
    Ok (module struct
      let api_key = api_key
      let api_secret = api_secret
      let base_url = "api.sandbox.gemini.com"
    end : Gemini.Cfg.S)
  | _ ->
    Error "GEMINI_SANDBOX_API_KEY and GEMINI_SANDBOX_API_SECRET must be set"

(** Public endpoint tests (no auth required) *)
let public_tests = [
  ("symbols", fun () ->
    let config = Gemini.Cfg.of_string "sandbox" in
    Gemini.V1.Symbols.request config () >>| function
    | `Ok symbols ->
      if List.exists symbols ~f:(String.equal test_symbol) then
        Ok ()
      else
        Error (sprintf "Test symbol %s not found in symbols list" test_symbol)
    | #Gemini.Rest.Error.t as err ->
      Error (Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_t err)));

  ("ticker", fun () ->
    let config = Gemini.Cfg.of_string "sandbox" in
    Gemini.V1.Ticker.request config { symbol = test_symbol } >>| function
    | `Ok ticker ->
      (* Verify ticker has non-zero bid/ask *)
      (match Float.of_string_opt ticker.bid, Float.of_string_opt ticker.ask with
       | Some bid, Some ask when Float.(bid > 0. && ask > 0. && ask >= bid) ->
         Ok ()
       | _ ->
         Error (sprintf "Invalid ticker: bid=%s ask=%s" ticker.bid ticker.ask))
    | #Gemini.Rest.Error.t as err ->
      Error (Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_t err)));
]

(** Account endpoint tests (requires auth) *)
let account_tests = [
  ("balances", fun () ->
    match get_config () with
    | Error e -> return (Error e)
    | Ok config ->
      Gemini.V1.Balances.request config () >>| function
      | `Ok balances ->
        (* Just verify we get a list of balances *)
        if List.length balances >= 0 then  (* Can be empty on new account *)
          Ok ()
        else
          Error "Unexpected balances response"
      | #Gemini.Rest.Error.t as err ->
        Error (Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_t err)));

  ("past_trades", fun () ->
    match get_config () with
    | Error e -> return (Error e)
    | Ok config ->
      Gemini.V1.Past_trades.request config {
        symbol = test_symbol;
        limit_trades = Some 10;
        timestamp = None;
      } >>| function
      | `Ok _trades -> Ok ()  (* Can be empty if no trades *)
      | #Gemini.Rest.Error.t as err ->
        Error (Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_t err)));
]

(** Trading endpoint tests (requires auth, uses sandbox) *)
let trading_tests = [
  ("place_and_cancel_limit_order", fun () ->
    match get_config () with
    | Error e -> return (Error e)
    | Ok config ->
      (* Get current ticker to place order far from market *)
      Gemini.V1.Ticker.request config { symbol = test_symbol } >>= function
      | `Ok ticker ->
        (match Float.of_string_opt ticker.bid with
         | None -> return (Error "Could not parse bid price")
         | Some bid_price ->
           (* Place limit buy order 20% below market *)
           let order_price = bid_price *. 0.8 in
           let order_request = Gemini.V1.New_order.T.{
             symbol = test_symbol;
             amount = "0.001";  (* Small amount *)
             price = Float.to_string order_price;
             side = "buy";
             type_ = "exchange limit";
             options = [];
           } in

           Gemini.V1.New_order.request config order_request >>= function
           | `Ok order_response ->
             (* Cancel the order *)
             let cancel_request = Gemini.V1.Cancel_order.T.{
               order_id = order_response.order_id;
             } in

             Gemini.V1.Cancel_order.request config cancel_request >>| (function
               | `Ok cancel_response ->
                 if cancel_response.is_cancelled then
                   Ok ()
                 else
                   Error "Order not cancelled"
               | #Gemini.Rest.Error.t as err ->
                 Error (sprintf "Cancel failed: %s" (Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_t err))))
           | #Gemini.Rest.Error.t as err ->
             return (Error (sprintf "Place order failed: %s" (Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_t err)))))
      | #Gemini.Rest.Error.t as err ->
        return (Error (sprintf "Ticker failed: %s" (Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_t err)))));

  ("active_orders", fun () ->
    match get_config () with
    | Error e -> return (Error e)
    | Ok config ->
      Gemini.V1.Active_orders.request config () >>| function
      | `Ok _orders -> Ok ()  (* Can be empty *)
      | #Gemini.Rest.Error.t as err ->
        Error (Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_t err)));

  ("cancel_all_orders", fun () ->
    match get_config () with
    | Error e -> return (Error e)
    | Ok config ->
      (* This is safe to call even if no orders *)
      Gemini.V1.Cancel_all_orders.request config () >>| function
      | `Ok _result -> Ok ()
      | #Gemini.Rest.Error.t as err ->
        Error (Sexp.to_string_hum (Gemini.Rest.Error.sexp_of_t err)));
]

(** Main test runner *)
let run_all_tests () =
  let suite = Suite.create ~name:"Gemini Integration Tests" () in
  let rate_limiter = Rate_limiter.create ~requests_per_second:5 in

  (* Check if sandbox credentials are available *)
  match Config.from_env ~exchange:"gemini" ~testnet:true with
  | None ->
    printf "\n⚠ Skipping Gemini integration tests - sandbox credentials not available\n";
    printf "Set GEMINI_SANDBOX_API_KEY and GEMINI_SANDBOX_API_SECRET to run these tests\n\n";
    return false
  | Some _config ->
    (* Run public tests *)
    Runner.run_tests ~suite ~exchange:"Gemini" ~tests:public_tests ~rate_limiter >>= fun () ->

    (* Run account tests *)
    Runner.run_tests ~suite ~exchange:"Gemini (Account)" ~tests:account_tests ~rate_limiter >>= fun () ->

    (* Run trading tests *)
    Runner.run_tests ~suite ~exchange:"Gemini (Trading)" ~tests:trading_tests ~rate_limiter >>= fun () ->

    (* Print summary *)
    let all_passed = Suite.print_summary suite in
    return all_passed

let () =
  Command.async
    ~summary:"Run Gemini integration tests against sandbox"
    Command.Param.(return (fun () -> run_all_tests () >>| fun success -> if not success then exit 1))
  |> Command_unix.run
