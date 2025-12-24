(** Unit tests for Kraken Session module *)

open Core
open Async
open Kraken

(** Test 1: State module exists and has expected values *)
let%test_unit "state_module_values" =
  let open Fluxum.Session_intf.State in
  (* Just verify the state values exist *)
  let _disconnected = Disconnected in
  let _connecting = Connecting in
  let _connected = Connected in
  let _ready = Ready in
  let _reconnecting = Reconnecting in
  let _failed = Failed in
  ()

(** Test 2: Create session with empty symbols *)
let%test "create_session_empty_symbols" =
  Thread_safe.block_on_async_exn (fun () ->
    let%bind session = Session.create ~symbols:[] () in
    let state = Session.state session in
    let events = Session.events session in
    let symbols = Session.Events.symbols events in

    (* Verify state is Ready *)
    [%test_result: Fluxum.Session_intf.State.t] ~expect:Ready state;
    [%test_result: int] ~expect:0 (List.length symbols);

    Session.close session
    >>| fun () -> true
  )

(** Test 3: Create session with symbols *)
let%test "create_session_with_symbols" =
  Thread_safe.block_on_async_exn (fun () ->
    let%bind session = Session.create ~symbols:["BTC/USD"; "ETH/USD"] () in
    let events = Session.events session in
    let symbols = Session.Events.symbols events in

    [%test_result: int] ~expect:2 (List.length symbols);
    [%test_result: bool] ~expect:true (List.mem symbols "BTC/USD" ~equal:String.equal);
    [%test_result: bool] ~expect:true (List.mem symbols "ETH/USD" ~equal:String.equal);

    (* Verify event streams exist *)
    let _balance_pipe = Session.Events.balance events in
    let trades_map = Session.Events.trades events in
    let market_data_map = Session.Events.market_data events in
    let order_books_map = Session.Events.order_books events in
    let ledger_map = Session.Events.ledger events in
    let _order_events = Session.Events.order_events events in

    (* Verify maps have entries for symbols *)
    [%test_result: bool] ~expect:true (Map.mem trades_map "BTC/USD");
    [%test_result: bool] ~expect:true (Map.mem trades_map "ETH/USD");
    [%test_result: bool] ~expect:true (Map.mem market_data_map "BTC/USD");
    [%test_result: bool] ~expect:true (Map.mem order_books_map "BTC/USD");
    [%test_result: bool] ~expect:true (Map.mem ledger_map "BTC/USD");

    Session.close session
    >>| fun () -> true
  )

(** Test 4: Session state changes on close *)
let%test "session_state_changes_on_close" =
  Thread_safe.block_on_async_exn (fun () ->
    let%bind session = Session.create ~symbols:[] () in

    (* Initial state should be Ready *)
    [%test_result: Fluxum.Session_intf.State.t] ~expect:Ready (Session.state session);

    (* Close session *)
    let%bind () = Session.close session in

    (* State should be Disconnected after close *)
    [%test_result: Fluxum.Session_intf.State.t] ~expect:Disconnected (Session.state session);

    return true
  )

(** Test 5: State changes pipe emits events *)
let%test "state_changes_pipe_emits_events" =
  Thread_safe.block_on_async_exn (fun () ->
    let%bind session = Session.create ~symbols:[] () in
    let state_changes = Session.state_changes session in

    (* Collect state changes *)
    let states = ref [] in
    let collector = Pipe.iter state_changes ~f:(fun state ->
      states := state :: !states;
      Deferred.unit
    ) in

    (* Give it a moment to collect initial states *)
    let%bind () = after (Time_float_unix.Span.of_sec 0.1) in

    (* Close the session to emit Disconnected state *)
    let%bind () = Session.close session in
    let%bind () = after (Time_float_unix.Span.of_sec 0.1) in

    (* Stop collecting *)
    Pipe.close_read state_changes;
    let%bind () = collector in

    (* Should have seen at least Connecting, Ready, and Disconnected *)
    let states_list = List.rev !states in
    [%test_result: bool] ~expect:true (List.length states_list >= 2);

    (* Check that we saw some expected states *)
    let has_connecting = List.mem states_list Connecting ~equal:Fluxum.Session_intf.State.equal in
    let has_ready = List.mem states_list Ready ~equal:Fluxum.Session_intf.State.equal in
    let has_disconnected = List.mem states_list Disconnected ~equal:Fluxum.Session_intf.State.equal in

    [%test_result: bool] ~expect:true (has_connecting || has_ready);
    [%test_result: bool] ~expect:true has_disconnected;

    return true
  )

(** Test 6: Events accessors work *)
let%test "events_accessors" =
  Thread_safe.block_on_async_exn (fun () ->
    let%bind session = Session.create ~symbols:["BTC/USD"] () in
    let events = Session.events session in

    (* Call all accessor functions *)
    let _symbols = Session.Events.symbols events in
    let _balance = Session.Events.balance events in
    let _trades = Session.Events.trades events in
    let _market_data = Session.Events.market_data events in
    let _order_books = Session.Events.order_books events in
    let _ledger = Session.Events.ledger events in
    let _order_events = Session.Events.order_events events in

    Session.close session
    >>| fun () -> true
  )

(** Test 7: Multiple symbols in session *)
let%test "multiple_symbols" =
  Thread_safe.block_on_async_exn (fun () ->
    let symbols = ["BTC/USD"; "ETH/USD"; "XRP/USD"; "SOL/USD"] in
    let%bind session = Session.create ~symbols () in
    let events = Session.events session in
    let session_symbols = Session.Events.symbols events in

    [%test_result: int] ~expect:4 (List.length session_symbols);

    (* Verify all symbols are present *)
    List.iter symbols ~f:(fun sym ->
      [%test_result: bool] ~expect:true
        (List.mem session_symbols sym ~equal:String.equal)
    );

    Session.close session
    >>| fun () -> true
  )

let () = print_endline "All Session tests defined"
