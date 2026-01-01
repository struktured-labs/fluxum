open Core
open Async

let test () =
  let open Deferred.Let_syntax in

  printf "Starting MEXC order book for BTC/USDT...\n";
  printf "Connecting to MEXC WebSocket (Protobuf)...\n\n%!";

  (* Use LimitDepth for full snapshots, or AggreDepth for incremental updates *)
  let streams = [
    Mexc.Ws.Stream.LimitDepths { symbol = "BTCUSDT"; levels = 20 };
  ] in

  (* Try secure endpoint first, fall back to insecure if needed *)
  let%bind ws_result = Mexc.Ws.connect ~streams () in
  let%bind ws_result = match ws_result with
    | Ok _ -> return ws_result
    | Error _ ->
      printf "(TLS issue, trying insecure endpoint...)\n%!";
      Mexc.Ws.connect ~url:Mexc.Ws.Endpoint.public_url_insecure ~streams ()
  in

  match ws_result with
  | Error err ->
    eprintf "Connection error: %s\n%!" (Error.to_string_hum err);
    return ()
  | Ok ws ->
    printf "✓ Connected to MEXC WebSocket\n";
    printf "Waiting for order book data...\n\n%!";

    (* Create order book *)
    let book = ref (Mexc.Order_book.Book.create ~symbol:"BTCUSDT") in
    let update_count = ref 0 in

    (* Process messages *)
    let%bind () =
      Pipe.iter (Mexc.Ws.messages ws) ~f:(fun msg ->
        let parsed = Mexc.Ws.parse_message msg in

        match parsed with
        | Mexc.Ws.Message.SubscriptionAck { msg; _ } ->
          printf "[INFO] Subscription ack: %s\n%!" msg;
          return ()

        | Mexc.Ws.Message.Data wrapper ->
          (match wrapper.body with
           | Mexc.Ws.Message.LimitDepth depth ->
             (* Full snapshot from LimitDepth stream *)
             update_count := !update_count + 1;

             (* Clear and rebuild order book *)
             book := Mexc.Order_book.Book.create ~symbol:"BTCUSDT";

             (* Add bids *)
             List.iter depth.bids ~f:(fun item ->
               let price = Float.of_string item.price in
               let size = Float.of_string item.quantity in
               book := Mexc.Order_book.Book.set !book ~side:`Bid ~price ~size
             );

             (* Add asks *)
             List.iter depth.asks ~f:(fun item ->
               let price = Float.of_string item.price in
               let size = Float.of_string item.quantity in
               book := Mexc.Order_book.Book.set !book ~side:`Ask ~price ~size
             );

             (* Display every 5 updates *)
             if !update_count mod 5 = 0 then begin
               let mid = Mexc.Order_book.Book.mid_price !book in
               let spread = Mexc.Order_book.Book.spread !book in
               printf "Update #%d: Mid=%.2f Spread=%.2f\n%!" !update_count mid spread
             end;

             return ()

           | Mexc.Ws.Message.AggreDepth depth ->
             (* Incremental updates from AggreDepth stream *)
             update_count := !update_count + 1;

             (* Apply incremental updates *)
             List.iter depth.bids ~f:(fun item ->
               let price = Float.of_string item.price in
               let size = Float.of_string item.quantity in
               book := Mexc.Order_book.Book.set !book ~side:`Bid ~price ~size
             );

             List.iter depth.asks ~f:(fun item ->
               let price = Float.of_string item.price in
               let size = Float.of_string item.quantity in
               book := Mexc.Order_book.Book.set !book ~side:`Ask ~price ~size
             );

             if !update_count mod 10 = 0 then begin
               let mid = Mexc.Order_book.Book.mid_price !book in
               let spread = Mexc.Order_book.Book.spread !book in
               printf "Update #%d: Mid=%.2f Spread=%.2f\n%!" !update_count mid spread
             end;

             return ()

           | Mexc.Ws.Message.AggreDeals deals ->
             let deal_count = List.length deals.deals in
             printf "Received %d trades\n%!" deal_count;
             return ()

           | Mexc.Ws.Message.BookTicker ticker ->
             printf "Book ticker: bid=%s ask=%s\n%!"
               ticker.bid_price ticker.ask_price;
             return ()

           | Mexc.Ws.Message.Unknown _ ->
             printf "Unknown message type\n%!";
             return ())

        | Mexc.Ws.Message.Pong ->
          printf "[PONG] Keepalive response\n%!";
          return ()

        | Mexc.Ws.Message.Error err ->
          eprintf "Parse error: %s\n%!" err;
          return ()

        | Mexc.Ws.Message.Raw data ->
          printf "Raw message: %d bytes\n%!" (String.length data);
          return ()
      )
    in

    printf "Order book stream ended\n%!";
    return ()

let () =
  Command.async
    ~summary:"MEXC order book (Protobuf WebSocket)"
    ~readme:(fun () ->
      "Demonstrates MEXC order book streaming via Protobuf WebSocket.\n\
       Uses LimitDepths stream for full snapshots.\n\
       Note: MEXC may require VPN in some regions.")
    (Command.Param.return test)
  |> Command_unix.run
