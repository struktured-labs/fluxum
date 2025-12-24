(** Kraken websocket commands *)

let name = "ws"

module Subscribe = struct
  let name = "subscribe"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Subscribe to Kraken WebSocket feed"
      (Command.Param.(
        let channel = flag "--channel" 
          (required (Arg_type.create (fun s -> s)))
          ~doc:"STRING channel name (ticker, spread, trade, ohlc, book, own-orders)"
        and pairs = flag "--pairs" 
          (optional (Arg_type.comma_separated string))
          ~doc:"PAIRS comma-separated trading pairs (e.g., XETHZUSD,XDGUSD)"
        and interval = flag "--interval" 
          (optional int)
          ~doc:"INT OHLC interval in seconds (default 60)"
        and depth = flag "--depth" 
          (optional int)
          ~doc:"INT order book depth (default 10)"
        and is_private = flag "--private" no_arg
          ~doc:"Use private channel (for own-orders)"
        in
        return (fun channel pairs interval depth is_private () ->
          if is_private then
            printf "Private channel subscription would connect to: %s\n" Ws.Endpoint.private_url
          else
            printf "Public channel subscription would connect to: %s\n" Ws.Endpoint.public_url;
          
          printf "Channel: %s\n" channel;
          (match pairs with
          | Some p -> printf "Pairs: %s\n" (String.concat ~sep:", " p)
          | None -> printf "Pairs: (none - private channel)\n");
          
          let subscription_msg = match channel with
            | "ticker" -> 
              (match pairs with
              | Some p -> Ws.Public.subscribe_ticker p
              | None -> "error: pairs required for ticker channel")
            | "spread" -> 
              (match pairs with
              | Some p -> Ws.Public.subscribe_spread p
              | None -> "error: pairs required for spread channel")
            | "trade" -> 
              (match pairs with
              | Some p -> Ws.Public.subscribe_trade p
              | None -> "error: pairs required for trade channel")
            | "ohlc" -> 
              (match pairs with
              | Some p -> 
                let int_val = Option.value interval ~default:60 in
                Ws.Public.subscribe_ohlc ~interval:int_val p
              | None -> "error: pairs required for ohlc channel")
            | "book" ->
              (match pairs with
              | Some p ->
                let depth_val = Option.value depth ~default:10 in
                Ws.Public.subscribe_book ~depth:depth_val p
              | None -> "error: pairs required for book channel")
            | "own-orders" -> 
              Ws.Private.auth_message ~signature:"<your-signature>"
            | _ -> "unknown channel"
          in
          
          printf "\nSubscription message:\n%s\n" subscription_msg;
          printf "\n(WebSocket connection would be established with async_websocket library)\n";
          Deferred.unit)
        <*> channel
        <*> pairs
        <*> interval
        <*> depth
        <*> is_private
      )))
end

module Stream = struct
  let name = "stream"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"Stream live market data from Kraken"
      (Command.Param.(
        let channel = flag "--channel" 
          (optional_with_default "ticker" string)
          ~doc:"STRING channel (ticker, spread, trade, ohlc, book)"
        and pairs = flag "--pairs" 
          (optional_with_default "XRPUSD" string)
          ~doc:"STRING trading pair (default XRPUSD)"
        and limit = flag "--limit" 
          (optional_with_default 10 int)
          ~doc:"INT number of messages to stream (default 10)"
        and url = flag "--url"
          (optional_with_default Ws.Endpoint.public_url string)
          ~doc:(Printf.sprintf "STRING WebSocket URL (default %s)" Ws.Endpoint.public_url)
        in
        return (fun channel pairs limit url () ->
          let pair_list = String.split pairs ~on:',' |> List.map ~f:String.strip in
          let subscription = {
            Market_data.channel;
            pairs = pair_list;
            interval = None;
            depth = None;
          } in
          
          printf "Connecting to Kraken public WebSocket...\n";
          printf "WebSocket URL: %s\n" url;
          printf "Channel: %s\n" channel;
          printf "Pairs: %s\n" (String.concat ~sep:", " pair_list);
          printf "Streaming up to %d messages...\n\n" limit;

          (* Print the subscription payload that would be sent *)
          let is_v2 = String.is_substring url ~substring:"/v2" in
          let normalize_symbol s =
            if String.mem s '/' then s
            else
              let len = String.length s in
              if len > 3 then
                let base = String.sub s ~pos:0 ~len:(len - 3) in
                let quote = String.sub s ~pos:(len - 3) ~len:3 in
                base ^ "/" ^ quote
              else s
          in
          let subscription_payload =
            if is_v2 then
              let symbols = List.map pair_list ~f:normalize_symbol in
              let params =
                `Assoc [
                  ("channel", `String channel);
                  ("symbol", `List (List.map symbols ~f:(fun p -> `String p)));
                  ("event_trigger", `String "trades");
                  ("snapshot", `Bool true)
                ]
              in
              Yojson.Safe.to_string (`Assoc [
                ("method", `String "subscribe");
                ("params", params)
              ])
            else
              match channel with
              | "ticker" -> Ws.Public.subscribe_ticker pair_list
              | "spread" -> Ws.Public.subscribe_spread pair_list
              | "trade" -> Ws.Public.subscribe_trade pair_list
              | "ohlc" -> Ws.Public.subscribe_ohlc ~interval:60 pair_list
              | "book" -> Ws.Public.subscribe_book ~depth:10 pair_list
              | _ -> "unknown channel"
          in
          printf "Subscription payload (JSON):\n%s\n\n" subscription_payload;
          
          Market_data.connect ~url ~subscriptions:[subscription] ()
          >>= function
          | Error err ->
            printf "Connection failed: %s\n" 
              (match err with
              | `Connection_error msg -> msg
              | `Json_parse_error msg -> "JSON error: " ^ msg
              | `Authentication_error msg -> "Auth error: " ^ msg
              | `Channel_parse_error msg -> "Channel error: " ^ msg);
            Deferred.unit
          | Ok client ->
            let message_stream = Market_data.messages client in
            let message_count = ref 0 in
            
            Pipe.iter message_stream ~f:(fun msg ->
              incr message_count;
              printf "[%d] %s\n" !message_count msg;
              if !message_count >= limit then
                Pipe.close_read message_stream;
              Deferred.unit)
            >>= fun () ->
            Market_data.close client
            >>= fun () ->
            printf "\nStreaming complete.\n";
            Deferred.unit)
        <*> channel
        <*> pairs
        <*> limit
        <*> url
      )))
end

module Channels = struct
  let name = "channels"

  let command : string * Command.t =
    (name, Command.async
      ~summary:"List available WebSocket channels"
      (Command.Param.return (fun () ->
        printf "=== Public Channels (Market Data) ===\n";
        printf "  ticker   - Best ask/bid prices and recent trade\n";
        printf "  spread   - Ask/bid spread and volume\n";
        printf "  trade    - Individual trades in real-time\n";
        printf "  ohlc     - Candlestick OHLC data (configurable interval, default 60s)\n";
        printf "  book     - Order book updates (configurable depth, default 10)\n";
        printf "\n=== Private Channels (requires authentication) ===\n";
        printf "  own-orders   - User's open/closed orders in real-time\n";
        printf "  own-trades   - User's executed trades\n";
        printf "\n=== Examples ===\n";
        printf "  # Ticker for ETH and Doge\n";
        printf "  fluxum kraken ws subscribe --channel ticker --pairs XETHZUSD,XDGUSD\n";
        printf "  # Watch your orders in real-time\n";
        printf "  fluxum kraken ws subscribe --channel own-orders --private\n";
        printf "  # 5-minute candles\n";
        printf "  fluxum kraken ws subscribe --channel ohlc --pairs XETHZUSD --interval 300\n";
        printf "  # Order book with 25 levels\n";
        printf "  fluxum kraken ws subscribe --channel book --pairs XETHZUSD --depth 25\n";
        Deferred.unit)
      ))
end

let command : string * Command.t =
  (name, Command.group
    ~summary:"Kraken WebSocket commands"
    [ Subscribe.command; Channels.command; Stream.command ])
