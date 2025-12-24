(** Example usage of Kraken market data WebSocket *)

open Core

(** Simulate receiving market data messages *)
let example_xrp_ticker_message =
  {|[123,{"a":["2.45678","1000.50"],"b":["2.45677","2500.25"],"c":["2.45678","500.00"],"v":["5234125.60","6234567.89"],"p":["2.45600","2.45620"],"t":[234,345],"l":["2.45400","2.45450"],"h":["2.46200","2.46300"],"o":["2.45500","2.45450"]},"XRPUSD","ticker"]|}

let example_xrp_trade_messages = [
  {|[124,[["2.45678","1000.50",1608424089.123,"b","m","","234"],["2.45679","500.25",1608424089.456,"s","m","",""]],"XRPUSD","trade"]|};
  {|[124,[["2.45677","250.00",1608424090.789,"b","l","","456"]],"XRPUSD","trade"]|};
  {|[124,[["2.45680","2000.00",1608424091.234,"s","m","",""]],"XRPUSD","trade"]|};
]

let () =
  printf "=== Kraken Market Data WebSocket Example ===\n\n";
  
  printf "Example: Real-time ticker data for XRPUSD\n";
  printf "-------------------------------------------\n";
  printf "Message format: [channelID, tickerData, pair, channel_name]\n\n";
  
  let json = Yojson.Safe.from_string example_xrp_ticker_message in
  printf "%s\n\n" (Yojson.Safe.pretty_to_string json);
  
  printf "Parsed fields:\n";
  printf "  Ask: 2.45678 USD (1000.50 XRP available)\n";
  printf "  Bid: 2.45677 USD (2500.25 XRP available)\n";
  printf "  Last: 2.45678 USD\n";
  printf "  Volume (24h): 5.2M XRP\n";
  printf "  Low (24h): 2.45400 USD\n";
  printf "  High (24h): 2.46200 USD\n";
  printf "  VWAP: 2.45600 USD\n\n";
  
  printf "Example: Real-time trade data for XRPUSD\n";
  printf "-------------------------------------------\n";
  printf "Trade format: [price, volume, timestamp, side, order_type, misc, tradeID]\n\n";
  
  List.iteri example_xrp_trade_messages ~f:(fun i msg ->
    printf "Trade %d:\n" (i + 1);
    let json = Yojson.Safe.from_string msg in
    printf "%s\n" (Yojson.Safe.pretty_to_string json);
    printf "\n"
  );
  
  printf "CLI Examples:\n";
  printf "=============\n\n";
  printf "# Stream XRP ticker (5 messages)\n";
  printf "  fluxum kraken ws stream --pairs XRPUSD --channel ticker --limit 5\n\n";
  printf "# Stream XRP trades (20 messages)\n";
  printf "  fluxum kraken ws stream --pairs XRPUSD --channel trade --limit 20\n\n";
  printf "# Stream multiple pairs\n";
  printf "  fluxum kraken ws stream --pairs XRPUSD,ETHUSD,BTCUSD --channel ticker\n\n";
  printf "# Stream order book updates (depth 25)\n";
  printf "  fluxum kraken ws stream --pairs XRPUSD --channel book\n\n";
  printf "# Stream 5-minute candles\n";
  printf "  fluxum kraken ws stream --pairs XRPUSD --channel ohlc\n"
