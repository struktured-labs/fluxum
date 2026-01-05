(** Parquet Export - bin_prot to columnar data converter

    Provides export capabilities for Python interoperability:
    - CSV export (universal compatibility)
    - Arrow IPC format (for direct pyarrow loading)
    - Streaming and batch modes
*)

open Core
open Async

module Config = struct
  type format =
    | Csv
    | Arrow_ipc
    | Json_lines
  [@@deriving sexp]

  type t =
    { output_path : string
    ; format : format
    ; batch_size : int
    ; include_header : bool
    ; compress : bool
    }
  [@@deriving sexp]

  let default =
    { output_path = "./export"
    ; format = Csv
    ; batch_size = 10000
    ; include_header = true
    ; compress = false
    }
end

(** Event to row conversion *)
module Row = struct
  type t =
    { event_id : string
    ; timestamp : string
    ; sequence : int64
    ; category : string
    ; event_type : string
    ; symbol : string option
    ; venue : string option
    ; side : string option
    ; price : float option
    ; qty : float option
    ; fee : float option
    ; order_id : string option
    ; message : string option
    }
  [@@deriving sexp]

  let csv_header =
    "event_id,timestamp,sequence,category,event_type,symbol,venue,side,price,qty,fee,order_id,message"

  let to_csv t =
    let opt_str = Option.value ~default:"" in
    let opt_float f = Option.value_map f ~default:"" ~f:(sprintf "%.8f") in
    sprintf "%s,%s,%Ld,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s"
      t.event_id
      t.timestamp
      t.sequence
      t.category
      t.event_type
      (opt_str t.symbol)
      (opt_str t.venue)
      (opt_str t.side)
      (opt_float t.price)
      (opt_float t.qty)
      (opt_float t.fee)
      (opt_str t.order_id)
      (opt_str t.message)

  let to_json t =
    let opt_field name value =
      Option.value_map value ~default:"" ~f:(fun v ->
        sprintf ", \"%s\": \"%s\"" name v)
    in
    let opt_float_field name value =
      Option.value_map value ~default:"" ~f:(fun v ->
        sprintf ", \"%s\": %.8f" name v)
    in
    sprintf "{\"event_id\": \"%s\", \"timestamp\": \"%s\", \"sequence\": %Ld, \"category\": \"%s\", \"event_type\": \"%s\"%s%s%s%s%s%s%s%s}"
      t.event_id
      t.timestamp
      t.sequence
      t.category
      t.event_type
      (opt_field "symbol" t.symbol)
      (opt_field "venue" t.venue)
      (opt_field "side" t.side)
      (opt_float_field "price" t.price)
      (opt_float_field "qty" t.qty)
      (opt_float_field "fee" t.fee)
      (opt_field "order_id" t.order_id)
      (opt_field "message" t.message)

  let of_envelope (env : Event.envelope) =
    let base =
      { event_id = env.id
      ; timestamp = Event.Time.to_string env.timestamp
      ; sequence = env.sequence
      ; category = Event.category env.event
      ; event_type = ""
      ; symbol = None
      ; venue = None
      ; side = None
      ; price = None
      ; qty = None
      ; fee = None
      ; order_id = None
      ; message = None
      }
    in
    match env.event with
    | Event.Market (Book_update { symbol; venue; is_snapshot; _ }) ->
      { base with
        event_type = (match is_snapshot with true -> "book_snapshot" | false -> "book_update")
      ; symbol = Some symbol
      ; venue = Some (Event.Venue.to_string venue)
      }
    | Event.Market (Trade { symbol; venue; price; qty; side; _ }) ->
      { base with
        event_type = "trade"
      ; symbol = Some symbol
      ; venue = Some (Event.Venue.to_string venue)
      ; price = Some price
      ; qty = Some qty
      ; side = Option.map side ~f:Event.Side.to_string
      }
    | Event.Market (Ticker { symbol; venue; last; _ }) ->
      { base with
        event_type = "ticker"
      ; symbol = Some symbol
      ; venue = Some (Event.Venue.to_string venue)
      ; price = Some last
      }
    | Event.Order (Order_submitted { order_id; symbol; venue; side; qty; price; _ }) ->
      { base with
        event_type = "order_submitted"
      ; order_id = Some order_id
      ; symbol = Some symbol
      ; venue = Some (Event.Venue.to_string venue)
      ; side = Some (Event.Side.to_string side)
      ; qty = Some qty
      ; price
      }
    | Event.Order (Order_accepted { order_id; exchange_id; venue }) ->
      { base with
        event_type = "order_accepted"
      ; order_id = Some order_id
      ; venue = Some (Event.Venue.to_string venue)
      ; message = Some exchange_id
      }
    | Event.Order (Order_filled { order_id; venue; fill_qty; fill_price; fee; _ }) ->
      { base with
        event_type = "order_filled"
      ; order_id = Some order_id
      ; venue = Some (Event.Venue.to_string venue)
      ; qty = Some fill_qty
      ; price = Some fill_price
      ; fee = Some fee
      }
    | Event.Order (Order_partially_filled { order_id; venue; fill_qty; fill_price; fee; _ }) ->
      { base with
        event_type = "order_partial_fill"
      ; order_id = Some order_id
      ; venue = Some (Event.Venue.to_string venue)
      ; qty = Some fill_qty
      ; price = Some fill_price
      ; fee = Some fee
      }
    | Event.Order (Order_cancelled { order_id; venue; reason }) ->
      { base with
        event_type = "order_cancelled"
      ; order_id = Some order_id
      ; venue = Some (Event.Venue.to_string venue)
      ; message = Some reason
      }
    | Event.Order (Order_rejected { order_id; venue; reason }) ->
      { base with
        event_type = "order_rejected"
      ; order_id = Some order_id
      ; venue = Some (Event.Venue.to_string venue)
      ; message = Some reason
      }
    | Event.Balance (Balance_update { venue; currency; total; _ }) ->
      { base with
        event_type = "balance_update"
      ; venue = Some (Event.Venue.to_string venue)
      ; symbol = Some currency
      ; qty = Some total
      }
    | Event.Balance (Balance_snapshot { venue; _ }) ->
      { base with
        event_type = "balance_snapshot"
      ; venue = Some (Event.Venue.to_string venue)
      }
    | Event.System (Bot_started { bot_id; _ }) ->
      { base with
        event_type = "bot_started"
      ; message = Some bot_id
      }
    | Event.System (Bot_stopped { reason; _ }) ->
      { base with
        event_type = "bot_stopped"
      ; message = Some reason
      }
    | Event.System (Connection_state_changed { venue; new_state; _ }) ->
      { base with
        event_type = "connection_changed"
      ; venue = Some (Event.Venue.to_string venue)
      ; message = Some (Sexp.to_string (Event.Connection_state.sexp_of_t new_state))
      }
    | Event.System (Error { venue; message; is_fatal }) ->
      { base with
        event_type = (match is_fatal with true -> "fatal_error" | false -> "error")
      ; venue = Option.map venue ~f:Event.Venue.to_string
      ; message = Some message
      }
    | Event.System (Heartbeat { sequence }) ->
      { base with
        event_type = "heartbeat"
      ; message = Some (sprintf "%Ld" sequence)
      }
    | Event.Strategy (Signal_generated { signal_type; symbol; reason; _ }) ->
      { base with
        event_type = "signal"
      ; symbol
      ; message = Some (sprintf "%s: %s"
          (Sexp.to_string (Event.Strategy_event.sexp_of_signal_type signal_type))
          reason)
      }
    | Event.Strategy (Position_changed { symbol; venue; new_qty; _ }) ->
      { base with
        event_type = "position_changed"
      ; symbol = Some symbol
      ; venue = Some (Event.Venue.to_string venue)
      ; qty = Some new_qty
      }
    | Event.Strategy (Pnl_update { symbol; venue; total; _ }) ->
      { base with
        event_type = "pnl_update"
      ; symbol = Some symbol
      ; venue = Some (Event.Venue.to_string venue)
      ; price = Some total  (* Using price field for P&L *)
      }
end

(** Export events to CSV *)
let export_csv ~events ~output_path ~include_header =
  let%bind writer = Writer.open_file output_path in
  let%bind () =
    match include_header with
    | true -> Writer.write_line writer Row.csv_header; return ()
    | false -> return ()
  in
  List.iter events ~f:(fun env ->
    let row = Row.of_envelope env in
    Writer.write_line writer (Row.to_csv row)
  );
  Writer.close writer

(** Export events to JSON Lines *)
let export_jsonl ~events ~output_path =
  let%bind writer = Writer.open_file output_path in
  List.iter events ~f:(fun env ->
    let row = Row.of_envelope env in
    Writer.write_line writer (Row.to_json row)
  );
  Writer.close writer

(** Streaming export to CSV *)
let streaming_export_csv ~events ~output_path ~include_header =
  let%bind writer = Writer.open_file output_path in
  let%bind () =
    match include_header with
    | true -> Writer.write_line writer Row.csv_header; return ()
    | false -> return ()
  in
  let%bind () = Pipe.iter events ~f:(fun env ->
    let row = Row.of_envelope env in
    Writer.write_line writer (Row.to_csv row);
    return ()
  )
  in
  Writer.close writer

(** Streaming export to JSON Lines *)
let streaming_export_jsonl ~events ~output_path =
  let%bind writer = Writer.open_file output_path in
  let%bind () = Pipe.iter events ~f:(fun env ->
    let row = Row.of_envelope env in
    Writer.write_line writer (Row.to_json row);
    return ()
  )
  in
  Writer.close writer

(** Export with config *)
let export ~events ~config =
  let output_path = config.Config.output_path in
  match config.format with
  | Csv ->
    export_csv ~events ~output_path ~include_header:config.include_header
  | Json_lines ->
    export_jsonl ~events ~output_path
  | Arrow_ipc ->
    (* Arrow IPC requires more complex implementation *)
    (* For now, fall back to CSV *)
    export_csv ~events ~output_path:(output_path ^ ".csv")
      ~include_header:config.include_header

(** Streaming export with config *)
let streaming_export ~events ~config =
  let output_path = config.Config.output_path in
  match config.format with
  | Csv ->
    streaming_export_csv ~events ~output_path ~include_header:config.include_header
  | Json_lines ->
    streaming_export_jsonl ~events ~output_path
  | Arrow_ipc ->
    streaming_export_csv ~events ~output_path:(output_path ^ ".csv")
      ~include_header:config.include_header

(** Convert event store to export format *)
let convert_store ~input_path ~output_path ~format =
  let%bind reader = Event_store.Reader.open_ ~path:input_path in
  let%bind events = Event_store.Reader.read_all reader in
  let%bind () = Event_store.Reader.close reader in
  let config = { Config.default with output_path; format } in
  export ~events ~config

(** Export specific event types *)
let export_filtered ~events ~output_path ~filter =
  let filtered = List.filter events ~f:filter in
  export_csv ~events:filtered ~output_path ~include_header:true

(** Export only order events *)
let export_orders ~events ~output_path =
  export_filtered ~events ~output_path ~filter:(fun env ->
    match env.Event.event with
    | Event.Order _ -> true
    | _ -> false
  )

(** Export only trade events *)
let export_trades ~events ~output_path =
  export_filtered ~events ~output_path ~filter:(fun env ->
    match env.Event.event with
    | Event.Market (Trade _) -> true
    | Event.Order (Order_filled _) -> true
    | Event.Order (Order_partially_filled _) -> true
    | _ -> false
  )

(** Python helper script for loading exported data *)
let python_helper_script = {|
# Python script for loading Fluxum event exports
import pandas as pd

def load_events(path: str) -> pd.DataFrame:
    """Load events from CSV export."""
    return pd.read_csv(path, parse_dates=['timestamp'])

def load_events_jsonl(path: str) -> pd.DataFrame:
    """Load events from JSON Lines export."""
    return pd.read_json(path, lines=True)

def filter_orders(df: pd.DataFrame) -> pd.DataFrame:
    """Filter to order-related events."""
    return df[df['category'] == 'order']

def filter_trades(df: pd.DataFrame) -> pd.DataFrame:
    """Filter to trade-related events."""
    return df[df['event_type'].isin(['trade', 'order_filled', 'order_partial_fill'])]

def calculate_pnl(df: pd.DataFrame, symbol: str = None) -> float:
    """Calculate P&L from fill events."""
    fills = df[df['event_type'].isin(['order_filled', 'order_partial_fill'])]
    if symbol:
        fills = fills[fills['symbol'] == symbol]

    # Simple P&L calculation (would need avg cost for proper calculation)
    buys = fills[fills['side'] == 'buy']
    sells = fills[fills['side'] == 'sell']

    buy_value = (buys['price'] * buys['qty']).sum()
    sell_value = (sells['price'] * sells['qty']).sum()
    fees = fills['fee'].sum()

    return sell_value - buy_value - fees

def event_summary(df: pd.DataFrame) -> dict:
    """Generate summary statistics."""
    return {
        'total_events': len(df),
        'event_types': df['event_type'].value_counts().to_dict(),
        'venues': df['venue'].dropna().unique().tolist(),
        'symbols': df['symbol'].dropna().unique().tolist(),
        'time_range': (df['timestamp'].min(), df['timestamp'].max()),
    }

if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1:
        df = load_events(sys.argv[1])
        print(event_summary(df))
|}

(** Write Python helper script *)
let write_python_helper ~output_dir =
  let path = output_dir ^/ "load_events.py" in
  Writer.save path ~contents:python_helper_script

(** Export summary statistics *)
module Stats = struct
  type t =
    { total_events : int
    ; event_counts : (string * int) list
    ; venues : string list
    ; symbols : string list
    ; time_range : (Event.Time.t * Event.Time.t) option
    ; total_volume : float
    ; total_fees : float
    }
  [@@deriving sexp_of]

  let of_events events =
    let event_counts = Hashtbl.create (module String) in
    let venues = Hash_set.create (module String) in
    let symbols = Hash_set.create (module String) in
    let first_time = ref None in
    let last_time = ref None in
    let total_volume = ref 0. in
    let total_fees = ref 0. in

    List.iter events ~f:(fun env ->
      (* Time range *)
      (match !first_time with
      | None -> first_time := Some env.Event.timestamp
      | Some _ -> ());
      last_time := Some env.timestamp;

      (* Event type count *)
      let row = Row.of_envelope env in
      Hashtbl.incr event_counts row.event_type;

      (* Venue *)
      Option.iter row.venue ~f:(Hash_set.add venues);

      (* Symbol *)
      Option.iter row.symbol ~f:(Hash_set.add symbols);

      (* Volume and fees *)
      Option.iter row.qty ~f:(fun q -> total_volume := !total_volume +. q);
      Option.iter row.fee ~f:(fun f -> total_fees := !total_fees +. f)
    );

    { total_events = List.length events
    ; event_counts = Hashtbl.to_alist event_counts
    ; venues = Hash_set.to_list venues
    ; symbols = Hash_set.to_list symbols
    ; time_range = (match !first_time, !last_time with
        | Some f, Some l -> Some (f, l)
        | _ -> None)
    ; total_volume = !total_volume
    ; total_fees = !total_fees
    }

  let to_string t =
    let buf = Buffer.create 512 in
    Buffer.add_string buf (sprintf "Total Events: %d\n" t.total_events);
    Buffer.add_string buf "Event Counts:\n";
    List.iter t.event_counts ~f:(fun (typ, count) ->
      Buffer.add_string buf (sprintf "  %s: %d\n" typ count)
    );
    Buffer.add_string buf (sprintf "Venues: %s\n" (String.concat ~sep:", " t.venues));
    Buffer.add_string buf (sprintf "Symbols: %s\n" (String.concat ~sep:", " t.symbols));
    (match t.time_range with
    | Some (first, last) ->
      Buffer.add_string buf (sprintf "Time Range: %s to %s\n"
        (Event.Time.to_string first)
        (Event.Time.to_string last))
    | None -> ());
    Buffer.add_string buf (sprintf "Total Volume: %.8f\n" t.total_volume);
    Buffer.add_string buf (sprintf "Total Fees: %.8f\n" t.total_fees);
    Buffer.contents buf
end
