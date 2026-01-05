(** Bot Dashboard - TUI Monitoring Interface

    Provides real-time visualization of:
    - Bot status and uptime
    - Connection states
    - Positions and P&L
    - Active orders
    - Recent events
*)

open Core
open Async

module Config = struct
  type t =
    { refresh_rate : Time_float.Span.t
    ; max_events : int
    ; show_positions : bool
    ; show_orders : bool
    ; show_events : bool
    ; compact_mode : bool
    }
  [@@deriving sexp]

  let default =
    { refresh_rate = Time_float.Span.of_ms 250.
    ; max_events = 10
    ; show_positions = true
    ; show_orders = true
    ; show_events = true
    ; compact_mode = false
    }
end

(** ANSI color codes *)
module Color = struct
  let reset = "\027[0m"
  let bold = "\027[1m"
  let dim = "\027[2m"

  let red = "\027[31m"
  let green = "\027[32m"
  let yellow = "\027[33m"
  let blue = "\027[34m"
  let magenta = "\027[35m"
  let cyan = "\027[36m"
  let white = "\027[37m"

  let bg_red = "\027[41m"
  let bg_green = "\027[42m"
  let bg_blue = "\027[44m"

  let apply color s = sprintf "%s%s%s" color s reset
  let red_text s = apply red s
  let green_text s = apply green s
  let yellow_text s = apply yellow s
  let blue_text s = apply blue s
  let cyan_text s = apply cyan s
  let bold_text s = apply bold s

  let pnl_color pnl =
    match Float.(pnl >= 0.) with
    | true -> green
    | false -> red
end

(** Format time span for display *)
let format_uptime span =
  let secs = Time_ns.Span.to_sec span |> Int.of_float in
  let hours = secs / 3600 in
  let mins = (secs mod 3600) / 60 in
  let secs = secs mod 60 in
  match hours > 0 with
  | true -> sprintf "%dh %02dm %02ds" hours mins secs
  | false ->
    match mins > 0 with
    | true -> sprintf "%dm %02ds" mins secs
    | false -> sprintf "%ds" secs

(** Format price for display *)
let format_price p =
  match Float.(p >= 1000.) with
  | true -> sprintf "%.2f" p
  | false ->
    match Float.(p >= 1.) with
    | true -> sprintf "%.4f" p
    | false -> sprintf "%.8f" p

(** Format quantity for display *)
let format_qty q =
  match Float.(abs q < 0.00001) with
  | true -> "0"
  | false ->
    match Float.(abs q >= 1.) with
    | true -> sprintf "%.4f" q
    | false -> sprintf "%.8f" q

type t =
  { config : Config.t
  ; mutable recent_events : Event.envelope list
  ; mutable last_render : Time_ns.t
  }

let create config =
  { config
  ; recent_events = []
  ; last_render = Time_ns.epoch
  }

(** Add event to recent events buffer *)
let add_event t envelope =
  let events = envelope :: t.recent_events in
  t.recent_events <- List.take events t.config.max_events

(** Render horizontal line *)
let render_line width =
  String.make width '-'

(** Render header section *)
let render_header ~bot_id ~is_running ~uptime ~width =
  let status = match is_running with
    | true -> Color.green_text "● RUNNING"
    | false -> Color.red_text "○ STOPPED"
  in
  let uptime_str = format_uptime uptime in
  let header = sprintf "Bot: %s  │  Status: %s  │  Uptime: %s"
    (Color.bold_text bot_id) status uptime_str
  in
  let line = render_line width in
  sprintf "┌%s┐\n│ %-*s │\n├%s┤" line (width - 4 + 20) header line

(** Render connections section *)
let render_connections state ~width =
  let conns =
    Map.fold state.State.connections ~init:[] ~f:(fun ~key:venue ~data:conn acc ->
      let status = match conn.State.Connection.status with
        | Ready -> Color.green_text "● Ready"
        | Connected -> Color.green_text "○ Connected"
        | Connecting -> Color.yellow_text "◐ Connecting"
        | Disconnected -> Color.dim ^ "○ Disconnected" ^ Color.reset
        | Reconnecting n -> Color.yellow_text (sprintf "◑ Retry(%d)" n)
        | Failed _ -> Color.red_text "✗ Failed"
      in
      (Event.Venue.to_string venue, status) :: acc
    )
    |> List.rev
  in
  let conn_str = match conns with
    | [] -> "No connections configured"
    | _ ->
      List.map conns ~f:(fun (venue, status) ->
        sprintf "%s: %s" venue status
      )
      |> String.concat ~sep:"  │  "
  in
  sprintf "│ Connections: %-*s │" (width - 18 + 40) conn_str

(** Render positions section *)
let render_positions state ~width =
  let entries = Unified_ledger.Ledger.all_entries state.State.ledger in
  let active_entries = List.filter entries ~f:(fun e ->
    Float.(abs e.Unified_ledger.Entry.position > 0.00000001)
  )
  in
  let lines = Buffer.create 256 in
  Buffer.add_string lines (sprintf "│ Positions %-*s │\n" (width - 14) "");
  match active_entries with
  | [] ->
    Buffer.add_string lines (sprintf "│   %-*s │" (width - 6) "(no positions)");
    Buffer.contents lines
  | _ ->
    List.iter active_entries ~f:(fun entry ->
      let pos_str = match Float.(entry.position >= 0.) with
        | true -> Color.green_text (sprintf "+%s" (format_qty entry.position))
        | false -> Color.red_text (format_qty entry.position)
      in
      let pnl_str =
        let pnl = entry.total_pnl in
        let color = Color.pnl_color pnl in
        let sign = match Float.(pnl >= 0.) with true -> "+" | false -> "" in
        sprintf "%s%s%.2f%s" color sign pnl Color.reset
      in
      Buffer.add_string lines (sprintf "│   %s: %s @ %s  P&L: %s%*s │\n"
        entry.symbol
        pos_str
        (format_price entry.avg_cost)
        pnl_str
        (width - 60) "")
    );
    Buffer.contents lines |> String.rstrip ~drop:(Char.equal '\n')

(** Render P&L summary *)
let render_pnl state ~width =
  let ledger = state.State.ledger in
  let total = Unified_ledger.Ledger.total_pnl ledger in
  let realized = Unified_ledger.Ledger.total_realized ledger in
  let unrealized = Unified_ledger.Ledger.total_unrealized ledger in
  let fees = Unified_ledger.Ledger.total_fees ledger in

  let format_pnl v =
    let color = Color.pnl_color v in
    let sign = match Float.(v >= 0.) with true -> "+" | false -> "" in
    sprintf "%s%s%.2f%s" color sign v Color.reset
  in

  sprintf "│ P&L: Total: %s  │  Realized: %s  │  Unrealized: %s  │  Fees: %.2f%*s │"
    (format_pnl total)
    (format_pnl realized)
    (format_pnl unrealized)
    fees
    (width - 90) ""

(** Render active orders *)
let render_orders state ~width =
  let orders = Map.data state.State.active_orders in
  let lines = Buffer.create 256 in
  let count = List.length orders in
  Buffer.add_string lines (sprintf "│ Active Orders (%d)%-*s │\n" count (width - 21) "");
  match orders with
  | [] ->
    Buffer.add_string lines (sprintf "│   %-*s │" (width - 6) "(no active orders)");
    Buffer.contents lines
  | _ ->
    List.iter (List.take orders 5) ~f:(fun order ->
      let side_str = match order.State.Active_order.side with
        | Buy -> Color.green_text "BUY "
        | Sell -> Color.red_text "SELL"
      in
      let price_str = match order.price with
        | Some p -> sprintf "@ %s" (format_price p)
        | None -> "@ MKT"
      in
      Buffer.add_string lines (sprintf "│   #%s %s %s %s [%s]%*s │\n"
        (String.prefix order.order_id 8)
        side_str
        (format_qty order.remaining_qty)
        order.symbol
        price_str
        (width - 60) "")
    );
    (match count > 5 with
    | true -> Buffer.add_string lines (sprintf "│   ... and %d more%*s │\n" (count - 5) (width - 25) "")
    | false -> ());
    Buffer.contents lines |> String.rstrip ~drop:(Char.equal '\n')

(** Render recent events *)
let render_events t ~width =
  let lines = Buffer.create 512 in
  Buffer.add_string lines (sprintf "│ Recent Events%-*s │\n" (width - 18) "");
  match t.recent_events with
  | [] ->
    Buffer.add_string lines (sprintf "│   %-*s │" (width - 6) "(no events yet)");
    Buffer.contents lines
  | events ->
    List.iter (List.take events t.config.max_events) ~f:(fun env ->
      let time = Time_ns.to_string_abs_parts env.Event.timestamp ~zone:Time_float.Zone.utc
        |> List.last_exn
        |> (fun s -> String.prefix s 12)
      in
      let desc = Event.describe env.event in
      let desc = match String.length desc > (width - 20) with
        | true -> String.prefix desc (width - 23) ^ "..."
        | false -> desc
      in
      let color = match env.event with
        | Event.Order (Order_filled _) -> Color.green
        | Event.Order (Order_rejected _) -> Color.red
        | Event.System (Error _) -> Color.red
        | Event.System (Bot_started _) -> Color.green
        | Event.System (Bot_stopped _) -> Color.yellow
        | _ -> ""
      in
      Buffer.add_string lines (sprintf "│   %s%s %s%s%*s │\n"
        color time desc Color.reset (width - 18 - String.length desc) "")
    );
    Buffer.contents lines |> String.rstrip ~drop:(Char.equal '\n')

(** Render full dashboard *)
let render t state =
  let width = 80 in
  let buf = Buffer.create 2048 in

  (* Clear screen and move cursor to top *)
  Buffer.add_string buf "\027[2J\027[H";

  (* Header *)
  let uptime = State.uptime state in
  Buffer.add_string buf (render_header
    ~bot_id:state.State.bot_id
    ~is_running:state.is_running
    ~uptime
    ~width);
  Buffer.add_char buf '\n';

  (* Connections *)
  Buffer.add_string buf (render_connections state ~width);
  Buffer.add_char buf '\n';
  Buffer.add_string buf (sprintf "├%s┤\n" (render_line width));

  (* Positions and P&L *)
  (match t.config.show_positions with
  | true ->
    Buffer.add_string buf (render_positions state ~width);
    Buffer.add_char buf '\n';
    Buffer.add_string buf (render_pnl state ~width);
    Buffer.add_char buf '\n';
    Buffer.add_string buf (sprintf "├%s┤\n" (render_line width))
  | false -> ());

  (* Active Orders *)
  (match t.config.show_orders with
  | true ->
    Buffer.add_string buf (render_orders state ~width);
    Buffer.add_char buf '\n';
    Buffer.add_string buf (sprintf "├%s┤\n" (render_line width))
  | false -> ());

  (* Recent Events *)
  (match t.config.show_events with
  | true ->
    Buffer.add_string buf (render_events t ~width);
    Buffer.add_char buf '\n'
  | false -> ());

  (* Footer *)
  let now = Time_ns.to_string_abs_parts (Time_ns.now ()) ~zone:Time_float.Zone.utc
    |> String.concat ~sep:" "
  in
  Buffer.add_string buf (sprintf "└%s┘\n" (render_line width));
  Buffer.add_string buf (sprintf "%sLast update: %s  │  Events: %d  │  Press Ctrl+C to stop%s\n"
    Color.dim now state.event_count Color.reset);

  t.last_render <- Time_ns.now ();
  Buffer.contents buf

(** Run dashboard with state updates *)
let run t ~state_reader =
  let rec loop () =
    match%bind Clock.with_timeout t.config.refresh_rate (Pipe.read state_reader) with
    | `Timeout ->
      loop ()
    | `Result `Eof ->
      return ()
    | `Result (`Ok state) ->
      let output = render t state in
      Core.print_string output;
      loop ()
  in
  loop ()

(** Run dashboard with engine *)
let run_with_engine t engine =
  let reader, writer = Pipe.create () in

  (* Start state update loop *)
  don't_wait_for (
    let rec loop () =
      let%bind () = Clock.after t.config.refresh_rate in
      match Engine.is_running engine with
      | false ->
        Pipe.close writer;
        return ()
      | true ->
        let state = Engine.state engine in
        match Pipe.is_closed writer with
        | true -> return ()
        | false ->
          Pipe.write_without_pushback writer state;
          loop ()
    in
    loop ()
  );

  (* Also subscribe to engine events for recent events display *)
  don't_wait_for (
    Pipe.iter (Engine.events engine) ~f:(fun envelope ->
      add_event t envelope;
      return ()
    )
  );

  run t ~state_reader:reader

(** Render once without loop *)
let render_once t state =
  render t state

(** Create simple text summary (no ANSI) *)
let text_summary state =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (sprintf "=== Bot: %s ===\n" state.State.bot_id);
  Buffer.add_string buf (sprintf "Status: %s\n"
    (match state.is_running with true -> "RUNNING" | false -> "STOPPED"));
  Buffer.add_string buf (sprintf "Uptime: %s\n" (format_uptime (State.uptime state)));
  Buffer.add_string buf (sprintf "Events: %d\n" state.event_count);
  Buffer.add_string buf (sprintf "Errors: %d\n" state.error_count);

  Buffer.add_string buf "\nConnections:\n";
  Map.iter state.connections ~f:(fun conn ->
    let status = match conn.State.Connection.status with
      | Ready -> "Ready"
      | Connected -> "Connected"
      | Connecting -> "Connecting"
      | Disconnected -> "Disconnected"
      | Reconnecting n -> sprintf "Reconnecting(%d)" n
      | Failed r -> sprintf "Failed: %s" r
    in
    Buffer.add_string buf (sprintf "  %s: %s\n"
      (Event.Venue.to_string conn.venue) status)
  );

  Buffer.add_string buf "\nP&L:\n";
  Buffer.add_string buf (sprintf "  Total:      %.2f\n" (Unified_ledger.Ledger.total_pnl state.ledger));
  Buffer.add_string buf (sprintf "  Realized:   %.2f\n" (Unified_ledger.Ledger.total_realized state.ledger));
  Buffer.add_string buf (sprintf "  Unrealized: %.2f\n" (Unified_ledger.Ledger.total_unrealized state.ledger));
  Buffer.add_string buf (sprintf "  Fees:       %.2f\n" (Unified_ledger.Ledger.total_fees state.ledger));

  Buffer.add_string buf (sprintf "\nActive Orders: %d\n" (Map.length state.active_orders));

  Buffer.contents buf
