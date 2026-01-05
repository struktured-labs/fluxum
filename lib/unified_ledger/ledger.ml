(** Unified Ledger - Multi-symbol P&L tracking

    Manages a collection of ledger entries across symbols and venues.
    Supports:
    - Aggregated P&L across venues
    - Snapshot/restore for persistence
    - Mark-to-market updates
*)

open Core
open Async

(** Key for ledger entries: symbol + venue *)
module Key = struct
  module T = struct
    type t = { symbol : string; venue : Entry.Venue.t }
    [@@deriving sexp, compare, equal, bin_io]
  end
  include T
  include Comparator.Make(T)
end

type t =
  { entries : Entry.t Map.M(Key).t
  ; mutable last_update : Time_ns.t
  }

let create () =
  { entries = Map.empty (module Key)
  ; last_update = Time_ns.now ()
  }

(** Get entry for symbol/venue, creating empty if not exists *)
let get_or_create t ~symbol ~venue =
  let key = { Key.symbol; venue } in
  match Map.find t.entries key with
  | Some entry -> entry
  | None -> Entry.empty ~symbol ~venue

(** Get entry for symbol/venue *)
let get_entry t ~symbol ~venue =
  let key = { Key.symbol; venue } in
  Map.find t.entries key

(** Update an entry *)
let set_entry t entry =
  let key = { Key.symbol = entry.Entry.symbol; venue = entry.venue } in
  { entries = Map.set t.entries ~key ~data:entry
  ; last_update = Time_ns.now ()
  }

(** Apply a fill to the ledger *)
let apply_fill t ~symbol ~venue ~price ~qty ~side ~fee =
  let entry = get_or_create t ~symbol ~venue in
  let side = Entry.Side.of_fluxum_side side in
  let updated = Entry.apply_fill entry ~price ~qty ~side ~fee in
  set_entry t updated, updated

(** Mark all positions to market with current prices *)
let mark_to_market t ~prices =
  let entries =
    Map.mapi t.entries ~f:(fun ~key ~data:entry ->
      match List.Assoc.find prices ~equal:String.equal key.symbol with
      | None -> entry
      | Some price -> Entry.mark_to_market entry ~price
    )
  in
  { entries; last_update = Time_ns.now () }

(** Get all entries *)
let all_entries t =
  Map.data t.entries

(** Get all entries for a specific venue *)
let entries_for_venue t ~venue =
  Map.filter t.entries ~f:(fun entry ->
    Entry.Venue.equal entry.venue venue
  )
  |> Map.data

(** Get all entries for a specific symbol (across venues) *)
let entries_for_symbol t ~symbol =
  Map.filter t.entries ~f:(fun entry ->
    String.equal entry.symbol symbol
  )
  |> Map.data

(** Snapshot all entries for persistence *)
let snapshot t =
  Map.data t.entries

(** Restore from snapshot *)
let restore _t entries =
  let entries_map =
    List.fold entries ~init:(Map.empty (module Key)) ~f:(fun acc entry ->
      let key = { Key.symbol = entry.Entry.symbol; venue = entry.venue } in
      Map.set acc ~key ~data:entry
    )
  in
  { entries = entries_map; last_update = Time_ns.now () }

(** Serialize snapshot to binary *)
let serialize_snapshot entries =
  let writer = Bin_prot.Writer.to_string [%bin_writer: Entry.t list] entries in
  writer

(** Deserialize snapshot from binary *)
let deserialize_snapshot data =
  try Ok (Bin_prot.Reader.of_string [%bin_reader: Entry.t list] data)
  with exn -> Error (Error.of_exn exn)

(** Save snapshot to file *)
let save t ~path =
  let data = serialize_snapshot (snapshot t) in
  Writer.save path ~contents:data

(** Load snapshot from file *)
let load ~path =
  let%bind contents = Reader.file_contents path in
  match deserialize_snapshot contents with
  | Ok entries ->
    let t = create () in
    return (restore t entries)
  | Error e ->
    failwithf "Failed to load ledger: %s" (Error.to_string_hum e) ()

(** Get total P&L across all entries *)
let total_pnl t =
  Map.fold t.entries ~init:0. ~f:(fun ~key:_ ~data:entry acc ->
    acc +. entry.total_pnl
  )

(** Get total realized P&L *)
let total_realized t =
  Map.fold t.entries ~init:0. ~f:(fun ~key:_ ~data:entry acc ->
    acc +. entry.realized_pnl
  )

(** Get total unrealized P&L *)
let total_unrealized t =
  Map.fold t.entries ~init:0. ~f:(fun ~key:_ ~data:entry acc ->
    acc +. entry.unrealized_pnl
  )

(** Get total fees paid *)
let total_fees t =
  Map.fold t.entries ~init:0. ~f:(fun ~key:_ ~data:entry acc ->
    acc +. entry.total_fees
  )

(** Get number of entries *)
let count t = Map.length t.entries

(** Get P&L by venue *)
let pnl_by_venue t =
  Map.fold t.entries ~init:(Map.empty (module Entry.Venue)) ~f:(fun ~key:_ ~data:entry acc ->
    Map.update acc entry.venue ~f:(function
      | None -> entry.total_pnl
      | Some pnl -> pnl +. entry.total_pnl
    )
  )
  |> Map.to_alist

(** Get P&L by symbol *)
let pnl_by_symbol t =
  Map.fold t.entries ~init:(Map.empty (module String)) ~f:(fun ~key:_ ~data:entry acc ->
    Map.update acc entry.symbol ~f:(function
      | None -> entry.total_pnl
      | Some pnl -> pnl +. entry.total_pnl
    )
  )
  |> Map.to_alist

(** Summary for display *)
let summary t =
  let total = total_pnl t in
  let realized = total_realized t in
  let unrealized = total_unrealized t in
  let sign = match Float.(total >= 0.) with true -> "+" | false -> "" in
  sprintf "Ledger: %d entries, P&L: %s%.2f (realized: %.2f, unrealized: %.2f)"
    (count t) sign total realized unrealized

(** Detailed report *)
let report t =
  let buf = Buffer.create 2048 in
  Buffer.add_string buf "\n========== LEDGER REPORT ==========\n";
  Buffer.add_string buf (sprintf "Entries: %d\n" (count t));
  Buffer.add_string buf (sprintf "Last Update: %s\n" (Time_ns.to_string t.last_update));
  Buffer.add_string buf "\n--- P&L Summary ---\n";
  Buffer.add_string buf (sprintf "Total:      %.8f\n" (total_pnl t));
  Buffer.add_string buf (sprintf "Realized:   %.8f\n" (total_realized t));
  Buffer.add_string buf (sprintf "Unrealized: %.8f\n" (total_unrealized t));
  Buffer.add_string buf (sprintf "Total Fees: %.8f\n" (total_fees t));
  Buffer.add_string buf "\n--- By Venue ---\n";
  List.iter (pnl_by_venue t) ~f:(fun (venue, pnl) ->
    Buffer.add_string buf (sprintf "  %-12s: %.8f\n" (Entry.Venue.to_string venue) pnl)
  );
  Buffer.add_string buf "\n--- By Symbol ---\n";
  List.iter (pnl_by_symbol t) ~f:(fun (symbol, pnl) ->
    Buffer.add_string buf (sprintf "  %-12s: %.8f\n" symbol pnl)
  );
  Buffer.add_string buf "\n--- Entries ---\n";
  List.iter (all_entries t) ~f:(fun entry ->
    Buffer.add_string buf (sprintf "  %s\n" (Entry.summary entry))
  );
  Buffer.add_string buf "====================================\n";
  Buffer.contents buf

(** Create pipe for streaming ledger updates *)
let updates_pipe _t =
  let reader, writer = Pipe.create () in
  (reader, writer)
