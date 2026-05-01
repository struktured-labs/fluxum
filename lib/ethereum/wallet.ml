(** Read-only Ethereum wallet introspection. *)

open Core

module Eth_rpc = Rpc

open Async

(** ~12 seconds per block on Ethereum mainnet (post-Merge). L2s differ. *)
let mainnet_block_time_seconds = 12.

let blocks_in_span (span : Core.Time_float.Span.t) : int =
  let secs = Core.Time_float.Span.to_sec span in
    Float.to_int (secs /. mainnet_block_time_seconds)

let default_lookback_span = Core.Time_float.Span.of_day 7.

(** Default Ethereum mainnet RPC endpoint. As of v0.11.0: 1rpc.io/eth, which
    handles Multicall3 batched calls cleanly without auth or rate-limits.
    See [doc/eth_rpc_reliability.md] for empirical comparison.
    Consumers should reference this constant rather than hardcoding so they
    auto-benefit from future updates without consumer-side code changes. *)
let default_rpc_url = "https://1rpc.io/eth"

(** Default block-chunk size for paginated [eth_getLogs] queries.
    10,000 matches most public-RPC provider caps. *)
let default_chunk_size = 10_000

(** Default politeness pause between paginated [eth_getLogs] chunks (200ms).
    Prevents free-tier rate-limit responses on long lookbacks. *)
let default_pause_between_chunks = Core.Time_float.Span.of_ms 200.

let eth_balance ~rpc_url ~address =
  Eth_rpc.eth_get_balance ~rpc_url ~address
  >>| function
  | Error e -> Error e
  | Ok hex ->
    Ok
      (Erc20.Token_amount.create
         ~raw_hex:hex
         ~decimals:18
         ~symbol:"ETH"
         ())

let erc20_balance ~rpc_url ~address ~contract :
  (Erc20.Token_amount.t, Eth_rpc.error) Result.t Deferred.t =
  let balance_d = Erc20.balance_of ~rpc_url ~token:contract ~owner:address in
  let decimals_d = Erc20.decimals ~rpc_url ~token:contract in
  let symbol_d = Erc20.symbol ~rpc_url ~token:contract in
  let%bind balance = balance_d in
  let%bind decimals = decimals_d in
  let%bind symbol_r = symbol_d in
    match balance with
    | Error e -> return (Error e)
    | Ok raw_hex ->
      (match decimals with
       | Error e -> return (Error e)
       | Ok decimals ->
         let symbol =
           match symbol_r with
           | Ok s -> Some s
           | Error _ -> None
         in
           return (Ok (Erc20.Token_amount.create ~raw_hex ~decimals ?symbol ())))

(** Build calldata for [balanceOf(address)] without calling — needed when
    we're going to submit it through Multicall3 instead of a direct eth_call. *)
let balance_of_calldata ~owner =
  Abi.encode_function_call
    ~selector:(Abi.function_selector ~signature:"balanceOf(address)")
    ~args:[Address owner]

let decimals_calldata () =
  Abi.encode_function_call
    ~selector:(Abi.function_selector ~signature:"decimals()")
    ~args:[]

let symbol_calldata () =
  Abi.encode_function_call
    ~selector:(Abi.function_selector ~signature:"symbol()")
    ~args:[]

let parse_uint256_returndata (hex : string) : string =
  (* balanceOf returns a single uint256 = exactly 32 bytes; just trim the
     "0x" prefix and keep the canonical form. *)
  hex

let parse_uint8_returndata (hex : string) : int =
  Eth_rpc.hex_to_int hex

let parse_string_returndata (hex : string) : string option =
  let stripped =
    match String.is_prefix hex ~prefix:"0x" with
    | true -> String.drop_prefix hex 2
    | false -> hex
  in
  let total = String.length stripped in
    match total >= 128 with
    | false -> None
    | true ->
      (try
         let length_hex = String.sub stripped ~pos:64 ~len:64 in
         let length = Int.of_string ("0x" ^ length_hex) in
           match length > 0 && length <= 64 with
           | false -> None
           | true ->
             let data_chars = length * 2 in
               match 128 + data_chars > total with
               | true -> None
               | false ->
                 let raw = String.sub stripped ~pos:128 ~len:data_chars in
                   Some (Hex.to_string (`Hex raw))
       with _ -> None)

let erc20_balances ~rpc_url ~address ~contracts :
  (Erc20.Token_amount.t list, Eth_rpc.error) Result.t Deferred.t =
  match contracts with
  | [] -> return (Ok [])
  | _ ->
    let balance_calls =
      List.map contracts ~f:(fun contract ->
        { Multicall3.target = contract
        ; allow_failure = true
        ; call_data = balance_of_calldata ~owner:address })
    in
    let meta_calls =
      List.concat_map contracts ~f:(fun contract ->
        [ { Multicall3.target = contract
          ; allow_failure = true
          ; call_data = decimals_calldata () }
        ; { Multicall3.target = contract
          ; allow_failure = true
          ; call_data = symbol_calldata () }
        ])
    in
    let bal_d = Multicall3.aggregate3 ~rpc_url balance_calls in
    let meta_d = Multicall3.aggregate3 ~rpc_url meta_calls in
    let%bind bal_r = bal_d in
    let%bind meta_r = meta_d in
      match bal_r with
      | Error e -> return (Error e)
      | Ok bal_results ->
        (match meta_r with
         | Error e -> return (Error e)
         | Ok meta_results ->
           let meta_pairs =
             List.groupi meta_results ~break:(fun i _ _ -> i % 2 = 0)
           in
           let zipped =
             List.map3_exn contracts bal_results meta_pairs ~f:(fun _c bal meta ->
               let raw_hex =
                 match bal.success with
                 | true -> parse_uint256_returndata bal.return_data
                 | false -> "0x0"
               in
               let decimals, symbol =
                 match meta with
                 | [d; s] ->
                   let dec =
                     match d.success with
                     | true ->
                       (try parse_uint8_returndata d.return_data with _ -> 18)
                     | false -> 18
                   in
                   let sym =
                     match s.success with
                     | true -> parse_string_returndata s.return_data
                     | false -> None
                   in
                     (dec, sym)
                 | _ -> (18, None)
               in
                 Erc20.Token_amount.create ~raw_hex ~decimals ?symbol ())
           in
             return (Ok zipped))

module Transfer = struct
  type t =
    { block_number : int
    ; tx_hash : string
    ; log_index : int
    ; from_addr : string
    ; to_addr : string
    ; value : Erc20.Token_amount.t
    ; direction : [ `Incoming | `Outgoing | `Self ]
    }
  [@@deriving sexp]
end

(** Decode one log entry into a Transfer.t, given the wallet address being
    queried (used to label direction) and known token decimals/symbol. *)
let decode_transfer_log
    ~queried_address
    ~decimals
    ~symbol
    (log : Yojson.Safe.t) : Transfer.t option =
  let lower_addr = String.lowercase queried_address in
    match log with
    | `Assoc fields ->
      let str_field name =
        match List.Assoc.find fields ~equal:String.equal name with
        | Some (`String s) -> Some s
        | _ -> None
      in
      let topics =
        match List.Assoc.find fields ~equal:String.equal "topics" with
        | Some (`List ts) ->
          List.filter_map ts ~f:(function `String s -> Some s | _ -> None)
        | _ -> []
      in
      let strip_topic_to_addr t =
        let s =
          match String.is_prefix t ~prefix:"0x" with
          | true -> String.drop_prefix t 2
          | false -> t
        in
        (* Last 20 bytes (40 hex chars) are the address *)
        let len = String.length s in
          match len >= 40 with
          | true -> "0x" ^ String.lowercase (String.sub s ~pos:(len - 40) ~len:40)
          | false -> "0x" ^ String.lowercase s
      in
        (match topics with
         | _topic0 :: from_topic :: to_topic :: _ ->
           let block_number =
             Option.bind (str_field "blockNumber") ~f:(fun h ->
               try Some (Eth_rpc.hex_to_int h) with _ -> None)
             |> Option.value ~default:0
           in
           let tx_hash = Option.value (str_field "transactionHash") ~default:"" in
           let log_index =
             Option.bind (str_field "logIndex") ~f:(fun h ->
               try Some (Eth_rpc.hex_to_int h) with _ -> None)
             |> Option.value ~default:0
           in
           let from_addr = strip_topic_to_addr from_topic in
           let to_addr = strip_topic_to_addr to_topic in
           let value =
             Option.value (str_field "data") ~default:"0x0"
           in
           let value_amt =
             Erc20.Token_amount.create ~raw_hex:value ~decimals ?symbol ()
           in
           let direction =
             match
               String.equal from_addr lower_addr
               , String.equal to_addr lower_addr
             with
             | true, true -> `Self
             | true, false -> `Outgoing
             | false, true -> `Incoming
             | false, false -> `Outgoing
             (* Shouldn't happen given our topic filters, but stay safe. *)
           in
             Some
               { block_number
               ; tx_hash
               ; log_index
               ; from_addr
               ; to_addr
               ; value = value_amt
               ; direction }
         | _ -> None)
    | _ -> None

let chunk_size = default_chunk_size

let pause_between_chunks = default_pause_between_chunks

(** Iterate [from_block..to_block] in [chunk_size]-block windows, calling [f]
    on each window. Pauses between windows. Aborts on first error. *)
let paginate_blocks ~from_block ~to_block ~f =
  let rec loop acc cur =
    match cur > to_block with
    | true -> return (Ok (List.concat (List.rev acc)))
    | false ->
      let chunk_end = Int.min (cur + chunk_size - 1) to_block in
        f ~from_block:cur ~to_block:chunk_end
        >>= function
        | Error e -> return (Error e)
        | Ok results ->
          let acc = results :: acc in
          let next = chunk_end + 1 in
            (match next > to_block with
             | true -> return (Ok (List.concat (List.rev acc)))
             | false ->
               let%bind () = Clock.after pause_between_chunks in
                 loop acc next)
  in
    loop [] from_block

let recent_transfers
    ~rpc_url
    ~address
    ~contract
    ?(since = default_lookback_span)
    ?from_block
    ?to_block
    ()
  =
  let%bind end_block_r =
    match to_block with
    | Some b -> return (Ok b)
    | None -> Eth_rpc.eth_block_number ~rpc_url
  in
    match end_block_r with
    | Error e ->
      return (Error (e :> [> Eth_rpc.log_error | `Json_parse of string ]))
    | Ok end_block ->
      let start_block =
        match from_block with
        | Some b -> b
        | None -> Int.max 0 (end_block - blocks_in_span since)
      in
      let address_topic = Erc20.address_to_topic address in
      let outgoing_topics =
        [Some Erc20.transfer_event_topic; Some address_topic; None]
      in
      let incoming_topics =
        [Some Erc20.transfer_event_topic; None; Some address_topic]
      in
      let fetch_window topics ~from_block ~to_block =
        Eth_rpc.eth_get_logs
          ~rpc_url
          ~address:contract
          ~topics
          ~from_block:(`Number from_block)
          ~to_block:(`Number to_block)
          ()
      in
      (* Fetch decimals/symbol once, in parallel with the log scans. *)
      let decimals_d = Erc20.decimals ~rpc_url ~token:contract in
      let symbol_d = Erc20.symbol ~rpc_url ~token:contract in
      let outgoing_d =
        paginate_blocks ~from_block:start_block ~to_block:end_block ~f:(fun ~from_block ~to_block ->
          fetch_window outgoing_topics ~from_block ~to_block)
      in
      let incoming_d =
        paginate_blocks ~from_block:start_block ~to_block:end_block ~f:(fun ~from_block ~to_block ->
          fetch_window incoming_topics ~from_block ~to_block)
      in
      let%bind decimals_r = decimals_d in
      let%bind symbol_r = symbol_d in
      let%bind outgoing_r = outgoing_d in
      let%bind incoming_r = incoming_d in
      let decimals =
        match decimals_r with
        | Ok d -> d
        | Error _ -> 18
      in
      let symbol =
        match symbol_r with
        | Ok s -> Some s
        | Error _ -> None
      in
        match outgoing_r with
        | Error e -> return (Error e)
        | Ok outgoing_logs ->
          (match incoming_r with
           | Error e -> return (Error e)
           | Ok incoming_logs ->
             let all_logs = outgoing_logs @ incoming_logs in
             let transfers =
               List.filter_map all_logs ~f:(decode_transfer_log
                                              ~queried_address:address
                                              ~decimals
                                              ~symbol)
             in
             (* Self-transfers (from = to = queried_address) match BOTH the
                outgoing and incoming topic filters and would appear twice in
                [all_logs]. Dedup by (block_number, tx_hash, log_index) — the
                canonical unique key for a log entry on the chain. *)
             let deduped =
               List.dedup_and_sort
                 transfers
                 ~compare:(fun (a : Transfer.t) (b : Transfer.t) ->
                   match Int.compare a.block_number b.block_number with
                   | 0 ->
                     (match Int.compare a.log_index b.log_index with
                      | 0 -> String.compare a.tx_hash b.tx_hash
                      | c -> c)
                   | c -> c)
             in
               return (Ok deduped))
