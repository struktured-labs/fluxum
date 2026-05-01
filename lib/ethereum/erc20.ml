(** ERC-20 Token Operations

    Standard ERC-20 token contract interactions via JSON-RPC.
    Provides read operations (balance, allowance) and calldata
    builders for write operations (approve, transfer).

    @see <https://eips.ethereum.org/EIPS/eip-20> *)

open Core

(** Local Ethereum RPC module - aliased before opening Async which shadows Rpc *)
module Eth_rpc = Rpc

open Async

(** ERC-20 function selectors (first 4 bytes of keccak256 of signature) *)
let _balance_of_selector = Abi.function_selector ~signature:"balanceOf(address)"

let _allowance_selector = Abi.function_selector ~signature:"allowance(address,address)"
let _approve_selector = Abi.function_selector ~signature:"approve(address,uint256)"
let _transfer_selector = Abi.function_selector ~signature:"transfer(address,uint256)"
let _decimals_selector = Abi.function_selector ~signature:"decimals()"
let _symbol_selector = Abi.function_selector ~signature:"symbol()"

(** Selector for the ERC-20 [Transfer(address indexed from, address indexed to, uint256 value)]
    event topic[0]. Used by log filtering. *)
let transfer_event_topic =
  let hash = Digestif.KECCAK_256.digest_string "Transfer(address,address,uint256)" in
  let raw = Digestif.KECCAK_256.to_raw_string hash in
    "0x" ^ (Hex.of_string raw |> Hex.show)

(** Encode an Ethereum address as a 32-byte ABI topic value (left-padded). *)
let address_to_topic (addr : string) : string =
  let stripped =
    match String.is_prefix addr ~prefix:"0x" with
    | true -> String.drop_prefix addr 2
    | false -> addr
  in
    "0x" ^ Abi.pad_left_32 (String.lowercase stripped)

(** Query ERC-20 token balance for an address *)
let balance_of ~rpc_url ~token ~owner =
  let data =
    Abi.encode_function_call ~selector:_balance_of_selector ~args:[Address owner]
  in
    Eth_rpc.eth_call ~rpc_url ~to_:token ~data
    >>| function
    | Ok hex_result -> Ok hex_result (* Returns uint256 as hex *)
    | Error e -> Error e

(** Query ERC-20 allowance: how much spender can spend of owner's tokens *)
let allowance ~rpc_url ~token ~owner ~spender =
  let data =
    Abi.encode_function_call
      ~selector:_allowance_selector
      ~args:[Address owner; Address spender]
  in
    Eth_rpc.eth_call ~rpc_url ~to_:token ~data
    >>| function
    | Ok hex_result -> Ok hex_result
    | Error e -> Error e

(** Build calldata for ERC-20 approve(spender, amount) *)
let approve_calldata ~spender ~amount : string =
  Abi.encode_function_call
    ~selector:_approve_selector
    ~args:[Address spender; Uint256 amount]

(** Build calldata for ERC-20 transfer(to, amount) *)
let transfer_calldata ~to_ ~amount : string =
  Abi.encode_function_call ~selector:_transfer_selector ~args:[Address to_; Uint256 amount]

(** Query token decimals *)
let decimals ~rpc_url ~token =
  let data = Abi.encode_function_call ~selector:_decimals_selector ~args:[] in
    Eth_rpc.eth_call ~rpc_url ~to_:token ~data
    >>| function
    | Ok hex_result -> Ok (Eth_rpc.hex_to_int hex_result)
    | Error e -> Error e

(** Build calldata for unlimited approval (max uint256) *)
let approve_unlimited_calldata ~spender : string =
  approve_calldata ~spender ~amount:Abi.max_uint256

(** Query token symbol. Most ERC-20s return a dynamic string; some legacy tokens
    (MKR-era) encode it as a bytes32 — we try dynamic first, fall back to
    truncated-bytes32 interpretation if the dynamic length looks bogus. *)
let symbol ~rpc_url ~token =
  let data = Abi.encode_function_call ~selector:_symbol_selector ~args:[] in
    Eth_rpc.eth_call ~rpc_url ~to_:token ~data
    >>| function
    | Error e -> Error e
    | Ok hex_result ->
      let stripped =
        match String.is_prefix hex_result ~prefix:"0x" with
        | true -> String.drop_prefix hex_result 2
        | false -> hex_result
      in
      let total_chars = String.length stripped in
      let try_dynamic () =
        match total_chars >= 128 with
        | false -> None
        | true ->
          (try
             let length_hex = String.sub stripped ~pos:64 ~len:64 in
             let length = Int.of_string ("0x" ^ length_hex) in
               match length > 0 && length <= 64 with
               | false -> None
               | true ->
                 let data_chars = length * 2 in
                   match 128 + data_chars > total_chars with
                   | true -> None
                   | false ->
                     let raw_hex = String.sub stripped ~pos:128 ~len:data_chars in
                     let bytes_val =
                       Hex.to_string (`Hex raw_hex)
                     in
                       Some bytes_val
           with _ -> None)
      in
      let try_bytes32 () =
        match total_chars >= 64 with
        | false -> None
        | true ->
          (try
             let raw_hex = String.sub stripped ~pos:0 ~len:64 in
             let bytes_val = Hex.to_string (`Hex raw_hex) in
             let trimmed = String.rstrip ~drop:(Char.equal '\x00') bytes_val in
               match String.is_empty trimmed with
               | true -> None
               | false -> Some trimmed
           with _ -> None)
      in
        (match try_dynamic () with
         | Some s -> Ok s
         | None ->
           (match try_bytes32 () with
            | Some s -> Ok s
            | None -> Error (`Json_parse "Could not decode ERC-20 symbol")))

(** A typed token amount: the raw uint256 (as hex), the token's decimal
    precision, and optionally the symbol. Carries enough context to format
    for display without lossy float conversion at the call site. *)
module Token_amount = struct
  type t =
    { raw_hex : string  (** "0x"-prefixed uint256 hex; canonical form *)
    ; decimals : int
    ; symbol : string option
    }
  [@@deriving sexp]

  (** Construct from the raw hex returned by [balanceOf]. *)
  let create ~raw_hex ~decimals ?symbol () =
    let raw_hex =
      match String.is_prefix raw_hex ~prefix:"0x" with
      | true -> raw_hex
      | false -> "0x" ^ raw_hex
    in
      { raw_hex; decimals; symbol }

  let hex_digit_value (c : char) : int =
    match c with
    | '0' .. '9' -> Char.to_int c - Char.to_int '0'
    | 'a' .. 'f' -> Char.to_int c - Char.to_int 'a' + 10
    | 'A' .. 'F' -> Char.to_int c - Char.to_int 'A' + 10
    | _ -> 0

  (** Convert to [float]. WARNING: lossy for amounts where the integer
      mantissa exceeds 2^53. Use [to_string_decimal] for display. *)
  let to_float (t : t) : float =
    let stripped =
      match String.is_prefix t.raw_hex ~prefix:"0x" with
      | true -> String.drop_prefix t.raw_hex 2
      | false -> t.raw_hex
    in
    (* Use Int64 path when it fits; else fall back to scaled big-string approach. *)
    let raw_str = String.lstrip ~drop:(Char.equal '0') stripped in
      match String.is_empty raw_str with
      | true -> 0.
      | false ->
        (try
           let as_i64 = Int64.of_string ("0x" ^ raw_str) in
           let scale = Float.int_pow 10. t.decimals in
             Int64.to_float as_i64 /. scale
         with _ ->
           (* Out-of-range for int64 — convert via float multiplication.
              Lossy beyond ~15 sig digits. *)
           let n = String.length raw_str in
           let acc = ref 0. in
             String.iter raw_str ~f:(fun c ->
               acc := (!acc *. 16.) +. Float.of_int (hex_digit_value c));
             let _ = n in
             let scale = Float.int_pow 10. t.decimals in
               !acc /. scale)

  (** Format as a decimal string with [decimals] places after the point.
      Lossless — uses big-int arithmetic via string manipulation. *)
  let to_string_decimal (t : t) : string =
    let stripped =
      match String.is_prefix t.raw_hex ~prefix:"0x" with
      | true -> String.drop_prefix t.raw_hex 2
      | false -> t.raw_hex
    in
    let raw_no_lead = String.lstrip ~drop:(Char.equal '0') stripped in
    let raw_no_lead =
      match String.is_empty raw_no_lead with
      | true -> "0"
      | false -> raw_no_lead
    in
    (* Convert hex digits to a base-10 string by repeated mul/add.
       For typical ERC-20 amounts (uint256 max = 78 decimal digits) this is
       trivially fast. *)
    let to_decimal_string (hex : string) : string =
      let digits = ref [0] in
      String.iter hex ~f:(fun c ->
        let v = hex_digit_value c in
        (* Multiply each digit by 16, add v as carry *)
        let carry = ref v in
        digits :=
          List.map !digits ~f:(fun d ->
            let prod = (d * 16) + !carry in
            carry := prod / 10;
            prod % 10);
        while !carry > 0 do
          digits := !digits @ [!carry % 10];
          carry := !carry / 10
        done);
      let s =
        List.rev_map !digits ~f:(fun d -> Char.of_int_exn (d + Char.to_int '0'))
        |> String.of_char_list
      in
        match String.is_empty s with
        | true -> "0"
        | false -> s
    in
    let dec = to_decimal_string raw_no_lead in
    let pad_to = t.decimals + 1 in
    let dec =
      match String.length dec < pad_to with
      | true -> String.make (pad_to - String.length dec) '0' ^ dec
      | false -> dec
    in
    let split_pos = String.length dec - t.decimals in
    let int_part = String.sub dec ~pos:0 ~len:split_pos in
    let frac_part = String.sub dec ~pos:split_pos ~len:t.decimals in
    let frac_trimmed = String.rstrip ~drop:(Char.equal '0') frac_part in
      match String.is_empty frac_trimmed with
      | true -> int_part
      | false -> int_part ^ "." ^ frac_trimmed

  (** Display string with optional symbol suffix. *)
  let to_display (t : t) : string =
    let amt = to_string_decimal t in
      match t.symbol with
      | Some s -> sprintf "%s %s" amt s
      | None -> amt
end

(** Check if allowance is sufficient, and if not, build approve calldata *)
let ensure_allowance ~rpc_url ~token ~owner ~spender ~required_amount =
  let%bind current = allowance ~rpc_url ~token ~owner ~spender in
    match current with
    | Error e -> return (Error e)
    | Ok current_hex ->
      (* Compare as strings - both are 64-char hex, zero-padded *)
      let current_stripped =
        match String.is_prefix current_hex ~prefix:"0x" with
        | true -> String.drop_prefix current_hex 2
        | false -> current_hex
      in
      let required_padded = Abi.pad_left_32 required_amount in
        (match String.( >= ) current_stripped required_padded with
         | true -> return (Ok None) (* Sufficient allowance *)
         | false -> return (Ok (Some (approve_unlimited_calldata ~spender))))
