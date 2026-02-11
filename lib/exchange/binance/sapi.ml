(** Binance SAPI (Wallet) Endpoints - Deposits and Withdrawals *)

open Core
open Async

(** SAPI-specific request module that uses /sapi/v1/ path instead of /api/v3/ *)
module Sapi_request (Operation : Rest.Operation.S) = struct
  let request (module Cfg : Cfg.S) (req : Operation.request) =
    let base_params = Operation.request_to_params req in

    (* Add timestamp and signature for authenticated endpoints *)
    let params, headers =
      match Operation.requires_auth with
      | true ->
        let timestamp = Signature.generate_timestamp () in
        let params_with_ts = base_params @ [ ("timestamp", timestamp) ] in
        let signature =
          Signature.sign ~api_secret:Cfg.api_secret ~params:params_with_ts
        in
        let final_params = params_with_ts @ [ ("signature", signature) ] in
        let headers =
          Cohttp.Header.of_list
            [ ("X-MBX-APIKEY", Cfg.api_key)
            ; ("Content-Type", "application/json")
            ]
        in
        (final_params, headers)
      | false ->
        let headers =
          Cohttp.Header.of_list [ ("Content-Type", "application/json") ]
        in
        (base_params, headers)
    in

    (* SAPI uses /sapi/v1/ path prefix *)
    let path = sprintf "/sapi/v1/%s" Operation.endpoint in
    let uri =
      Uri.make
        ~scheme:"https"
        ~host:Cfg.base_url
        ~path
        ~query:(List.map params ~f:(fun (k, v) -> (k, [ v ])))
        ()
    in

    Log.Global.debug
      "Binance SAPI call: %s %s"
      (match Operation.http_method with
      | `GET -> "GET"
      | `POST -> "POST"
      | `PUT -> "PUT"
      | `DELETE -> "DELETE")
      Operation.endpoint;

    let make_request () =
      match Operation.http_method with
      | `GET ->
        (match Operation.requires_auth with
        | true -> Cohttp_async.Client.get ~headers ?interrupt:None ?ssl_config:None uri
        | false -> Cohttp_async.Client.get ?interrupt:None ?ssl_config:None uri)
      | `POST ->
        (* For POST, include params in both query string and body *)
        let query_string = Signature.build_query_string params in
        let body = Cohttp_async.Body.of_string query_string in
        let headers =
          Cohttp.Header.replace headers "Content-Type" "application/x-www-form-urlencoded"
        in
        Cohttp_async.Client.post ~headers ~body ?chunked:None ?interrupt:None ?ssl_config:None uri
      | `PUT ->
        (* For PUT, include params in query string *)
        let query_string = Signature.build_query_string params in
        let body = Cohttp_async.Body.of_string query_string in
        let headers =
          Cohttp.Header.replace headers "Content-Type" "application/x-www-form-urlencoded"
        in
        Cohttp_async.Client.put ~headers ~body ?chunked:None ?interrupt:None ?ssl_config:None uri
      | `DELETE -> Cohttp_async.Client.delete ~headers ?chunked:None ?interrupt:None ?ssl_config:None uri
    in

    make_request () >>= fun (response, body) ->
    match Cohttp.Response.status response with
    | `OK | `Created ->
      Cohttp_async.Body.to_string body >>| fun s ->
      Log.Global.debug "SAPI Response: %s" s;
      (try
        let json = Yojson.Safe.from_string s in
        Rest.Response.parse json Operation.response_of_yojson
      with e -> `Json_parse_error Rest.Error.{ message = Exn.to_string e; body = s })
    | `Bad_request ->
      Cohttp_async.Body.to_string body >>| fun b -> `Bad_request b
    | `Not_found -> return `Not_found
    | `Unauthorized ->
      Cohttp_async.Body.to_string body >>| fun b -> `Unauthorized b
    | `Forbidden ->
      Cohttp_async.Body.to_string body >>| fun b -> `Forbidden b
    | `Too_many_requests ->
      Cohttp_async.Body.to_string body >>| fun b -> `Too_many_requests b
    | `Service_unavailable ->
      Cohttp_async.Body.to_string body >>| fun b -> `Service_unavailable b
    | (code : Cohttp.Code.status_code) ->
      Cohttp_async.Body.to_string body >>| fun b ->
      failwiths ~here:[%here]
        (sprintf "Unexpected Binance SAPI status code (body=%S)" b)
        code Cohttp.Code.sexp_of_status_code
end

(** Functor for SAPI operations *)
module Make (Operation : Rest.Operation.S) = struct
  include Sapi_request (Operation)
end

(* ===== WALLET / CAPITAL ENDPOINTS ===== *)

(** Get deposit address - GET /sapi/v1/capital/deposit/address *)
module Deposit_address = struct
  module T = struct
    let name = "deposit-address"
    let endpoint = "capital/deposit/address"
    let http_method = `GET
    let requires_auth = true

    type request =
      { coin : string
      ; network : string option [@default None]
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { coin; network; recvWindow } =
      let base = [ ("coin", coin) ] in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "network" network
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)

    type response =
      { address : string
      ; coin : string
      ; tag : string [@default ""]
      ; url : string [@default ""]
      }
    [@@deriving sexp, of_yojson { strict = false }]
  end

  include T
  include Make (T)
end

(** Get deposit history - GET /sapi/v1/capital/deposit/hisrec *)
module Deposit_history = struct
  module T = struct
    let name = "deposit-history"
    let endpoint = "capital/deposit/hisrec"
    let http_method = `GET
    let requires_auth = true

    type request =
      { coin : string option [@default None]
      ; status : int option [@default None]  (* 0=pending, 1=success, 6=credited *)
      ; startTime : int64 option [@default None]
      ; endTime : int64 option [@default None]
      ; offset : int option [@default None]
      ; limit : int option [@default None]
      ; recvWindow : int option [@default None]
      ; txId : string option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { coin; status; startTime; endTime; offset; limit; recvWindow; txId } =
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      []
      |> add_opt "coin" coin
      |> add_opt "status" (Option.map status ~f:Int.to_string)
      |> add_opt "startTime" (Option.map startTime ~f:Int64.to_string)
      |> add_opt "endTime" (Option.map endTime ~f:Int64.to_string)
      |> add_opt "offset" (Option.map offset ~f:Int.to_string)
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)
      |> add_opt "txId" txId

    (** Deposit record from Binance API *)
    type deposit =
      { id : string [@default ""]
      ; amount : string
      ; coin : string
      ; network : string
      ; status : int
      ; address : string
      ; addressTag : string [@default ""]
      ; txId : string [@default ""]
      ; insertTime : int64
      ; transferType : int [@default 0]
      ; confirmTimes : string [@default "0/0"]
      ; unlockConfirm : int [@default 0]
      ; walletType : int [@default 0]
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response = deposit list [@@deriving sexp, of_yojson]
  end

  include T
  include Make (T)
end

(** Withdraw - POST /sapi/v1/capital/withdraw/apply *)
module Withdraw = struct
  module T = struct
    let name = "withdraw"
    let endpoint = "capital/withdraw/apply"
    let http_method = `POST
    let requires_auth = true

    type request =
      { coin : string
      ; withdrawOrderId : string option [@default None]  (* Client-defined ID *)
      ; network : string option [@default None]
      ; address : string
      ; addressTag : string option [@default None]  (* Memo for XRP, XLM, etc. *)
      ; amount : string
      ; transactionFeeFlag : bool option [@default None]  (* true = fee from amount *)
      ; name : string option [@default None]  (* Address label *)
      ; walletType : int option [@default None]  (* 0=spot, 1=funding *)
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params
        { coin; withdrawOrderId; network; address; addressTag; amount
        ; transactionFeeFlag; name; walletType; recvWindow } =
      let base =
        [ ("coin", coin)
        ; ("address", address)
        ; ("amount", amount)
        ]
      in
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      base
      |> add_opt "withdrawOrderId" withdrawOrderId
      |> add_opt "network" network
      |> add_opt "addressTag" addressTag
      |> add_opt "transactionFeeFlag" (Option.map transactionFeeFlag ~f:(function
        | true -> "true"
        | false -> "false"))
      |> add_opt "name" name
      |> add_opt "walletType" (Option.map walletType ~f:Int.to_string)
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)

    type response =
      { id : string
      }
    [@@deriving sexp, of_yojson]
  end

  include T
  include Make (T)
end

(** Get withdrawal history - GET /sapi/v1/capital/withdraw/history *)
module Withdrawal_history = struct
  module T = struct
    let name = "withdrawal-history"
    let endpoint = "capital/withdraw/history"
    let http_method = `GET
    let requires_auth = true

    type request =
      { coin : string option [@default None]
      ; withdrawOrderId : string option [@default None]
      ; status : int option [@default None]
        (* 0=email sent, 1=cancelled, 2=awaiting approval, 3=rejected,
           4=processing, 5=failure, 6=completed *)
      ; startTime : int64 option [@default None]
      ; endTime : int64 option [@default None]
      ; offset : int option [@default None]
      ; limit : int option [@default None]
      ; recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { coin; withdrawOrderId; status; startTime; endTime; offset; limit; recvWindow } =
      let add_opt key = function
        | None -> Fun.id
        | Some v -> List.cons (key, v)
      in
      []
      |> add_opt "coin" coin
      |> add_opt "withdrawOrderId" withdrawOrderId
      |> add_opt "status" (Option.map status ~f:Int.to_string)
      |> add_opt "startTime" (Option.map startTime ~f:Int64.to_string)
      |> add_opt "endTime" (Option.map endTime ~f:Int64.to_string)
      |> add_opt "offset" (Option.map offset ~f:Int.to_string)
      |> add_opt "limit" (Option.map limit ~f:Int.to_string)
      |> add_opt "recvWindow" (Option.map recvWindow ~f:Int.to_string)

    (** Withdrawal record from Binance API *)
    type withdrawal =
      { id : string
      ; amount : string
      ; transactionFee : string [@default "0"]
      ; coin : string
      ; status : int
      ; address : string
      ; txId : string [@default ""]
      ; applyTime : string  (* ISO datetime string *)
      ; network : string [@default ""]
      ; transferType : int [@default 0]
      ; withdrawOrderId : string option [@default None]
      ; info : string [@default ""]
      ; confirmNo : int [@default 0]
      ; walletType : int [@default 0]
      ; txKey : string [@default ""]
      ; completeTime : string option [@default None]
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response = withdrawal list [@@deriving sexp, of_yojson]
  end

  include T
  include Make (T)
end

(** All coins info (networks, fees, limits) - GET /sapi/v1/capital/config/getall *)
module All_coins_info = struct
  module T = struct
    let name = "all-coins-info"
    let endpoint = "capital/config/getall"
    let http_method = `GET
    let requires_auth = true

    type request =
      { recvWindow : int option [@default None]
      }
    [@@deriving sexp]

    let request_to_params { recvWindow } =
      match recvWindow with
      | None -> []
      | Some rw -> [ ("recvWindow", Int.to_string rw) ]

    (** Network info for a coin *)
    type network_info =
      { network : string
      ; coin : string [@default ""]
      ; withdrawIntegerMultiple : string [@default "0"]
      ; isDefault : bool [@default false]
      ; depositEnable : bool
      ; withdrawEnable : bool
      ; depositDesc : string [@default ""]
      ; withdrawDesc : string [@default ""]
      ; specialTips : string [@default ""]
      ; specialWithdrawTips : string [@default ""]
      ; name : string [@default ""]
      ; resetAddressStatus : bool [@default false]
      ; addressRegex : string [@default ""]
      ; addressRule : string [@default ""]
      ; memoRegex : string [@default ""]
      ; withdrawFee : string [@default "0"]
      ; withdrawMin : string [@default "0"]
      ; withdrawMax : string [@default "0"]
      ; minConfirm : int [@default 0]
      ; unLockConfirm : int [@default 0]
      ; sameAddress : bool [@default false]
      ; estimatedArrivalTime : int [@default 0]
      ; busy : bool [@default false]
      ; country : string [@default ""]
      ; contractAddressUrl : string [@default ""]
      ; contractAddress : string [@default ""]
      }
    [@@deriving sexp, of_yojson { strict = false }]

    (** Coin info *)
    type coin_info =
      { coin : string
      ; depositAllEnable : bool [@default true]
      ; withdrawAllEnable : bool [@default true]
      ; name : string
      ; free : string [@default "0"]
      ; locked : string [@default "0"]
      ; freeze : string [@default "0"]
      ; withdrawing : string [@default "0"]
      ; ipoing : string [@default "0"]
      ; ipoable : string [@default "0"]
      ; storage : string [@default "0"]
      ; isLegalMoney : bool [@default false]
      ; trading : bool [@default true]
      ; networkList : network_info list [@default []]
      }
    [@@deriving sexp, of_yojson { strict = false }]

    type response = coin_info list [@@deriving sexp, of_yojson]
  end

  include T
  include Make (T)
end
