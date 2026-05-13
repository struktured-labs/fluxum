open Core
open Async

type error =
  [ `Quote_error of Jupiter.error
  | `Swap_build_error of Jupiter.error
  | `Sign_error of Transaction.error
  | `Submit_error of Solana_rpc.error
  | `Confirm_error of Solana_rpc.error
  ]

let sexp_of_error = function
  | `Quote_error e -> Sexp.List [ Sexp.Atom "Quote_error"; Jupiter.sexp_of_error e ]
  | `Swap_build_error e -> Sexp.List [ Sexp.Atom "Swap_build_error"; Jupiter.sexp_of_error e ]
  | `Sign_error e -> Sexp.List [ Sexp.Atom "Sign_error"; Transaction.sexp_of_error e ]
  | `Submit_error e -> Sexp.List [ Sexp.Atom "Submit_error"; Solana_rpc.sexp_of_error e ]
  | `Confirm_error e -> Sexp.List [ Sexp.Atom "Confirm_error"; Solana_rpc.sexp_of_error e ]

type ok =
  { quote : Jupiter.quote
  ; signature : string
  ; confirmation : Solana_rpc.signature_status option
  ; tx_error : string option
  }

let swap
    ?(rpc_url = Solana_rpc.default_rpc_url)
    ?(slippage_bps = 50)
    ?(confirm_until = `Confirmed)
    ?(confirm_timeout = Time_float.Span.of_sec 60.0)
    ?(poll = Time_float.Span.of_sec 1.0)
    ?(skip_preflight = false)
    ~keypair
    ~input_mint
    ~output_mint
    ~amount
    ()
  =
  let user_pubkey = Keypair.pubkey_base58 keypair in
  match%bind Jupiter.get_quote ~input_mint ~output_mint ~amount ~slippage_bps () with
  | Error e -> return (Error (`Quote_error e))
  | Ok quote ->
    (match%bind Jupiter.get_swap_tx ~quote ~user_pubkey with
     | Error e -> return (Error (`Swap_build_error e))
     | Ok tx_b64 ->
       (match Transaction.sign_jupiter_swap_b64 ~keypair tx_b64 with
        | Error e -> return (Error (`Sign_error e))
        | Ok signed_b64 ->
          (match%bind
             Solana_rpc.send_transaction ~rpc_url ~skip_preflight ~tx_base64:signed_b64 ()
           with
           | Error e -> return (Error (`Submit_error e))
           | Ok signature ->
             (match%bind
                Solana_rpc.confirm
                  ~rpc_url
                  ~until:confirm_until
                  ~timeout:confirm_timeout
                  ~poll
                  ~signature_base58:signature
                  ()
              with
              | Error e -> return (Error (`Confirm_error e))
              | Ok `Timeout ->
                return
                  (Ok
                     { quote
                     ; signature
                     ; confirmation = None
                     ; tx_error = None
                     })
              | Ok (`Tx_error msg) ->
                return
                  (Ok
                     { quote
                     ; signature
                     ; confirmation = None
                     ; tx_error = Some msg
                     })
              | Ok (`Confirmed status) ->
                return
                  (Ok
                     { quote
                     ; signature
                     ; confirmation = Some status
                     ; tx_error = None
                     })))))
