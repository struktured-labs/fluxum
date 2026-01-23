(** KuCoin API Types

    Native KuCoin data structures from REST and WebSocket APIs.
*)

open Core

(** {1 Public API Types} *)

(** Ticker data (24hr stats) *)
type ticker = {
  symbol : string;              (** Trading pair (e.g., "BTC-USDT") *)
  high : string;                (** 24h high price *)
  low : string;                 (** 24h low price *)
  last : string;                (** Last traded price *)
  vol : string;                 (** 24h volume (base currency) *)
  volValue : string;            (** 24h volume (quote currency) *)
  buy : string;                 (** Best bid price *)
  sell : string;                (** Best ask price *)
  changePrice : string;         (** 24h price change *)
  changeRate : string;          (** 24h change rate *)
  averagePrice : string;        (** 24h average price *)
  time : int64;                 (** Timestamp (milliseconds) *)
} [@@deriving yojson { strict = false }, sexp]

(** Order book entry [price, size] *)
type order_book_entry = string * string
[@@deriving sexp]

let order_book_entry_of_yojson = function
  | `List [`String price; `String size] -> Ok (price, size)
  | _ -> Error "Order book entry must be [price, size]"

let order_book_entry_to_yojson (price, size) =
  `List [`String price; `String size]

(** Order book snapshot *)
type order_book = {
  sequence : int64;             (** Sequence number *)
  time : int64;                 (** Timestamp *)
  bids : order_book_entry list; (** Buy orders *)
  asks : order_book_entry list; (** Sell orders *)
} [@@deriving yojson { strict = false }, sexp]

(** Public trade *)
type trade = {
  sequence : string;            (** Sequence number *)
  price : string;               (** Trade price *)
  size : string;                (** Trade size *)
  side : string;                (** "buy" or "sell" *)
  time : int64;                 (** Timestamp (nanoseconds) *)
} [@@deriving yojson { strict = false }, sexp]

(** Symbol info *)
type symbol_info = {
  symbol : string;              (** Trading pair *)
  name : string;                (** Symbol name *)
  baseCurrency : string;        (** Base currency *)
  quoteCurrency : string;       (** Quote currency *)
  baseMinSize : string;         (** Minimum order size *)
  quoteMinSize : string;        (** Minimum order value *)
  baseMaxSize : string;         (** Maximum order size *)
  quoteMaxSize : string;        (** Maximum order value *)
  baseIncrement : string;       (** Size increment *)
  quoteIncrement : string;      (** Price increment *)
  priceIncrement : string;      (** Price step *)
  enableTrading : bool;         (** Trading enabled *)
} [@@deriving yojson { strict = false }, sexp]

(** {1 Private API Types} *)

(** Account balance *)
type balance = {
  id : string;                  (** Account ID *)
  currency : string;            (** Currency code *)
  type_ : string; [@key "type"] (** Account type *)
  balance : string;             (** Total balance *)
  available : string;           (** Available balance *)
  holds : string;               (** Frozen balance *)
} [@@deriving yojson { strict = false }, sexp]

(** Order response *)
type order_response = {
  orderId : string;             (** Order ID *)
} [@@deriving yojson { strict = false }, sexp]

(** Order details *)
type order = {
  id : string;                  (** Order ID *)
  symbol : string;              (** Trading pair *)
  type_ : string; [@key "type"] (** "limit" or "market" *)
  side : string;                (** "buy" or "sell" *)
  price : string;               (** Order price *)
  size : string;                (** Order size *)
  dealSize : string;            (** Filled size *)
  dealFunds : string;           (** Filled value *)
  fee : string;                 (** Trading fee *)
  feeCurrency : string;         (** Fee currency *)
  isActive : bool;              (** Order active status *)
  cancelExist : bool;           (** Has cancel request *)
  createdAt : int64;            (** Creation timestamp *)
} [@@deriving yojson { strict = false }, sexp]

(** {1 WebSocket Types} *)

(** WebSocket connection token response *)
type ws_token_response = {
  instanceServers : ws_server list; (** Available servers *)
  token : string;                   (** Connection token *)
} [@@deriving yojson { strict = false }]

and ws_server = {
  endpoint : string;            (** WebSocket endpoint *)
  encrypt : bool;               (** Encryption enabled *)
  protocol : string;            (** Protocol *)
  pingInterval : int;           (** Ping interval (ms) *)
  pingTimeout : int;            (** Ping timeout (ms) *)
} [@@deriving yojson { strict = false }]

(** WebSocket message *)
type ws_message = {
  type_ : string; [@key "type"]  (** Message type *)
  topic : string option; [@default None] (** Channel topic *)
  subject : string option; [@default None] (** Message subject *)
  data : Yojson.Safe.t option; [@default None] (** Message data *)
} [@@deriving yojson { strict = false }]

(** WebSocket subscription *)
type subscription = {
  id : string;                  (** Request ID *)
  type_ : string; [@key "type"] (** "subscribe" or "unsubscribe" *)
  topic : string;               (** Channel topic *)
  privateChannel : bool;        (** Private channel flag *)
  response : bool;              (** Response required *)
} [@@deriving yojson]
