(** Curve Finance DEX Types *)

open Core

type coin = {
  address : string;
  symbol : string;
  decimals : int;
} [@@deriving yojson { strict = false }, sexp]

type pool_data = {
  id : string;
  address : string;
  name : string;
  symbol : string;
  assetType : int;
  coins : coin list;
  coinAddresses : string list;
  decimals : string list;
  totalSupply : string;
  lpTokenAddress : string;
  virtualPrice : string;
  usdTotal : float;
  amplificationCoefficient : string option;
} [@@deriving yojson { strict = false }, sexp]

type pool_list_response = {
  success : bool;
  data : pool_data list;
} [@@deriving yojson { strict = false }, sexp]

type pool_detail_response = {
  success : bool;
  data : pool_data;
} [@@deriving yojson { strict = false }, sexp]

type volume_data = {
  date : string;
  volume : float;
} [@@deriving yojson { strict = false }, sexp]

type volume_response = {
  success : bool;
  data : volume_data list;
} [@@deriving yojson { strict = false }, sexp]

type trade = {
  tx : string;
  timestamp : string;
  tokenIn : string;
  tokenOut : string;
  amountIn : string;
  amountOut : string;
  trader : string;
} [@@deriving yojson { strict = false }, sexp]

type trades_response = {
  success : bool;
  data : trade list;
} [@@deriving yojson { strict = false }, sexp]
