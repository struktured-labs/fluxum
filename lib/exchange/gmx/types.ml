(** GMX DEX Types *)
open Core
type token = { id : string; symbol : string; decimals : int; } [@@deriving yojson { strict = false }, sexp]
type trade = { id : string; timestamp : string; account : string; sizeDelta : string; collateralDelta : string; isLong : bool; price : string; fee : string; } [@@deriving yojson { strict = false }, sexp]
type position = { id : string; account : string; collateral : string; size : string; averagePrice : string; entryFundingRate : string; realisedPnl : string; } [@@deriving yojson { strict = false }, sexp]

(** GLP Pool - GMX Liquidity Provider pool for spot swaps *)
type glp_pool = {
  id : string;
  token0 : token;
  token1 : token;
  token0_price : float;
  token1_price : float;
  token0_pool_amount : float;
  token1_pool_amount : float;
  swap_fee_bps : int;
} [@@deriving yojson { strict = false }, sexp]
