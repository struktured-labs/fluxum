(** GMX DEX Types *)
open Core
type token = { id : string; symbol : string; decimals : int; } [@@deriving yojson { strict = false }, sexp]
type trade = { id : string; timestamp : string; account : string; sizeDelta : string; collateralDelta : string; isLong : bool; price : string; fee : string; } [@@deriving yojson { strict = false }, sexp]
type position = { id : string; account : string; collateral : string; size : string; averagePrice : string; entryFundingRate : string; realisedPnl : string; } [@@deriving yojson { strict = false }, sexp]
