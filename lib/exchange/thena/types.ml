(** Thena DEX Types *)
open Core
type token = { id : string; symbol : string; decimals : string; } [@@deriving yojson { strict = false }, sexp]
type pair = { id : string; token0 : token; token1 : token; reserve0 : string; reserve1 : string; totalSupply : string; } [@@deriving yojson { strict = false }, sexp]
type swap = { id : string; timestamp : string; amount0In : string; amount1In : string; amount0Out : string; amount1Out : string; to_ : string; [@key "to"] } [@@deriving yojson { strict = false }, sexp]
