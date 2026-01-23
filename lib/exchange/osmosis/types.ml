(** Osmosis DEX Types *)
open Core
type pool = { id : string; pool_assets : pool_asset list; total_shares : string; } [@@deriving yojson { strict = false }, sexp]
and pool_asset = { token : token_info; weight : string; } [@@deriving yojson { strict = false }, sexp]
and token_info = { denom : string; amount : string; } [@@deriving yojson { strict = false }, sexp]
type swap_msg = { pool_id : string; token_in_denom : string; token_out_denom : string; token_in_amount : string; token_out_min_amount : string; } [@@deriving yojson { strict = false }, sexp]
