(** OKX Order Book - Placeholder

    TODO: Implement Order_book_intf.S for OKX
*)

open Core
open Async

module Book = struct
  type t = unit Deferred.t

  let create ~symbol =
    ignore symbol;
    return ()

  let pipe _t =
    Pipe.create_reader ~close_on_exception:false (fun _writer ->
      Deferred.never ())
end
