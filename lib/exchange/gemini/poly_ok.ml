type 'a ok = [ `Ok of 'a ] [@@deriving sexp]

let debug_sexp : 'a -> Sexp.t = fun x ->
  let rec aux depth obj_x =
    match depth > 10 with
    | true -> Sexp.Atom "..."
    | false ->
      match Obj.is_int obj_x with
      | true -> Sexp.Atom (string_of_int (Obj.magic obj_x : int))
      | false ->
        match Obj.tag obj_x = Obj.string_tag with
        | true -> Sexp.Atom (Printf.sprintf "\"%s\"" (Obj.magic obj_x : string))
        | false ->
          match Obj.tag obj_x = Obj.double_tag with
          | true -> Sexp.Atom (string_of_float (Obj.magic obj_x : float))
          | false ->
            match Obj.tag obj_x = Obj.double_array_tag with
            | true ->
              let arr = (Obj.magic obj_x : float array) in
              Sexp.List (List.init (Array.length arr) ~f:(fun i -> Sexp.Atom (string_of_float arr.(i))))
            | false ->
              match Obj.tag obj_x < Obj.no_scan_tag with
              | true ->
                let tag = Obj.tag obj_x in
                let size = Obj.size obj_x in
                (match Int.equal size 0 with
                 | true -> Sexp.Atom (Printf.sprintf "Tag%d" tag)
                 | false ->
                   Sexp.List [
                     Sexp.Atom (Printf.sprintf "Tag%d" tag);
                     Sexp.List (List.init size ~f:(fun i -> aux (depth + 1) (Obj.field obj_x i)))
                   ])
              | false -> Sexp.Atom "opaque"
  in
  aux 0 (Obj.repr x)

let ok_exn ?(message = "not ok") ?here ?sexp_of_error (x : [> 'a ok ]) =
  match x with
  | `Ok x -> x
  | error -> (
    let sexp_of_error = match sexp_of_error with
    | Some f -> f
    | None -> debug_sexp in
      failwiths message ~here:(Option.value here ~default:[%here]) error sexp_of_error
  )
let ok_exn' t = ok_exn t

let ok_or_none (x : [> 'a ok ]) =
  match x with
  | `Ok x -> Some x
  | _ -> None

let ok_or_error_s (x : [> 'a ok ]) sexp_of_error =
  match x with
  | `Ok x -> Or_error.return x
  | e -> Or_error.error_s (sexp_of_error e)

let ok_or_error (x : [> 'a ok ]) string_of_error =
  match x with
  | `Ok x -> Or_error.return x
  | e -> Or_error.error_string @@ string_of_error e

let ok (x : 'a) : 'a ok = `Ok x
