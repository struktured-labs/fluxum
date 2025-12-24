open Core

(** Helper module for building CLI arguments from record fields.

    This module provides utilities to convert record fields into Command.Param.t
    values, enabling natural command-line flags instead of sexp input.

    Example:
      Instead of: --request '((pair "XETHZUSD") (volume 0.01))'
      Use:        --pair XETHZUSD --volume 0.01
*)

(** Convert a field name to a flag name.
    - Replaces underscores with hyphens
    - Removes trailing underscore (for OCaml reserved words like type_)

    Examples:
      "pair" -> "pair"
      "type_" -> "type"
      "time_in_force" -> "time-in-force"
*)
let field_to_flag_name (field_name : string) : string =
  let name =
    match String.is_suffix field_name ~suffix:"_" with
    | true -> String.drop_suffix field_name 1
    | false -> field_name
  in
  String.substr_replace_all name ~pattern:"_" ~with_:"-"

(** Required string flag *)
let string_flag ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.required Command.Param.string) ~doc

(** Optional string flag with default *)
let string_flag_opt ~field_name ~default ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name)
    (Command.Param.optional_with_default default Command.Param.string) ~doc

(** Optional string flag (None if not provided) *)
let string_flag_option ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.optional Command.Param.string) ~doc

(** Required int flag *)
let int_flag ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.required Command.Param.int) ~doc

(** Optional int flag with default *)
let int_flag_opt ~field_name ~default ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name)
    (Command.Param.optional_with_default default Command.Param.int) ~doc

(** Optional int flag (None if not provided) *)
let int_flag_option ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.optional Command.Param.int) ~doc

(** Required int64 flag *)
let int64_flag ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  let int64_arg_type =
    Command.Arg_type.create (fun s ->
      match Int64.of_string_opt s with
      | Some v -> v
      | None -> failwith (sprintf "Invalid int64: %s" s))
  in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.required int64_arg_type) ~doc

(** Optional int64 flag with default *)
let int64_flag_opt ~field_name ~default ~doc =
  let flag_name = field_to_flag_name field_name in
  let int64_arg_type =
    Command.Arg_type.create (fun s ->
      match Int64.of_string_opt s with
      | Some v -> v
      | None -> failwith (sprintf "Invalid int64: %s" s))
  in
  Command.Param.flag ("-" ^ flag_name)
    (Command.Param.optional_with_default default int64_arg_type) ~doc

(** Optional int64 flag (None if not provided) *)
let int64_flag_option ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  let int64_arg_type =
    Command.Arg_type.create (fun s ->
      match Int64.of_string_opt s with
      | Some v -> v
      | None -> failwith (sprintf "Invalid int64: %s" s))
  in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.optional int64_arg_type) ~doc

(** Required float flag *)
let float_flag ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.required Command.Param.float) ~doc

(** Optional float flag with default *)
let float_flag_opt ~field_name ~default ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name)
    (Command.Param.optional_with_default default Command.Param.float) ~doc

(** Optional float flag (None if not provided) *)
let float_flag_option ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.optional Command.Param.float) ~doc

(** Boolean flag (defaults to false if not provided) *)
let bool_flag ~field_name ~default ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name)
    (Command.Param.optional_with_default default Command.Param.bool) ~doc

(** Boolean no-arg flag (presence = true, absence = false) *)
let bool_no_arg_flag ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name) Command.Param.no_arg ~doc

(** String list flag (can be specified multiple times) *)
let string_list_flag ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name)
    (Command.Param.listed Command.Param.string) ~doc

(** Int list flag (can be specified multiple times) *)
let int_list_flag ~field_name ~doc =
  let flag_name = field_to_flag_name field_name in
  Command.Param.flag ("-" ^ flag_name)
    (Command.Param.listed Command.Param.int) ~doc

(** Create an arg_type for enum types with to_string/of_string_opt

    Example:
      let side_arg_type = enum_arg_type
        ~type_name:"SIDE"
        ~of_string_opt:Common.Side.of_string_opt
        ~all:Common.Side.all
        ~to_string:Common.Side.to_string
*)
let enum_arg_type
    ~type_name
    ~(of_string_opt : string -> 'a option)
    ~(all : 'a list)
    ~(to_string : 'a -> string)
  : 'a Command.Arg_type.t =
  Command.Arg_type.create
    ~complete:(fun _ ~part:_ -> List.map all ~f:to_string)
    (fun s ->
      match of_string_opt s with
      | Some v -> v
      | None ->
        failwith (sprintf "Invalid %s: %s (valid values: %s)"
          type_name s
          (String.concat ~sep:", " (List.map all ~f:to_string))))

(** Required enum flag *)
let enum_flag ~field_name ~type_name ~of_string_opt ~all ~to_string ~doc =
  let flag_name = field_to_flag_name field_name in
  let arg_type = enum_arg_type ~type_name ~of_string_opt ~all ~to_string in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.required arg_type) ~doc

(** Optional enum flag with default *)
let enum_flag_opt ~field_name ~type_name ~of_string_opt ~all ~to_string ~default ~doc =
  let flag_name = field_to_flag_name field_name in
  let arg_type = enum_arg_type ~type_name ~of_string_opt ~all ~to_string in
  Command.Param.flag ("-" ^ flag_name)
    (Command.Param.optional_with_default default arg_type) ~doc

(** Optional enum flag (None if not provided) *)
let enum_flag_option ~field_name ~type_name ~of_string_opt ~all ~to_string ~doc =
  let flag_name = field_to_flag_name field_name in
  let arg_type = enum_arg_type ~type_name ~of_string_opt ~all ~to_string in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.optional arg_type) ~doc

(** Enum list flag (can be specified multiple times) *)
let enum_list_flag ~field_name ~type_name ~of_string_opt ~all ~to_string ~doc =
  let flag_name = field_to_flag_name field_name in
  let arg_type = enum_arg_type ~type_name ~of_string_opt ~all ~to_string in
  Command.Param.flag ("-" ^ flag_name) (Command.Param.listed arg_type) ~doc
