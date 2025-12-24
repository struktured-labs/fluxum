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
val field_to_flag_name : string -> string

(** {1 String flags} *)

val string_flag : field_name:string -> doc:string -> string Command.Param.t
val string_flag_opt : field_name:string -> default:string -> doc:string -> string Command.Param.t
val string_flag_option : field_name:string -> doc:string -> string option Command.Param.t

(** {1 Int flags} *)

val int_flag : field_name:string -> doc:string -> int Command.Param.t
val int_flag_opt : field_name:string -> default:int -> doc:string -> int Command.Param.t
val int_flag_option : field_name:string -> doc:string -> int option Command.Param.t

(** {1 Int64 flags} *)

val int64_flag : field_name:string -> doc:string -> int64 Command.Param.t
val int64_flag_opt : field_name:string -> default:int64 -> doc:string -> int64 Command.Param.t
val int64_flag_option : field_name:string -> doc:string -> int64 option Command.Param.t

(** {1 Float flags} *)

val float_flag : field_name:string -> doc:string -> float Command.Param.t
val float_flag_opt : field_name:string -> default:float -> doc:string -> float Command.Param.t
val float_flag_option : field_name:string -> doc:string -> float option Command.Param.t

(** {1 Bool flags} *)

val bool_flag : field_name:string -> default:bool -> doc:string -> bool Command.Param.t
val bool_no_arg_flag : field_name:string -> doc:string -> bool Command.Param.t

(** {1 List flags} *)

val string_list_flag : field_name:string -> doc:string -> string list Command.Param.t
val int_list_flag : field_name:string -> doc:string -> int list Command.Param.t

(** {1 Enum flags} *)

(** Create an arg_type for enum types with to_string/of_string_opt

    Example:
      let side_arg_type = enum_arg_type
        ~type_name:"SIDE"
        ~of_string_opt:Common.Side.of_string_opt
        ~all:Common.Side.all
        ~to_string:Common.Side.to_string
*)
val enum_arg_type :
  type_name:string ->
  of_string_opt:(string -> 'a option) ->
  all:'a list ->
  to_string:('a -> string) ->
  'a Command.Arg_type.t

val enum_flag :
  field_name:string ->
  type_name:string ->
  of_string_opt:(string -> 'a option) ->
  all:'a list ->
  to_string:('a -> string) ->
  doc:string ->
  'a Command.Param.t

val enum_flag_opt :
  field_name:string ->
  type_name:string ->
  of_string_opt:(string -> 'a option) ->
  all:'a list ->
  to_string:('a -> string) ->
  default:'a ->
  doc:string ->
  'a Command.Param.t

val enum_flag_option :
  field_name:string ->
  type_name:string ->
  of_string_opt:(string -> 'a option) ->
  all:'a list ->
  to_string:('a -> string) ->
  doc:string ->
  'a option Command.Param.t

val enum_list_flag :
  field_name:string ->
  type_name:string ->
  of_string_opt:(string -> 'a option) ->
  all:'a list ->
  to_string:('a -> string) ->
  doc:string ->
  'a list Command.Param.t
