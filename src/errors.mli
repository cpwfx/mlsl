(* File: errors.mli *)

type position =
| BuiltinPos
| UserPos    of Lexing.position

val string_of_pos : position -> string

val fatal_error_p : position -> ('a, out_channel, unit) format -> 'a
val error_p       : position -> ('a, out_channel, unit) format -> 'a
val warning_p     : position -> ('a, out_channel, unit) format -> 'a

val fatal_error : ('a, out_channel, unit) format -> 'a
val error       : ('a, out_channel, unit) format -> 'a

val ok : unit -> bool
val print_status : unit -> unit
