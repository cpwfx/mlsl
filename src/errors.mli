(* File: errors.mli *)

val string_of_pos : Lexing.position -> string

val fatal_error_p : Lexing.position -> ('a, out_channel, unit) format -> 'a
val error_p       : Lexing.position -> ('a, out_channel, unit) format -> 'a
val warning_p     : Lexing.position -> ('a, out_channel, unit) format -> 'a

val fatal_error : ('a, out_channel, unit) format -> 'a
val error       : ('a, out_channel, unit) format -> 'a

val ok : unit -> bool
val print_status : unit -> unit
