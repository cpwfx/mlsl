(* File: errors.mli *)

val string_of_pos : Lexing.position -> string

val fatal_error_p : Lexing.position -> string -> unit
val error_p       : Lexing.position -> string -> unit
val warning_p     : Lexing.position -> string -> unit

val error : string -> unit

val ok : unit -> bool
val print_status : unit -> unit
