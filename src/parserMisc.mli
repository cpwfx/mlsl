(* File: parserMisc.mli *)

exception Invalid_character of char
exception Unknown_operator  of string

val string_of_pos : Lexing.position -> string
