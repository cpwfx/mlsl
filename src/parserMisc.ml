(* File: parserMisc.ml *)

exception Invalid_character of char
exception Unknown_operator  of string

let string_of_pos pos =
	Printf.sprintf "File %s, line %d, column %d"
		pos.Lexing.pos_fname
		pos.Lexing.pos_lnum
		(pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
