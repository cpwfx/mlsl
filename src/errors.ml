(* File: errors.ml *)

let error_counter   = ref 0
let warning_counter = ref 0

let string_of_pos pos =
	Printf.sprintf "file %s, line %d, column %d"
		pos.Lexing.pos_fname
		pos.Lexing.pos_lnum
		(pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let fatal_error_p pos msg =
	error_counter := !error_counter + 1;
	Printf.printf ("%s:\nFatal error: " ^^ msg ^^ "\n\n") (string_of_pos pos)

let error_p pos msg =
	error_counter := !error_counter + 1;
	Printf.printf ("%s:\nError: " ^^ msg ^^ "\n\n") (string_of_pos pos)

let warning_p pos msg =
	warning_counter := !warning_counter + 1;
	Printf.printf ("%s:\nWarning: " ^^ msg ^^ "\n\n") (string_of_pos pos)

let fatal_error msg =
	error_counter := !error_counter + 1;
	Printf.printf ("Fatal error: " ^^ msg ^^ "\n\n")

let error msg =
	error_counter := !error_counter + 1;
	Printf.printf ("Error: " ^^ msg ^^ "\n\n")

let ok () = !error_counter = 0
let print_status () =
	Printf.printf "%s! %d errors, %d warnings.\n"
		(if ok () then "Success" else "Compilation failed")
		!error_counter
		!warning_counter
