
let _ =
	if Array.length Sys.argv <= 1 then
		print_endline "Usage: mlsl <file>"
	else try
		match Parser.parse Sys.argv.(1) with
		| Some td_list -> print_endline "OK"
		| None -> ()
	with
	| Parsing.Parse_error -> ()
