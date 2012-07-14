
let _ =
	if Array.length Sys.argv <= 1 then
		print_endline "Usage: mlsl <file>"
	else try
		let chan   = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel chan in
		let _      = MlslParser.main MlslLexer.token lexbuf in
		print_endline "OK"
	with
	| Parsing.Parse_error -> ()
