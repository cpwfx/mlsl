(* File: parser.ml *)

let parse fname =
	let chan   = open_in fname in
	let lexbuf = Lexing.from_channel chan in
	lexbuf.Lexing.lex_curr_p <-
		{ lexbuf.Lexing.lex_curr_p with
		  Lexing.pos_fname = fname
		};
	try
		Some (MlslParser.main MlslLexer.token lexbuf)
	with
	| Parsing.Parse_error ->
		Errors.fatal_error_p (lexbuf.Lexing.lex_start_p)
			(Printf.sprintf "Syntax error (Unexpected token \"%s\")." 
				(Lexing.lexeme lexbuf)
			);
		None
	| ParserMisc.Invalid_character c ->
		Errors.fatal_error_p (lexbuf.Lexing.lex_start_p)
			(Printf.sprintf "Invalid character '%s' (0x%X)."
				(Char.escaped c)
				(Char.code c)
			);
		None
	| ParserMisc.Unknown_operator op ->
		Errors.fatal_error_p (lexbuf.Lexing.lex_start_p)
			(Printf.sprintf "Unrecognized operator \"%s\"." op);
		None
