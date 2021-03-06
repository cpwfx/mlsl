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
		Errors.fatal_error_p (Errors.UserPos lexbuf.Lexing.lex_start_p)
			"Syntax error (Unexpected token \"%s\")." 
			(Lexing.lexeme lexbuf);
		None
	| ParserMisc.Invalid_character c ->
		Errors.fatal_error_p (Errors.UserPos lexbuf.Lexing.lex_start_p)
			"Invalid character '%s' (0x%X)."
			(Char.escaped c) (Char.code c);
		None
	| ParserMisc.Invalid_number num ->
		Errors.fatal_error_p (Errors.UserPos lexbuf.Lexing.lex_start_p)
			"Invalid token \"%s\"." num;
		None
	| ParserMisc.Unknown_operator op ->
		Errors.fatal_error_p (Errors.UserPos lexbuf.Lexing.lex_start_p)
			"Unrecognized operator \"%s\"." op;
		None
