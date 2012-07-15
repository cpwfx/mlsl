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
		let pos = lexbuf.Lexing.lex_start_p in
		Printf.printf "%s:\nSyntax error (Unexpected token \"%s\")\n"
			(ParserMisc.string_of_pos pos)
			(Lexing.lexeme lexbuf);
		None
	| ParserMisc.Invalid_character c ->
		let pos = lexbuf.Lexing.lex_start_p in
		Printf.printf "%s:\nInvalid character '%s' (0x%X).\n"
			(ParserMisc.string_of_pos pos)
			(Char.escaped c)
			(Char.code c);
		None
	| ParserMisc.Unknown_operator op ->
		let pos = lexbuf.Lexing.lex_start_p in
		Printf.printf "%s:\nUnrecognized operator \"%s\".\n"
			(ParserMisc.string_of_pos pos)
			op;
		None
