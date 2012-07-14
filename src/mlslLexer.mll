
{

let kw_map = Hashtbl.create 32
	
}

let digit     = ['0'-'9']
let var_start = ['a'-'z' 'A'-'Z' '_' '\'']
let var_char  = var_start | digit

rule token = parse
	  [ ' ' '\r' '\t' ] { token lexbuf }
	| '\n' { Lexing.new_line lexbuf; token lexbuf }
	| "//" { line_comment lexbuf }
	| "(*" { block_comment 1 lexbuf }
	| var_start var_char* as x {
			try
				Hashtbl.find kw_map x
			with
			| Not_found -> MlslParser.ID x
		}
	| eof { MlslParser.EOF }

and line_comment = parse
	  '\n' { Lexing.new_line lexbuf; token lexbuf }
	| eof { MlslParser.EOF }
	| _   { line_comment lexbuf }

and block_comment depth = parse
	  "(*" { block_comment (depth+1) lexbuf }
	| "*)" {
			if depth = 1 then token lexbuf
			else block_comment (depth - 1) lexbuf
		}
	| '\n' { Lexing.new_line lexbuf; block_comment depth lexbuf }
	| eof  { MlslParser.EOF }
	| _    { block_comment depth lexbuf }
