
{

let kw_map = Hashtbl.create 32
let _ =
	Hashtbl.add kw_map "attr"     MlslParser.KW_ATTR;
	Hashtbl.add kw_map "bool"     MlslParser.KW_BOOL;
	Hashtbl.add kw_map "const"    MlslParser.KW_CONST;
	Hashtbl.add kw_map "float"    MlslParser.KW_FLOAT;
	Hashtbl.add kw_map "fragment" MlslParser.KW_FRAGMENT;
	Hashtbl.add kw_map "int"      MlslParser.KW_INT;
	Hashtbl.add kw_map "let"      MlslParser.KW_LET;
	Hashtbl.add kw_map "mat44"    MlslParser.KW_MAT44;
	Hashtbl.add kw_map "shader"   MlslParser.KW_SHADER;
	Hashtbl.add kw_map "unit"     MlslParser.KW_UNIT;
	Hashtbl.add kw_map "vec2"     MlslParser.KW_VEC2;
	Hashtbl.add kw_map "vec3"     MlslParser.KW_VEC3;
	Hashtbl.add kw_map "vec4"     MlslParser.KW_VEC4;
	Hashtbl.add kw_map "vertex"   MlslParser.KW_VERTEX;
	()

let op_map = Hashtbl.create 32
let _ =
	Hashtbl.add op_map "->" MlslParser.ARROW;
	Hashtbl.add op_map ":"  MlslParser.COLON;
	Hashtbl.add op_map ","  MlslParser.COMMA;
	Hashtbl.add op_map "="  MlslParser.EQ;
	Hashtbl.add op_map "*"  MlslParser.MUL;
	Hashtbl.add op_map ";"  MlslParser.SEMI;
	()
	
}

let digit     = ['0'-'9']
let var_start = ['a'-'z' 'A'-'Z' '_' '\'']
let var_char  = var_start | digit
let op_char   = ['~' '!' '@' '#' '%' '^' '&' '*' '-' '+' '=' '|' ':' ';' '<' ',' '>' '.' '?' '/']

rule token = parse
	  [ ' ' '\r' '\t' ] { token lexbuf }
	| '\n' { Lexing.new_line lexbuf; token lexbuf }
	| "//" { line_comment lexbuf }
	| "(*" { block_comment 1 lexbuf }
	| '('  { MlslParser.BR_OPN  }
	| ')'  { MlslParser.BR_CLS  }
	| '{'  { MlslParser.CBR_OPN }
	| '}'  { MlslParser.CBR_CLS }
	| var_start var_char* as x {
			try
				Hashtbl.find kw_map x
			with
			| Not_found -> MlslParser.ID x
		}
	| '$' (var_start var_char* as x) { MlslParser.VARYING x }
	| op_char+ as x {
			try
				Hashtbl.find op_map x
			with
			| Not_found -> raise (ParserMisc.Unknown_operator x)
		}
	| eof { MlslParser.EOF }
	| _ as x { raise (ParserMisc.Invalid_character x) }

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
	| eof  {
			Errors.warning_p (lexbuf.Lexing.lex_curr_p) 
				"End of file inside block comment ('*)' expected).";
			MlslParser.EOF 
		}
	| _    { block_comment depth lexbuf }
