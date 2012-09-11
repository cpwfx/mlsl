
{

let kw_map = Hashtbl.create 32
let _ =
	Hashtbl.add kw_map "_"           MlslParser.ANY;
	Hashtbl.add kw_map "and"         MlslParser.KW_AND;
	Hashtbl.add kw_map "attr"        MlslParser.KW_ATTR;
	Hashtbl.add kw_map "begin"       MlslParser.KW_BEGIN;
	Hashtbl.add kw_map "bool"        MlslParser.KW_BOOL;
	Hashtbl.add kw_map "const"       MlslParser.KW_CONST;
	Hashtbl.add kw_map "else"        MlslParser.KW_ELSE;
	Hashtbl.add kw_map "end"         MlslParser.KW_END;
	Hashtbl.add kw_map "false"       MlslParser.KW_FALSE;
	Hashtbl.add kw_map "fix"         MlslParser.KW_FIX;
	Hashtbl.add kw_map "float"       MlslParser.KW_FLOAT;
	Hashtbl.add kw_map "fragment"    MlslParser.KW_FRAGMENT;
	Hashtbl.add kw_map "fun"         MlslParser.KW_FUN;
	Hashtbl.add kw_map "if"          MlslParser.KW_IF;
	Hashtbl.add kw_map "in"          MlslParser.KW_IN;
	Hashtbl.add kw_map "int"         MlslParser.KW_INT;
	Hashtbl.add kw_map "let"         MlslParser.KW_LET;
	Hashtbl.add kw_map "mat22"       MlslParser.KW_MAT22;
	Hashtbl.add kw_map "mat23"       MlslParser.KW_MAT23;
	Hashtbl.add kw_map "mat24"       MlslParser.KW_MAT24;
	Hashtbl.add kw_map "mat32"       MlslParser.KW_MAT32;
	Hashtbl.add kw_map "mat33"       MlslParser.KW_MAT33;
	Hashtbl.add kw_map "mat34"       MlslParser.KW_MAT34;
	Hashtbl.add kw_map "mat42"       MlslParser.KW_MAT42;
	Hashtbl.add kw_map "mat43"       MlslParser.KW_MAT43;
	Hashtbl.add kw_map "mat44"       MlslParser.KW_MAT44;
	Hashtbl.add kw_map "match"       MlslParser.KW_MATCH;
	Hashtbl.add kw_map "rec"         MlslParser.KW_REC;
	Hashtbl.add kw_map "sampler"     MlslParser.KW_SAMPLER;
	Hashtbl.add kw_map "sampler2D"   MlslParser.KW_SAMPLER2D;
	Hashtbl.add kw_map "samplerCube" MlslParser.KW_SAMPLERCUBE;
	Hashtbl.add kw_map "shader"      MlslParser.KW_SHADER;
	Hashtbl.add kw_map "then"        MlslParser.KW_THEN;
	Hashtbl.add kw_map "true"        MlslParser.KW_TRUE;
	Hashtbl.add kw_map "unit"        MlslParser.KW_UNIT;
	Hashtbl.add kw_map "vec2"        MlslParser.KW_VEC2;
	Hashtbl.add kw_map "vec3"        MlslParser.KW_VEC3;
	Hashtbl.add kw_map "vec4"        MlslParser.KW_VEC4;
	Hashtbl.add kw_map "vertex"      MlslParser.KW_VERTEX;
	Hashtbl.add kw_map "with"        MlslParser.KW_WITH;
	Hashtbl.add kw_map "when"        MlslParser.KW_WHEN;
	()

let op_map = Hashtbl.create 32
let _ =
	Hashtbl.add op_map "&"  MlslParser.AMPER;
	Hashtbl.add op_map "->" MlslParser.ARROW;
	Hashtbl.add op_map ":"  MlslParser.COLON;
	Hashtbl.add op_map ","  MlslParser.COMMA;
	Hashtbl.add op_map "::" MlslParser.CONS;
	Hashtbl.add op_map "/"  MlslParser.DIV;
	Hashtbl.add op_map "."  MlslParser.DOT;
	Hashtbl.add op_map "="  MlslParser.EQ;
	Hashtbl.add op_map ">=" MlslParser.GE;
	Hashtbl.add op_map ">"  MlslParser.GT;
	Hashtbl.add op_map "^"  MlslParser.HAT;
	Hashtbl.add op_map "<=" MlslParser.LE;
	Hashtbl.add op_map "<"  MlslParser.LT;
	Hashtbl.add op_map "-"  MlslParser.MINUS;
	Hashtbl.add op_map "%"  MlslParser.MOD;
	Hashtbl.add op_map "*"  MlslParser.MUL;
	Hashtbl.add op_map "<>" MlslParser.NEQ;
	Hashtbl.add op_map "|"  MlslParser.PIPE;
	Hashtbl.add op_map "+"  MlslParser.PLUS;
	Hashtbl.add op_map "**" MlslParser.POW;
	Hashtbl.add op_map ";"  MlslParser.SEMI;
	()

let nat_regexp = Str.regexp "^[0-9]+$"

let float_regexp = Str.regexp "^[0-9]+.[0-9]*\\([eE][+-]?[0-9]+\\)?$" 

let tokenize_number str =
	if Str.string_match nat_regexp str 0 then
		MlslParser.NAT (int_of_string str)
	else if Str.string_match float_regexp str 0 then
		MlslParser.FLOAT (float_of_string str)
	else
		raise (ParserMisc.Invalid_number str)
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
	| "()" { MlslParser.UNIT    }
	| '('  { MlslParser.BR_OPN  }
	| ')'  { MlslParser.BR_CLS  }
	| '['  { MlslParser.SBR_OPN }
	| ']'  { MlslParser.SBR_CLS }
	| '{'  { MlslParser.CBR_OPN }
	| '}'  { MlslParser.CBR_CLS }
	| var_start var_char* as x {
			try
				Hashtbl.find kw_map x
			with
			| Not_found ->
				if Misc.Char.is_upper x.[0] then 
					MlslParser.CONSTR x
				else 
					MlslParser.ID x
		}
	| digit (var_char | digit | '.')* as x {
			tokenize_number x
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
			Errors.warning_p (Errors.UserPos lexbuf.Lexing.lex_curr_p) 
				"End of file inside block comment ('*)' expected).";
			MlslParser.EOF 
		}
	| _    { block_comment depth lexbuf }
