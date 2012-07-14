%token <string> ID
%token <int>    NAT
%token <string> FLOAT
%token EOF
%token BR_OPN BR_CLS
%token ARROW COLON
%token KW_CONST KW_FLOAT KW_INT KW_VEC2 KW_VEC3 KW_VEC4

%right ARROW

%start main
%type <MlslAst.topdecl list> main

%{

let parse_error err =
	print_endline "Syntax error."

%}

%%

main:
	topdecl_list_rev { List.rev $1 }
;

topdecl_list_rev:
	EOF                        { []       }
	| topdecl_list_rev topdecl { $2 :: $1 }
;

typ:
	typ_atom            { $1 }
	| BR_OPN typ BR_CLS { $2 }
	| typ ARROW typ     { MlslAst.TArrow($1, $3) }
;

typ_atom:
	KW_FLOAT  { MlslAst.TFloat }
	| KW_INT  { MlslAst.TInt   }
	| KW_VEC2 { MlslAst.TVec2  }
	| KW_VEC3 { MlslAst.TVec3  }
	| KW_VEC4 { MlslAst.TVec4  }
;

typ_term:
	typ {
			{ MlslAst.tt_pos = Parsing.rhs_start_pos 1
			; MlslAst.tt_typ = $1
			}
		}
;

topdecl:
	KW_CONST ID COLON typ_term { 
			{ MlslAst.td_pos  = Parsing.rhs_start_pos 2
			; MlslAst.td_type = MlslAst.TDConstDecl($2, $4)
			}
		}
;
