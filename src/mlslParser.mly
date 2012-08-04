%token <string> ID VARYING
%token <int>    NAT
%token <string> FLOAT
%token EOF
%token BR_OPN BR_CLS CBR_OPN CBR_CLS
%token ARROW COLON COMMA EQ MUL SEMI
%token KW_ATTR KW_BOOL KW_CONST KW_FLOAT KW_FRAGMENT KW_INT KW_LET KW_MAT44 
%token KW_SAMPLER KW_SAMPLER2D KW_SAMPLERCUBE KW_SHADER KW_UNIT KW_VEC2 KW_VEC3 
%token KW_VEC4 KW_VERTEX

%right ARROW
%left  COMMA
%left  MUL

%start main
%type <MlslAst.topdef list> main

%%

main:
	topdef_list_rev EOF { List.rev $1 }
;

topdef_list_rev:
	  /* empty */            { []       }
	| topdef_list_rev topdef { $2 :: $1 }
;

typ:
	  typ_atom          { $1 }
	| BR_OPN typ BR_CLS { $2 }
	| typ ARROW typ     { MlslAst.TArrow($1, $3) }
;

typ_atom:
	  KW_BOOL        { MlslAst.TBool        }
	| KW_FLOAT       { MlslAst.TFloat       }
	| KW_INT         { MlslAst.TInt         }
	| KW_MAT44       { MlslAst.TMat44       }
	| KW_SAMPLER2D   { MlslAst.TSampler2D   }
	| KW_SAMPLERCUBE { MlslAst.TSamplerCube }
	| KW_UNIT        { MlslAst.TUnit        }
	| KW_VEC2        { MlslAst.TVec2        }
	| KW_VEC3        { MlslAst.TVec3        }
	| KW_VEC4        { MlslAst.TVec4        }
;

typ_term:
	typ {
			{ MlslAst.tt_pos = Parsing.rhs_start_pos 1
			; MlslAst.tt_typ = $1
			}
		}
;

expr:
	expr_nosemi { $1 }
;

expr_nosemi:
	  expr_nosemi COMMA expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) (MlslAst.EPair($1, $3))
		}
	| expr_nosemi MUL expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) (MlslAst.EMul($1, $3))
		}
	| expr_call_atom expr_call_atom_list_rev {
			MlslAst.make_app $1 (List.rev $2)
		}
;

expr_call_atom_list_rev:
	  /* empty */                            { [] }
	| expr_call_atom_list_rev expr_call_atom { $2 :: $1 }
;

expr_call_atom:
	  NAT { MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EInt $1) }
	| ID  {	MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EVar $1) }
	| VARYING { MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EVarying $1) }
	| BR_OPN expr BR_CLS { $2 }
	| CBR_OPN record_values_rev CBR_CLS {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.ERecord (List.rev $2))
		}
;

record_values_rev:
	  record_field_value                        { [ $1 ]   }
	| record_values_rev SEMI record_field_value { $3 :: $1 }
;

record_field_value:
	ID EQ expr_nosemi {
			{ MlslAst.rfv_pos   = Parsing.rhs_start_pos 1
			; MlslAst.rfv_name  = $1
			; MlslAst.rfv_value = $3
			}
		}
;

topdef:
	  KW_ATTR ID ID COLON typ_term {
			let asem =
				{ MlslAst.asem_name = $2
				; MlslAst.asem_pos  = Parsing.rhs_start_pos 2
				} in
			{ MlslAst.td_pos  = Parsing.rhs_start_pos 3
			; MlslAst.td_kind = MlslAst.TDAttrDecl($3, asem, $5)
			}
		}
	| KW_CONST ID COLON typ_term { 
			{ MlslAst.td_pos  = Parsing.rhs_start_pos 2
			; MlslAst.td_kind = MlslAst.TDConstDecl($2, $4)
			}
		}
	| KW_LET KW_FRAGMENT ID EQ expr {
			{ MlslAst.td_pos  = Parsing.rhs_start_pos 3
			; MlslAst.td_kind = MlslAst.TDFragmentShader($3, $5)
			}
		}
	| KW_LET KW_VERTEX ID EQ expr {
			{ MlslAst.td_pos  = Parsing.rhs_start_pos 3
			; MlslAst.td_kind = MlslAst.TDVertexShader($3, $5)
			}
		}
	| KW_LET KW_SHADER ID EQ expr {
			{ MlslAst.td_pos  = Parsing.rhs_start_pos 3
			; MlslAst.td_kind = MlslAst.TDShader($3, $5)
			}
		}
	| KW_SAMPLER ID COLON typ_term {
			{ MlslAst.td_pos = Parsing.rhs_start_pos 2
			; MlslAst.td_kind = MlslAst.TDSamplerDecl($2, $4)
			}
		}
;
