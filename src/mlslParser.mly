%token <string> ID VARYING
%token <int>    NAT
%token <string> FLOAT
%token EOF
%token BR_OPN BR_CLS CBR_OPN CBR_CLS
%token AMPER ARROW COLON COMMA DIV DOT EQ HAT MINUS MOD MUL PLUS POW SEMI
%token ANY
%token KW_ATTR KW_BOOL KW_CONST KW_FLOAT KW_FRAGMENT KW_FUN KW_IN KW_INT 
%token KW_LET KW_MAT22 KW_MAT23 KW_MAT24 KW_MAT32 KW_MAT33 KW_MAT34 KW_MAT42 
%token KW_MAT43 KW_MAT44 KW_SAMPLER KW_SAMPLER2D KW_SAMPLERCUBE KW_SHADER 
%token KW_UNIT KW_VEC2 KW_VEC3 KW_VEC4 KW_VERTEX

%right ARROW
%left  COMMA
%left  MINUS PLUS
%left  AMPER DIV HAT MOD MUL
%left  UMINUS UPLUS
%right POW

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
	  KW_BOOL        { MlslAst.TBool                            }
	| KW_FLOAT       { MlslAst.TFloat                           }
	| KW_INT         { MlslAst.TInt                             }
	| KW_MAT22       { MlslAst.TMat(MlslAst.Dim2, MlslAst.Dim2) }
	| KW_MAT23       { MlslAst.TMat(MlslAst.Dim2, MlslAst.Dim3) }
	| KW_MAT24       { MlslAst.TMat(MlslAst.Dim2, MlslAst.Dim4) }
	| KW_MAT32       { MlslAst.TMat(MlslAst.Dim3, MlslAst.Dim2) }
	| KW_MAT33       { MlslAst.TMat(MlslAst.Dim3, MlslAst.Dim3) }
	| KW_MAT34       { MlslAst.TMat(MlslAst.Dim3, MlslAst.Dim4) }
	| KW_MAT42       { MlslAst.TMat(MlslAst.Dim4, MlslAst.Dim2) }
	| KW_MAT43       { MlslAst.TMat(MlslAst.Dim4, MlslAst.Dim3) }
	| KW_MAT44       { MlslAst.TMat(MlslAst.Dim4, MlslAst.Dim4) }
	| KW_SAMPLER2D   { MlslAst.TSampler2D                       }
	| KW_SAMPLERCUBE { MlslAst.TSamplerCube                     }
	| KW_UNIT        { MlslAst.TUnit                            }
	| KW_VEC2        { MlslAst.TVec MlslAst.Dim2                }
	| KW_VEC3        { MlslAst.TVec MlslAst.Dim3                }
	| KW_VEC4        { MlslAst.TVec MlslAst.Dim4                }
;

typ_term:
	typ {
			{ MlslAst.tt_pos = Parsing.rhs_start_pos 1
			; MlslAst.tt_typ = $1
			}
		}
;

expr:
	  KW_LET pattern EQ expr KW_IN expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.ELet($2, $4, $6))
		}
	| KW_LET ID pattern_atom pattern_atom_list_rev EQ expr KW_IN expr {
			let pos1 = Parsing.rhs_start_pos 1 in
			let pos2 = Parsing.rhs_start_pos 2 in
			MlslAst.make_expr pos1 (MlslAst.ELet
				( MlslAst.make_pattern pos2 (MlslAst.PVar $2)
				, MlslAst.make_expr pos2 (MlslAst.EAbs($3, MlslAst.make_abs_rev pos2 $4 $6))
				, $8
				))
		}
	| KW_FUN pattern_atom pattern_atom_list_rev ARROW expr {
			let pos = Parsing.rhs_start_pos 1 in
			MlslAst.make_expr pos (MlslAst.EAbs($2, MlslAst.make_abs_rev pos $3 $5))
		}
	| expr_nosemi { $1 }
;

expr_nosemi:
	  expr_nosemi COMMA expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) (MlslAst.EPair($1, $3))
		}
	| expr_nosemi MINUS expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOSub, $1, $3))
		}
	| expr_nosemi PLUS expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOAdd, $1, $3))
		}
	| expr_nosemi AMPER expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BODot, $1, $3))
		}
	| expr_nosemi DIV expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BODiv, $1, $3))
		}
	| expr_nosemi HAT expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOCross, $1, $3))
		}
	| expr_nosemi MOD expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOMod, $1, $3))
		}
	| expr_nosemi MUL expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOMul, $1, $3))
		}
	| MINUS expr_nosemi %prec UMINUS {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) 
				(MlslAst.EUnOp(MlslAst.UONeg, $2))
		}
	| PLUS expr_nosemi %prec UPLUS {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) 
				(MlslAst.EUnOp(MlslAst.UOPlus, $2))
		}
	| expr_nosemi POW expr_nosemi {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOPow, $1, $3))
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
	| expr_call_atom DOT ID {
			MlslAst.make_select (Parsing.rhs_start_pos 2) $1 $3
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

pattern:
	  ID COLON typ_term {
			MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PTypedVar($1, $3))
		}
	| pattern_atom { $1 }
; 

pattern_atom:
	  BR_OPN pattern BR_CLS { $2 }
	| ANY { MlslAst.make_pattern (Parsing.rhs_start_pos 1) MlslAst.PAny }
	| ID  { MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PVar $1) }
;

pattern_atom_list_rev:
	  /* empty */                        { []       }
	| pattern_atom_list_rev pattern_atom { $2 :: $1 }
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
