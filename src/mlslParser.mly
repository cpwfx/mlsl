%token <string> ID CONSTR VARYING
%token <int>    NAT
%token <float>  FLOAT
%token EOF
%token BR_OPN BR_CLS SBR_OPN SBR_CLS CBR_OPN CBR_CLS
%token AMPER ARROW COLON COMMA CONS DIV DOT EQ GE GT HAT JOIN LE LT MINUS MOD 
%token MUL NEQ PIPE PLUS POW SEMI
%token ANY UNIT
%token KW_AND KW_ATTR KW_BEGIN KW_BOOL KW_CONST KW_ELSE KW_END KW_FALSE KW_FIX
%token KW_FLOAT KW_FRAGMENT KW_FUN KW_IF KW_IN KW_INT KW_LET KW_MAT22 KW_MAT23
%token KW_MAT24 KW_MAT32 KW_MAT33 KW_MAT34 KW_MAT42 KW_MAT43 KW_MAT44 KW_MATCH
%token KW_OF KW_REC KW_SAMPLER KW_SAMPLER2D KW_SAMPLERCUBE KW_SHADER KW_THEN 
%token KW_TRUE KW_TYPE KW_UNIT KW_VEC2 KW_VEC3 KW_VEC4 KW_VERTEX KW_WHEN 
%token KW_WITH

%left  PIPE
%right ARROW
%left  STMT
%left  COMMA
%left  EQ GE GT LE LT NEQ
%right CONS
%left  MINUS PLUS
%left  AMPER DIV HAT MOD MUL
%left  JOIN
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
	  KW_BOOL        { MlslAst.TBool                              }
	| KW_FLOAT       { MlslAst.TFloat                             }
	| KW_INT         { MlslAst.TInt                               }
	| KW_MAT22       { MlslAst.TMat(Misc.Dim.Dim2, Misc.Dim.Dim2) }
	| KW_MAT23       { MlslAst.TMat(Misc.Dim.Dim2, Misc.Dim.Dim3) }
	| KW_MAT24       { MlslAst.TMat(Misc.Dim.Dim2, Misc.Dim.Dim4) }
	| KW_MAT32       { MlslAst.TMat(Misc.Dim.Dim3, Misc.Dim.Dim2) }
	| KW_MAT33       { MlslAst.TMat(Misc.Dim.Dim3, Misc.Dim.Dim3) }
	| KW_MAT34       { MlslAst.TMat(Misc.Dim.Dim3, Misc.Dim.Dim4) }
	| KW_MAT42       { MlslAst.TMat(Misc.Dim.Dim4, Misc.Dim.Dim2) }
	| KW_MAT43       { MlslAst.TMat(Misc.Dim.Dim4, Misc.Dim.Dim3) }
	| KW_MAT44       { MlslAst.TMat(Misc.Dim.Dim4, Misc.Dim.Dim4) }
	| KW_SAMPLER2D   { MlslAst.TSampler2D                         }
	| KW_SAMPLERCUBE { MlslAst.TSamplerCube                       }
	| KW_UNIT        { MlslAst.TUnit                              }
	| KW_VEC2        { MlslAst.TVec Misc.Dim.Dim2                 }
	| KW_VEC3        { MlslAst.TVec Misc.Dim.Dim3                 }
	| KW_VEC4        { MlslAst.TVec Misc.Dim.Dim4                 }
;

typ_pattern:
	  typ_pattern ARROW typ_pattern { MlslAst.TPArrow($1, $3) }
	| typ_pattern MUL typ_pattern { MlslAst.TPPair($1, $3) }
	| typ_pattern PIPE typ_pattern { MlslAst.TPOr($1, $3) }
	| typ_pattern_atom { $1 }
;

typ_pattern_atom:
	  BR_OPN typ_pattern BR_CLS { $2 }
	| ANY            { MlslAst.TPAny                               }
	| KW_BOOL        { MlslAst.TPBool                              }
	| KW_FLOAT       { MlslAst.TPFloat                             }
	| KW_INT         { MlslAst.TPInt                               }
	| KW_MAT22       { MlslAst.TPMat(Misc.Dim.Dim2, Misc.Dim.Dim2) }
	| KW_MAT23       { MlslAst.TPMat(Misc.Dim.Dim2, Misc.Dim.Dim3) }
	| KW_MAT24       { MlslAst.TPMat(Misc.Dim.Dim2, Misc.Dim.Dim4) }
	| KW_MAT32       { MlslAst.TPMat(Misc.Dim.Dim3, Misc.Dim.Dim2) }
	| KW_MAT33       { MlslAst.TPMat(Misc.Dim.Dim3, Misc.Dim.Dim3) }
	| KW_MAT34       { MlslAst.TPMat(Misc.Dim.Dim3, Misc.Dim.Dim4) }
	| KW_MAT42       { MlslAst.TPMat(Misc.Dim.Dim4, Misc.Dim.Dim2) }
	| KW_MAT43       { MlslAst.TPMat(Misc.Dim.Dim4, Misc.Dim.Dim3) }
	| KW_MAT44       { MlslAst.TPMat(Misc.Dim.Dim4, Misc.Dim.Dim4) }
	| KW_SAMPLER2D   { MlslAst.TPSampler2D                         }
	| KW_SAMPLERCUBE { MlslAst.TPSamplerCube                       }
	| KW_UNIT        { MlslAst.TPUnit                              }
	| KW_VEC2        { MlslAst.TPVec Misc.Dim.Dim2                 }
	| KW_VEC3        { MlslAst.TPVec Misc.Dim.Dim3                 }
	| KW_VEC4        { MlslAst.TPVec Misc.Dim.Dim4                 }
;

typ_term:
	typ {
			{ MlslAst.tt_pos = Errors.UserPos(Parsing.rhs_start_pos 1)
			; MlslAst.tt_typ = $1
			}
		}
;

expr_id:
	  ID             { $1            }
	| KW_BOOL        { "bool"        }
	| KW_FLOAT       { "float"       }
	| KW_INT         { "int"         }
	| KW_MAT22       { "mat22"       }
	| KW_MAT23       { "mat23"       }
	| KW_MAT24       { "mat24"       }
	| KW_MAT32       { "mat32"       }
	| KW_MAT33       { "mat33"       }
	| KW_MAT34       { "mat34"       }
	| KW_MAT42       { "mat42"       }
	| KW_MAT43       { "mat43"       }
	| KW_MAT44       { "mat44"       }
	| KW_SAMPLER2D   { "sampler2D"   }
	| KW_SAMPLERCUBE { "samplerCube" }
	| KW_UNIT        { "unit"        }
	| KW_VEC2        { "vec2"        }
	| KW_VEC3        { "vec3"        }
	| KW_VEC4        { "vec4"        } 
;

expr:
	  KW_LET let_pattern KW_IN expr %prec STMT {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.ELet(fst $2, snd $2, $4))
		}
	| KW_LET KW_REC let_pattern let_patterns_rev KW_IN expr %prec STMT {
			MlslAst.make_let_rec (Parsing.rhs_start_pos 1) ($3 :: List.rev $4) $6
		}
	| KW_FUN pattern_atom pattern_atom_list_rev ARROW expr %prec STMT {
			let pos = Parsing.rhs_start_pos 1 in
			MlslAst.make_expr pos (MlslAst.EAbs($2, MlslAst.make_abs_rev pos $3 $5))
		}
	| KW_FUN KW_REC expr_id pattern_atom pattern_atom_list_rev EQ expr %prec STMT {
			let pos = Parsing.rhs_start_pos 1 in
			let pat = MlslAst.make_pattern (Parsing.rhs_start_pos 3) (MlslAst.PVar $3) in
			MlslAst.make_expr pos (MlslAst.EFix(pat, 
				MlslAst.make_expr pos (MlslAst.EAbs($4, 
					MlslAst.make_abs_rev pos $5 $7))))
		}
	| KW_FIX pattern KW_IN expr %prec STMT {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EFix($2, $4))
		}
	| KW_IF expr KW_THEN expr KW_ELSE expr %prec STMT {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EIf($2, $4, $6))
		}
	| expr COMMA expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) (MlslAst.EPair($1, $3))
		}
	| expr EQ expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2)
				(MlslAst.EBinOp(MlslAst.BOEq, $1, $3))
		}
	| expr NEQ expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2)
				(MlslAst.EBinOp(MlslAst.BONeq, $1, $3))
		}
	| expr LT expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2)
				(MlslAst.EBinOp(MlslAst.BOLt, $1, $3))
		}
	| expr LE expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2)
				(MlslAst.EBinOp(MlslAst.BOLe, $1, $3))
		}
	| expr GT expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2)
				(MlslAst.EBinOp(MlslAst.BOGt, $1, $3))
		}
	| expr GE expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2)
				(MlslAst.EBinOp(MlslAst.BOGe, $1, $3))
		}
	| expr CONS expr {
			let pos = Parsing.rhs_start_pos 2 in
			MlslAst.make_expr pos (MlslAst.EConstrP("::",
				MlslAst.make_expr pos (MlslAst.EPair($1, $3))))
		}
	| expr MINUS expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOSub, $1, $3))
		}
	| expr PLUS expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOAdd, $1, $3))
		}
	| expr AMPER expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BODot, $1, $3))
		}
	| expr DIV expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BODiv, $1, $3))
		}
	| expr HAT expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOCross, $1, $3))
		}
	| expr MOD expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOMod, $1, $3))
		}
	| expr MUL expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOMul, $1, $3))
		}
	| expr JOIN expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2)
				(MlslAst.EBinOp(MlslAst.BOJoin, $1, $3))
		}
	| MINUS expr %prec UMINUS {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) 
				(MlslAst.EUnOp(MlslAst.UONeg, $2))
		}
	| PLUS expr %prec UPLUS {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) 
				(MlslAst.EUnOp(MlslAst.UOPlus, $2))
		}
	| expr POW expr {
			MlslAst.make_expr (Parsing.rhs_start_pos 2) 
				(MlslAst.EBinOp(MlslAst.BOPow, $1, $3))
		}
	| KW_FRAGMENT expr_call_atom {
			MlslAst.make_expr (Parsing.rhs_start_pos 1)	(MlslAst.EFragment $2)
		}
	| KW_VERTEX expr_call_atom {
			MlslAst.make_expr (Parsing.rhs_start_pos 1)	(MlslAst.EVertex $2)
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
	| FLOAT { MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EFloat $1) }
	| expr_id { MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EVar $1) }
	| VARYING { MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EVarying $1) }
	| CONSTR  { MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EConstrU $1) }
	| UNIT    { MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EConstrU "()") }
	| KW_TRUE  { MlslAst.make_expr (Parsing.rhs_start_pos 1) MlslAst.ETrue  }
	| KW_FALSE { MlslAst.make_expr (Parsing.rhs_start_pos 1) MlslAst.EFalse }
	| KW_MATCH expr KW_WITH pipe_opt match_patterns_rev KW_END {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EMatch($2, List.rev $5))
		}
	| KW_MATCH KW_TYPE KW_OF expr KW_WITH matchto_patterns_rev KW_END {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EMatchType($4, List.rev $6))
		} 
	| BR_OPN expr BR_CLS { $2 }
	| KW_BEGIN expr KW_END { $2 }
	| SBR_OPN SBR_CLS { MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.EConstrU "[]") }
	| SBR_OPN list_expr_rev SBR_CLS { MlslAst.make_list_rev (Parsing.rhs_start_pos 1) $2 }
	| CBR_OPN record_values_rev CBR_CLS {
			MlslAst.make_expr (Parsing.rhs_start_pos 1) (MlslAst.ERecord (List.rev $2))
		}
	| expr_call_atom DOT expr_id {
			MlslAst.make_select (Parsing.rhs_start_pos 2) $1 $3
		}
;

let_pattern:
	  pattern EQ expr { ($1, $3) }
	| expr_id pattern_atom pattern_atom_list_rev EQ expr {
			let pos = Parsing.rhs_start_pos 1 in
			( MlslAst.make_pattern pos (MlslAst.PVar $1)
			, MlslAst.make_expr pos (MlslAst.EAbs($2, MlslAst.make_abs_rev pos $3 $5))
			)
		}
;

let_patterns_rev:
	/* empty */                           { [] }
	| let_patterns_rev KW_AND let_pattern { $3 :: $1 }
;

record_values_rev:
	  record_field_value                        { [ $1 ]   }
	| record_values_rev SEMI record_field_value { $3 :: $1 }
;

record_field_value:
	expr_id EQ expr {
			{ MlslAst.rfv_pos   = Errors.UserPos(Parsing.rhs_start_pos 1)
			; MlslAst.rfv_name  = $1
			; MlslAst.rfv_value = $3
			}
		}
;

list_expr_rev:
	  expr { [ $1 ] }
	| list_expr_rev SEMI expr { $3 :: $1 }
;

pipe_opt:
	/* empty */ { () }
	| PIPE      { () }
;

match_pattern:
	pipe_sep_patterns_rev when_opt ARROW expr {
			{ MlslAst.mp_patterns  = List.rev $1
			; MlslAst.mp_condition = $2
			; MlslAst.mp_action    = $4
			}
		}
;

match_patterns_rev:
	  match_pattern { [ $1 ] }
	| match_patterns_rev PIPE match_pattern { $3 :: $1 }
;

pipe_sep_patterns_rev:
	  untyped_pattern { [ $1 ] }
	| pipe_sep_patterns_rev PIPE untyped_pattern { $3 :: $1 }
;

matchto_pattern:
	typ_pattern_atom ARROW expr {
		{ MlslAst.mtp_pos     = Errors.UserPos(Parsing.rhs_start_pos 1)
		; MlslAst.mtp_pattern = $1
		; MlslAst.mtp_action  = $3
		}
	}
;

matchto_patterns_rev:
	  /* empty */ { [] }
	| matchto_pattern { [ $1 ] }
	| matchto_patterns_rev PIPE matchto_pattern { $3 :: $1 }
;

when_opt:
	  /* empty */  { None }
	| KW_WHEN expr { Some $2 }
;

pattern:
	  expr_id COLON typ_term {
			MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PTypedVar($1, $3))
		}
	| untyped_pattern { $1 }
;
untyped_pattern:
	  untyped_pattern COMMA untyped_pattern {
			MlslAst.make_pattern (Parsing.rhs_start_pos 2) (MlslAst.PPair($1, $3))
		}
	| untyped_pattern CONS untyped_pattern {
			let pos = Parsing.rhs_start_pos 2 in
			MlslAst.make_pattern pos (MlslAst.PConstrP("::",
				MlslAst.make_pattern pos (MlslAst.PPair($1, $3))))
		}
	| CONSTR pattern_atom {
			MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PConstrP($1, $2))
		}
	| pattern_atom { $1 }
; 

pattern_atom:
	  BR_OPN pattern BR_CLS { $2 }
	| ANY { MlslAst.make_pattern (Parsing.rhs_start_pos 1) MlslAst.PAny }
	| expr_id { MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PVar $1) } 
	| UNIT { MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PConstrU "()") }
	| KW_TRUE { MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PTrue) }
	| KW_FALSE { MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PFalse) }
	| SBR_OPN SBR_CLS { MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PConstrU "[]") }
	| SBR_OPN list_pattern_rev SBR_CLS { MlslAst.make_list_pattern_rev (Parsing.rhs_start_pos 1) $2 }
	| CONSTR { MlslAst.make_pattern (Parsing.rhs_start_pos 1) (MlslAst.PConstrU $1) }
;

list_pattern_rev:
	  pattern { [ $1 ] }
	| list_pattern_rev SEMI pattern { $3 :: $1 }
;

pattern_atom_list_rev:
	  /* empty */                        { []       }
	| pattern_atom_list_rev pattern_atom { $2 :: $1 }
;

topdef:
	  KW_ATTR expr_id COLON typ_term {
			{ MlslAst.td_pos  = Errors.UserPos(Parsing.rhs_start_pos 2)
			; MlslAst.td_kind = MlslAst.TDAttrDecl($2, $4)
			}
		}
	| KW_CONST expr_id COLON typ_term { 
			{ MlslAst.td_pos  = Errors.UserPos(Parsing.rhs_start_pos 2)
			; MlslAst.td_kind = MlslAst.TDConstDecl($2, $4)
			}
		}
	| KW_LET let_pattern {
			{ MlslAst.td_pos  = Errors.UserPos(Parsing.rhs_start_pos 2)
			; MlslAst.td_kind = MlslAst.TDLocalDef(fst $2, snd $2)
			}
		}
	| KW_LET KW_REC let_pattern let_patterns_rev {
			MlslAst.make_topdef_let_rec (Parsing.rhs_start_pos 1) ($3 :: List.rev $4)
		}
	| KW_LET KW_FRAGMENT expr_id pattern_atom_list_rev EQ expr {
			let pos = Parsing.rhs_start_pos 2 in
			{ MlslAst.td_pos  = Errors.UserPos(Parsing.rhs_start_pos 3)
			; MlslAst.td_kind = MlslAst.TDLocalDef
				( MlslAst.make_pattern pos (MlslAst.PVar $3)
				, MlslAst.make_abs_rev pos $4 (MlslAst.make_expr pos 
					(MlslAst.EFragment $6))
				)
			}
		}
	| KW_LET KW_VERTEX expr_id pattern_atom_list_rev EQ expr {
			let pos = Parsing.rhs_start_pos 2 in
			{ MlslAst.td_pos  = Errors.UserPos(Parsing.rhs_start_pos 3)
			; MlslAst.td_kind = MlslAst.TDLocalDef
				( MlslAst.make_pattern pos (MlslAst.PVar $3)
				, MlslAst.make_abs_rev pos $4 (MlslAst.make_expr pos 
					(MlslAst.EVertex $6))
				)
			}
		}
	| KW_LET KW_SHADER expr_id EQ expr {
			{ MlslAst.td_pos  = Errors.UserPos(Parsing.rhs_start_pos 3)
			; MlslAst.td_kind = MlslAst.TDShader($3, $5)
			}
		}
	| KW_SAMPLER expr_id COLON typ_term {
			{ MlslAst.td_pos = Errors.UserPos(Parsing.rhs_start_pos 2)
			; MlslAst.td_kind = MlslAst.TDSamplerDecl($2, $4)
			}
		}
;
