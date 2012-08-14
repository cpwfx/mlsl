(* File: builtin.ml *)

let (-->) t1 t2 =
	MlslAst.TArrow(t1, t2)

let mt_float = MlslAst.TFloat
let mt_vec2  = MlslAst.TVec MlslAst.Dim2
let mt_vec3  = MlslAst.TVec MlslAst.Dim3
let mt_vec4  = MlslAst.TVec MlslAst.Dim4

type simple_expr =
| Var   of string
| Fun   of string * simple_expr
| BinOp of MlslAst.binop * simple_expr * simple_expr

let rec make_expr code =
	{ MlslAst.e_pos  = Errors.BuiltinPos
	; MlslAst.e_kind =
		begin match code with
		| Var x -> MlslAst.EVar x
		| Fun(x, code) -> MlslAst.EAbs(
			{ MlslAst.p_pos  = Errors.BuiltinPos
			; MlslAst.p_kind = MlslAst.PVar x
			}, make_expr code)
		| BinOp(op, c1, c2) -> MlslAst.EBinOp(op, make_expr c1, make_expr c2)
		end
	}

let builtins =
	[ "min", 
		[ mt_float --> (mt_float --> mt_float)
		; mt_vec2  --> (mt_vec2  --> mt_vec2 )
		; mt_vec3  --> (mt_vec3  --> mt_vec3 )
		; mt_vec4  --> (mt_vec4  --> mt_vec4 )
		], make_expr (Fun("x", Fun("y", BinOp(MlslAst.BOMin, Var "x", Var "y"))))
	]

let init () =
	List.iter (fun (name, types, expr) ->
			TypeCheck.declare_builtin name types;
			TopDef.add name (Eval.eval TopDef.empty_context expr)
		) builtins
