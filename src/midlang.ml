(* File: midlang.ml *)

module StrMap = Map.Make(String)

let instr_fresh   = Misc.Fresh.create ()
let var_fresh     = Misc.Fresh.create ()
let sampler_fresh = Misc.Fresh.create ()

type dim =
| Dim2
| Dim3
| Dim4

let int_of_dim d =
	match d with
	| Dim2 -> 2
	| Dim3 -> 3
	| Dim4 -> 4

let range_of_dim d =
	match d with
	| Dim2 -> [ 0; 1 ]
	| Dim3 -> [ 0; 1; 2 ]
	| Dim4 -> [ 0; 1; 2; 3 ]

let dim_of_ast d =
	match d with
	| MlslAst.Dim2 -> Dim2
	| MlslAst.Dim3 -> Dim3
	| MlslAst.Dim4 -> Dim4

type typ =
| TFloat
| TInt
| TMat of dim * dim
| TVec of dim

type variable_sort =
| VSAttribute
| VSConstant
| VSTemporary
| VSVarying

type variable =
	{ var_id   : int
	; var_typ  : typ
	; var_sort : variable_sort
	}

module Variable = struct
	type t = variable
	let compare x y = compare x.var_id y.var_id
end

type sampler_dim =
| SDim2D
| SDimCube

type sampler =
	{ sampler_id   : int
	; sampler_name : string
	; sampler_dim  : sampler_dim
	}

type semantics =
| SInput0
| SInput1
| SInput2
| SInput3
| SInput4
| SInput5
| SInput6
| SInput7
| SPosition
| STexcoord0
| STexcoord1
| STexcoord2
| STexcoord3

type attr =
	{ attr_semantics : semantics
	; attr_name      : string
	; attr_var       : variable
	}

type param =
	{ param_name : string
	; param_var  : variable
	}

type binop =
| BOAddF
| BOAddM  of dim * dim
| BOAddV  of dim
| BOSubF
| BOSubM  of dim * dim
| BOSubV  of dim
| BOMulFF
| BOMulMF of dim * dim
| BOMulMM of dim * dim * dim
| BOMulMV of dim * dim
| BOMulVF of dim
| BOMulVV of dim
| BODivFF
| BODivFV of dim
| BODivMF of dim * dim
| BODivVF of dim
| BODivVV of dim
| BOModFF
| BOModFV of dim
| BOModMF of dim * dim
| BOModVF of dim
| BOModVV of dim
| BODot   of dim
| BOCross2
| BOCross3
| BOPowFF
| BOPowVF of dim
| BOPowVV of dim

type unop =
| UONegF
| UONegM of dim * dim
| UONegV of dim

type instr_kind =
| IMov     of variable * variable
| IBinOp   of variable * variable * variable * binop
| IUnOp    of variable * variable * unop
| ISwizzle of variable * variable * MlslAst.Swizzle.t
| ITex     of variable * variable * sampler
| IRet     of variable
type instr =
	{ ins_id   : int
	; ins_kind : instr_kind
	}

let create_instr kind =
	{ ins_id   = Misc.Fresh.next instr_fresh
	; ins_kind = kind
	}

type shader =
	{ sh_name     : string
	; sh_attr     : attr list
	; sh_v_const  : param list
	; sh_f_const  : param list
	; sh_varying  : param list
	; sh_samplers : sampler list
	; sh_vertex   : instr list
	; sh_fragment : instr list
	}

let string_of_typ tp =
	match tp with
	| TFloat       -> "float"
	| TInt         -> "int"
	| TMat(d1, d2) -> Printf.sprintf "mat%d%d" (int_of_dim d1) (int_of_dim d2)
	| TVec d       -> Printf.sprintf "vec%d" (int_of_dim d)

let vectyp_of_int n =
	match n with
	| 1 -> TFloat
	| 2 -> TVec Dim2
	| 3 -> TVec Dim3
	| 4 -> TVec Dim4
	| _ -> raise Misc.InternalError

(* ========================================================================= *)

let create_var_ast sort ast_typ =
	{ var_id = Misc.Fresh.next var_fresh
	; var_typ =
		begin match ast_typ with
		| MlslAst.TFloat -> TFloat
		| MlslAst.TInt   -> TInt
		| MlslAst.TMat(d1, d2) -> TMat(dim_of_ast d1, dim_of_ast d2)
		| MlslAst.TVec d       -> TVec(dim_of_ast d)
		| MlslAst.TBool | MlslAst.TSampler2D | MlslAst.TSamplerCube 
		| MlslAst.TUnit | MlslAst.TArrow _ | MlslAst.TPair _
		| MlslAst.TRecord _ | MlslAst.TVertex _ | MlslAst.TFragment _
		| MlslAst.TVertexTop -> raise Misc.InternalError
		end
	; var_sort = sort
	}

let create_variable sort typ =
	{ var_id   = Misc.Fresh.next var_fresh
	; var_typ  = typ
	; var_sort = sort
	}

let create_sampler_ast name typ =
	{ sampler_id   = Misc.Fresh.next sampler_fresh
	; sampler_name = name
	; sampler_dim  =
		begin match typ with
		| MlslAst.TSampler2D   -> SDim2D
		| MlslAst.TSamplerCube -> SDimCube
		| _ -> raise Misc.InternalError
		end
	}

(* ========================================================================= *)

type value =
	{ v_pos  : Lexing.position
	; v_kind : value_kind
	}
and value_kind =
| VPair     of value * value
| VVertex   of value StrMap.t * MlslAst.expr
| VFragment of value StrMap.t * MlslAst.expr

exception Eval_exception

let credits = ref 1024
let globals = Hashtbl.create 32

let rec eval_expr gamma expr =
	match expr.MlslAst.e_kind with
	| MlslAst.EVar x ->
		begin try
			StrMap.find x gamma
		with
		| Not_found ->
			eval_global gamma expr.MlslAst.e_pos x
		end
	| MlslAst.EVarying x ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval_expr EVarying.";
		raise Eval_exception
	| MlslAst.EInt n ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval_expr EInt.";
		raise Eval_exception
	| MlslAst.ERecord rd ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval_expr ERecord.";
		raise Eval_exception
	| MlslAst.EPair(e1, e2) ->
		let v1 = eval_expr gamma e1 in
		let v2 = eval_expr gamma e2 in
			{ v_pos = expr.MlslAst.e_pos; v_kind = VPair(v1, v2) }
	| _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval_expr.";
		raise Eval_exception
and eval_global gamma pos x =
	if Hashtbl.mem globals x then
		Hashtbl.find globals x
	else match TopDef.check_name x with
	| None ->
		Errors.fatal_error "Internal error!!!";
		raise Eval_exception
	| Some (_, td) ->
		let result = 
			match td.MlslAst.td_kind with
			| MlslAst.TDAttrDecl _ ->
				Errors.error "Unimplemented: eval_global TDAttrDecl.";
				raise Eval_exception
			| MlslAst.TDConstDecl _ ->
				Errors.error "Unimplemented: eval_global TDConstDecl.";
				raise Eval_exception
			| MlslAst.TDFragmentShader(_, body) ->
				{ v_pos = pos; v_kind = VFragment(gamma, body) }
			| MlslAst.TDVertexShader(_, body) ->
				{ v_pos = pos; v_kind = VVertex(gamma, body) }
			| _ ->
				Errors.error "Unimplemented: eval_global.";
				raise Eval_exception
		in
			Hashtbl.add globals x result;
			result

(* ========================================================================= *)

let attr_map    = Hashtbl.create 32
let v_const_map = Hashtbl.create 32
let f_const_map = Hashtbl.create 32
let varying_map = Hashtbl.create 32
let sampler_map = Hashtbl.create 32

let create_attr_list () =
	Misc.ListExt.map_filter (fun (name, semantics, _) ->
		if Hashtbl.mem attr_map name then
			Some
				{ attr_name      = name
				; attr_semantics =
					begin match semantics.MlslAst.asem_name with
					| "INPUT0"    -> SInput0
					| "INPUT1"    -> SInput1
					| "INPUT2"    -> SInput2
					| "INPUT3"    -> SInput3
					| "INPUT4"    -> SInput4
					| "INPUT5"    -> SInput5
					| "INPUT6"    -> SInput6
					| "INPUT7"    -> SInput7
					| "POSITION"  -> SPosition
					| "TEXCOORD0" -> STexcoord0
					| "TEXCOORD1" -> STexcoord1
					| "TEXCOORD2" -> STexcoord2
					| "TEXCOORD3" -> STexcoord3
					| sem -> 
						Errors.error (Printf.sprintf 
							"Internal error: unknown semantics %s." sem);
						SPosition
					end
				; attr_var       = Hashtbl.find attr_map name
				}
		else None) (TopDef.attr_list ())

let create_v_const_list () =
	Misc.ListExt.map_filter (fun (name, _) ->
		if Hashtbl.mem v_const_map name then
			Some
				{ param_name = name
				; param_var  = Hashtbl.find v_const_map name
				}
		else None) (TopDef.const_list ())

let create_f_const_list () =
	Misc.ListExt.map_filter (fun (name, _) ->
		if Hashtbl.mem f_const_map name then
			Some
				{ param_name = name
				; param_var  = Hashtbl.find f_const_map name
				}
		else None) (TopDef.const_list ())

let create_varying_list () =
	Hashtbl.fold (fun name var l ->
		{ param_name = name
		; param_var  = var
		} :: l) varying_map []

let create_sampler_list () =
	Misc.ListExt.map_filter (fun (name, _) ->
		try
			Some (Hashtbl.find sampler_map name)
		with
		| Not_found -> None) (TopDef.sampler_list ())

(* ========================================================================= *)

type reg_value =
	{ rv_pos  : Lexing.position
	; rv_kind : reg_value_kind
	}
and reg_value_kind =
| RVReg     of variable
| RVRecord  of reg_value StrMap.t
| RVSampler of sampler
| RVValue   of value

exception Unfold_exception

let rec regval_of_value code value =
	match value.v_kind with
	| VPair _ ->
		Errors.error_p value.v_pos "Unimplemented: regval_of_value VPair.";
		{ rv_pos = value.v_pos; rv_kind = RVValue value }
	| VFragment _ | VVertex _ ->
		{ rv_pos = value.v_pos; rv_kind = RVValue value }

let string_of_rvkind kind =
	match kind with
	| RVReg _ -> "data type value"
	| RVRecord _ -> "record"
	| RVSampler _ -> "sampler"
	| RVValue _ -> "high level value"

let cast_regval_to_type pos code rv tp =
	Errors.error_p pos "Unimpleneted: cast_regval_to_type.";
	raise Unfold_exception

let rec bind_pattern code gamma pat rv =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny   -> gamma
	| MlslAst.PVar x -> StrMap.add x rv gamma
	| MlslAst.PTypedVar(x, tp) ->
		let rv' = cast_regval_to_type pat.MlslAst.p_pos code rv tp.MlslAst.tt_typ in
			StrMap.add x rv' gamma

let typ_and_ins_of_add pos tp1 tp2 =
	match tp1, tp2 with
	| TFloat, TFloat -> 
		(TFloat, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOAddF))
	| TMat(d1, d2), TMat(d1', d2') when d1 = d1' && d2 = d2' ->
		(TMat(d1, d2), fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOAddM(d1, d2)))
	| TVec d, TVec d' when d = d' ->
		(TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOAddV d))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Addition for types %s * %s is not defined."
				(string_of_typ tp1) (string_of_typ tp2));
		raise Unfold_exception

let typ_and_ins_of_sub pos tp1 tp2 =
	match tp1, tp2 with
	| TFloat, TFloat -> 
		(TFloat, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOSubF))
	| TMat(d1, d2), TMat(d1', d2') when d1 = d1' && d2 = d2' ->
		(TMat(d1, d2), fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOSubM(d1, d2)))
	| TVec d, TVec d' when d = d' ->
		(TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOSubV d))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Subtraction for types %s * %s is not defined."
				(string_of_typ tp1) (string_of_typ tp2));
		raise Unfold_exception

let typ_and_ins_of_mul pos tp1 tp2 =
	match tp1, tp2 with
	| TFloat, TFloat -> (TFloat, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOMulFF))
	| TFloat, TMat(d1, d2) -> 
		(TMat(d1, d2), fun r1 r2 r3 -> IBinOp(r1, r3, r2, BOMulMF(d1, d2)))
	| TFloat, TVec d ->
		(TVec d, fun r1 r2 r3 -> IBinOp(r1, r3, r2, BOMulVF d))
	| TMat(d1, d2), TFloat ->
		(TMat(d1, d2), fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOMulMF(d1, d2)))
	| TMat(d1, d2), TMat(d3, d4) when d2 = d3 ->
		(TMat(d1, d4), fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOMulMM(d1, d2, d4)))
	| TMat(d1, d2), TVec d when d2 = d -> 
		(TVec d1,  fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOMulMV(d1, d2)))
	| TVec d, TFloat  -> (TVec d,  fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOMulVF d))
	| TVec d1, TVec d2 when d1 = d2 ->
		(TVec d1, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOMulVV d1))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Multiplication for types %s * %s is not defined."
				(string_of_typ tp1) (string_of_typ tp2));
		raise Unfold_exception

let typ_and_ins_of_div pos tp1 tp2 =
	match tp1, tp2 with
	| TFloat, TFloat -> (TFloat, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BODivFF))
	| TFloat, TVec d -> (TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BODivFV d))
	| TMat(d1, d2), TFloat -> 
		(TMat(d1, d2), fun r1 r2 r3 -> IBinOp(r1, r2, r3, BODivMF(d1, d2)))
	| TVec d, TFloat -> (TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BODivVF d))
	| TVec d, TVec d' when d = d' ->
		(TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BODivVV d))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Division for types %s * %s is not defined."
				(string_of_typ tp1) (string_of_typ tp2));
		raise Unfold_exception

let typ_and_ins_of_mod pos tp1 tp2 =
	match tp1, tp2 with
	| TFloat, TFloat -> (TFloat, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOModFF))
	| TFloat, TVec d -> (TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOModFV d))
	| TMat(d1, d2), TFloat ->
		(TMat(d1, d2), fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOModMF(d1, d2)))
	| TVec d, TFloat -> (TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOModVF d))
	| TVec d, TVec d' when d = d' ->
		(TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOModVV d))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Modulo for types %s * %s is not defined."
				(string_of_typ tp1) (string_of_typ tp2));
		raise Unfold_exception

let typ_and_ins_of_dot pos tp1 tp2 =
	match tp1, tp2 with
	| TFloat, TFloat -> (TFloat, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOMulFF))
	| TVec d, TVec d' when d = d' -> 
		(TFloat, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BODot d))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Dot product for types %s * %s is not defined."
				(string_of_typ tp1) (string_of_typ tp2));
		raise Unfold_exception

let typ_and_ins_of_cross pos tp1 tp2 =
	match tp1, tp2 with
	| TVec Dim2, TVec Dim2 -> (TFloat, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOCross2))
	| TVec Dim3, TVec Dim3 -> (TVec Dim3, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOCross3))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Cross product for types %s * %s is not defined."
				(string_of_typ tp1) (string_of_typ tp2));
		raise Unfold_exception

let typ_and_ins_of_pow pos tp1 tp2 =
	match tp1, tp2 with
	| TFloat, TFloat -> (TFloat, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOPowFF))
	| TVec d, TFloat -> (TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOPowVF d))
	| TVec d, TVec d' when d = d' ->
		(TVec d, fun r1 r2 r3 -> IBinOp(r1, r2, r3, BOPowVV d))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Power for types %s * %s is not defined."
				(string_of_typ tp1) (string_of_typ tp2));
		raise Unfold_exception

let typ_and_ins_of_neg pos tp =
	match tp with
	| TFloat -> (TFloat, fun r1 r2 -> IUnOp(r1, r2, UONegF))
	| TMat(d1, d2) -> (TMat(d1, d2), fun r1 r2 -> IUnOp(r1, r2, UONegM(d1, d2)))
	| TVec d -> (TVec d, fun r1 r2 -> IUnOp(r1, r2, UONegV d))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Unary minus for type %s is not defined."
				(string_of_typ tp));
		raise Unfold_exception

let typ_and_ins_of_uplus pos tp =
	match tp with
	| TFloat | TMat _ | TVec _ ->
		(tp, fun r1 r2 -> IMov(r1, r2))
	| _ ->
		Errors.error_p pos
			(Printf.sprintf "Unary plus for type %s is not defined."
				(string_of_typ tp));
		raise Unfold_exception

let unfold_code_var vertex code gamma expr x =
	if StrMap.mem x gamma then begin 
		try
			StrMap.find x gamma
		with
		| Not_found ->
			Errors.error_p expr.MlslAst.e_pos 
				(Printf.sprintf "Unbound variable %s" x);
			raise Unfold_exception
	end else begin match TopDef.check_name x with
	| None ->
		Errors.error_p expr.MlslAst.e_pos "Internal error!!!";
		raise Misc.InternalError
	| Some(_, td) ->
		begin match td.MlslAst.td_kind with
		| MlslAst.TDAttrDecl(name, _, typ) ->
			if vertex then
				{ rv_pos  = td.MlslAst.td_pos
				; rv_kind = RVReg (
					try
						Hashtbl.find attr_map name
					with
					| Not_found ->
						let v = create_var_ast VSAttribute typ.MlslAst.tt_typ in
						Hashtbl.add attr_map name v;
						v
					)
				}
			else begin
				Errors.error_p expr.MlslAst.e_pos "Attributes are not available for fragment shaders.";
				raise Unfold_exception
			end
		| MlslAst.TDConstDecl(name, typ) ->
			let const_map = if vertex then v_const_map else f_const_map in
			{ rv_pos  = td.MlslAst.td_pos
			; rv_kind = RVReg (
				try
					Hashtbl.find const_map name
				with
				| Not_found ->
					let v = create_var_ast VSConstant typ.MlslAst.tt_typ in
					Hashtbl.add const_map name v;
					v
				)
			}
		| MlslAst.TDSamplerDecl(name, typ) ->
			{ rv_pos = td.MlslAst.td_pos
			; rv_kind = RVSampler (
				try
					Hashtbl.find sampler_map name
				with
				| Not_found ->
					let v = create_sampler_ast name typ.MlslAst.tt_typ in
					Hashtbl.add sampler_map name v;
					v
				)
			}
		| _ ->
			Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code_var EVar global.";
			raise Unfold_exception
		end
	end

let rec unfold_code vertex code gamma expr =
	match expr.MlslAst.e_kind with
	| MlslAst.EVar x ->
		unfold_code_var vertex code gamma expr x
	| MlslAst.EVarying x ->
		if vertex then begin
			Errors.error_p expr.MlslAst.e_pos "Varying variables are not allowed in vertex shaders.";
			raise Unfold_exception
		end else begin try
			let vr = Hashtbl.find varying_map x in
				{ rv_pos  = expr.MlslAst.e_pos
				; rv_kind = RVReg vr
				}
		with
		| Not_found ->
			Errors.error_p expr.MlslAst.e_pos (Printf.sprintf 
				"Undefinded varying variable $%s." x);
			raise Unfold_exception
		end
	| MlslAst.EInt n ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code EInt.";
		raise Unfold_exception
	| MlslAst.ESwizzle(e, swizzle) ->
		let rv = unfold_code vertex code gamma e in
		begin match rv.rv_kind with
		| RVReg rvreg ->
			begin match rvreg.var_typ with
			| TFloat ->
				if MlslAst.Swizzle.max_component_id swizzle = 0 then
					let rreg = create_variable VSTemporary 
						(vectyp_of_int (MlslAst.Swizzle.size swizzle)) in
					Misc.ImpList.add code 
						(create_instr (ISwizzle(rreg, rvreg, swizzle)));
					{ rv_pos = expr.MlslAst.e_pos; rv_kind = RVReg rreg }
				else begin
					Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
						"Value defined at %s has type float, can not be swizzled using pattern %s."
							(Errors.string_of_pos rv.rv_pos)
							(MlslAst.Swizzle.to_string swizzle)
						);
					raise Unfold_exception
				end
			| TVec d ->
				if MlslAst.Swizzle.max_component_id swizzle < int_of_dim d then
					let rreg = create_variable VSTemporary 
						(vectyp_of_int (MlslAst.Swizzle.size swizzle)) in
					Misc.ImpList.add code 
						(create_instr (ISwizzle(rreg, rvreg, swizzle)));
					{ rv_pos = expr.MlslAst.e_pos; rv_kind = RVReg rreg }
				else begin
					Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
						"Value defined at %s has type %s, can not be swizzled using pattern %s."
							(Errors.string_of_pos rv.rv_pos)
							(string_of_typ rvreg.var_typ)
							(MlslAst.Swizzle.to_string swizzle)
						);
					raise Unfold_exception
				end
			| tp ->
				Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
					"Value defined at %s has type %s, can not be swizzled."
						(Errors.string_of_pos rv.rv_pos)
						(string_of_typ tp)
					);
				raise Unfold_exception
			end
		| kind ->
			Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
				"Value defined at %s is a %s, can not be swizzled."
					(Errors.string_of_pos rv.rv_pos)
					(string_of_rvkind kind)
				);
			raise Unfold_exception
		end
	| MlslAst.ERecord rd ->
		let rd' =
			List.fold_left (fun regMap field ->
				let rv = unfold_code vertex code gamma field.MlslAst.rfv_value in
					StrMap.add field.MlslAst.rfv_name rv regMap
				) StrMap.empty rd
		in
			{ rv_pos = expr.MlslAst.e_pos; rv_kind = RVRecord rd' }
	| MlslAst.ESelect(e, field) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code ESelect.";
		raise Unfold_exception
	| MlslAst.EPair(e1, e2) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: unfold_code EPair.";
		raise Unfold_exception
	| MlslAst.EBinOp(op, e1, e2) ->
		let rv1 = unfold_code vertex code gamma e1 in
		let rv2 = unfold_code vertex code gamma e2 in
		begin match rv1.rv_kind, rv2.rv_kind with
		| RVReg r1, RVReg r2 ->
			let (rtp, ins) = 
				match op with
				| MlslAst.BOAdd ->
					typ_and_ins_of_add expr.MlslAst.e_pos r1.var_typ r2.var_typ
				| MlslAst.BOSub ->
					typ_and_ins_of_sub expr.MlslAst.e_pos r1.var_typ r2.var_typ
				| MlslAst.BOMul -> 
					typ_and_ins_of_mul expr.MlslAst.e_pos r1.var_typ r2.var_typ
				| MlslAst.BODiv ->
					typ_and_ins_of_div expr.MlslAst.e_pos r1.var_typ r2.var_typ
				| MlslAst.BOMod ->
					typ_and_ins_of_mod expr.MlslAst.e_pos r1.var_typ r2.var_typ
				| MlslAst.BODot ->
					typ_and_ins_of_dot expr.MlslAst.e_pos r1.var_typ r2.var_typ
				| MlslAst.BOCross ->
					typ_and_ins_of_cross expr.MlslAst.e_pos r1.var_typ r2.var_typ
				| MlslAst.BOPow ->
					typ_and_ins_of_pow expr.MlslAst.e_pos r1.var_typ r2.var_typ
			in
			let rreg = create_variable VSTemporary rtp in
			Misc.ImpList.add code (create_instr (ins rreg r1 r2));
			{ rv_pos = expr.MlslAst.e_pos; rv_kind = RVReg rreg }
		| RVReg _, _ ->
			Errors.error_p expr.MlslAst.e_pos 
				(Printf.sprintf "Second operand of %s defined at %s can not be a %s."
					(MlslAst.binop_name op)
					(Errors.string_of_pos rv2.rv_pos)
					(string_of_rvkind rv2.rv_kind));
			raise Unfold_exception
		| _ ->
			Errors.error_p expr.MlslAst.e_pos 
				(Printf.sprintf "First operand of %s defined at %s can not be a %s."
					(MlslAst.binop_name op)
					(Errors.string_of_pos rv1.rv_pos)
					(string_of_rvkind rv1.rv_kind));
			raise Unfold_exception
		end
	| MlslAst.EUnOp(op, e) ->
		let rv = unfold_code vertex code gamma e in
		begin match rv.rv_kind with
		| RVReg r ->
			let (rtp, ins) =
				match op with
				| MlslAst.UONeg ->
					typ_and_ins_of_neg expr.MlslAst.e_pos r.var_typ
				| MlslAst.UOPlus ->
					typ_and_ins_of_uplus expr.MlslAst.e_pos r.var_typ
			in
			let rreg = create_variable VSTemporary rtp in
			Misc.ImpList.add code (create_instr (ins rreg r));
			{ rv_pos = expr.MlslAst.e_pos; rv_kind = RVReg rreg }
		| _ ->
			Errors.error_p expr.MlslAst.e_pos 
				(Printf.sprintf "First operand of %s defined at %s can not be a %s."
					(MlslAst.unop_name op)
					(Errors.string_of_pos rv.rv_pos)
					(string_of_rvkind rv.rv_kind));
			raise Unfold_exception
		end
	| MlslAst.EApp(e1, e2) ->
		let func  = unfold_code vertex code gamma e1 in
		let rvarg = unfold_code vertex code gamma e2 in
		begin match func.rv_kind with
		| RVSampler sampler ->
			begin match rvarg.rv_kind with
			| RVReg coordreg ->
				begin match coordreg.var_typ with
				| TVec Dim2 ->
					let rreg = create_variable VSTemporary (TVec Dim4) in
					Misc.ImpList.add code 
						(create_instr (ITex(rreg, coordreg, sampler)));
					{ rv_pos = expr.MlslAst.e_pos; rv_kind = RVReg rreg }
				| tp ->
					Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
						"Texture coordinates defined at %s have type %s, but expected vec2."
							(Errors.string_of_pos rvarg.rv_pos)
							(string_of_typ tp));
					raise Unfold_exception
				end
			| _ ->
				Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
					"Value defined at %s is a %s, can not be used as texture coordinates."
						(Errors.string_of_pos rvarg.rv_pos)
						(string_of_rvkind rvarg.rv_kind));
					raise Unfold_exception
			end
		| _ -> 
			Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
				"Value defined at %s is not a function/sampler, can not be applied."
					(Errors.string_of_pos func.rv_pos));
				raise Unfold_exception
		end
	| MlslAst.ELet(pat, e1, e2) ->
		let rv1   = unfold_code vertex code gamma e1 in
		let gamma = bind_pattern code gamma pat rv1  in
			unfold_code vertex code gamma e2

let unfold_vertex gamma expr code =
	let reg_val = unfold_code true code gamma expr in
	match reg_val.rv_kind with
	| RVRecord rd ->
		if not (StrMap.mem "position" rd) then begin
			Errors.error_p expr.MlslAst.e_pos 
				(Printf.sprintf "This record defined at %s has not \"position\" field."
					(Errors.string_of_pos reg_val.rv_pos));
			raise Unfold_exception
		end else begin
			let ok = StrMap.fold (fun v_name v_rv st ->
				if v_name = "position" then st
				else match v_rv.rv_kind with
				| RVReg vr ->
					let vv = create_variable VSVarying vr.var_typ in
					Misc.ImpList.add code (create_instr (IMov(vv, vr)));
					Hashtbl.add varying_map v_name vv; 
					st
				| _ ->
					Errors.error_p expr.MlslAst.e_pos 
						(Printf.sprintf "Field %s defined at %s is not a primitive value."
							v_name (Errors.string_of_pos v_rv.rv_pos));
					false
				) rd true in
			match (StrMap.find "position" rd).rv_kind with
			| RVReg rr ->
				begin match rr.var_typ with
				| TVec Dim4 ->
					Misc.ImpList.add code (create_instr (IRet rr));
					if ok then code
					else raise Unfold_exception
				| tp ->
					Errors.error_p expr.MlslAst.e_pos (Printf.sprintf 
						"Result position of vertex shader defined at %s has type %s, but expected vec4."
						(Errors.string_of_pos (StrMap.find "position" rd).rv_pos)
						(string_of_typ tp)
					); raise Unfold_exception
				end
			| _ ->
				Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
					"Result position of vertex shader defined at %s is not a primitive value."
						(Errors.string_of_pos (StrMap.find "position" rd).rv_pos)
				); raise Unfold_exception
		end
	| _ ->
		Errors.error_p expr.MlslAst.e_pos "Non record result of vertex shader.";
		raise Unfold_exception

let unfold_fragment gamma expr code =
	let reg_val = unfold_code false code gamma expr in
	match reg_val.rv_kind with
	| RVReg col ->
		begin match col.var_typ with
		| TVec Dim4 ->
			Misc.ImpList.add code (create_instr (IRet col));
			code
		| tp ->
			Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
				"Result position of fragment shader defined at %s has type %s, but expected vec4."
				(Errors.string_of_pos reg_val.rv_pos)
				(string_of_typ tp)
			); raise Unfold_exception
		end
	| _ ->
		Errors.error_p expr.MlslAst.e_pos (Printf.sprintf
			"Result of fragment shader defined at %s is not a primitive value."
			(Errors.string_of_pos reg_val.rv_pos)
		); raise Unfold_exception

let unfold_shader name expr =
	credits := 1024;
	try
		let value = eval_expr StrMap.empty expr in
		match value.v_kind with
		| VPair(vs, fs) ->
			begin match vs.v_kind, fs.v_kind with
			| VVertex(vs_gamma, vs_expr), VFragment(fs_gamma, fs_expr) ->
				Hashtbl.clear attr_map;
				Hashtbl.clear v_const_map;
				Hashtbl.clear f_const_map;
				Hashtbl.clear varying_map;
				let vs_code   = Misc.ImpList.create () in
				let fs_code   = Misc.ImpList.create () in
				let vs_gamma' = StrMap.map (regval_of_value vs_code) vs_gamma in
				let fs_gamma' = StrMap.map (regval_of_value fs_code) fs_gamma in
				let vertex    = unfold_vertex vs_gamma' vs_expr vs_code in
				let fragment  = unfold_fragment fs_gamma' fs_expr fs_code in
					Some
						{ sh_name     = name
						; sh_attr     = create_attr_list ()
						; sh_v_const  = create_v_const_list ()
						; sh_f_const  = create_f_const_list ()
						; sh_varying  = create_varying_list ()
						; sh_samplers = create_sampler_list ()
						; sh_vertex   = Misc.ImpList.to_list vertex
						; sh_fragment = Misc.ImpList.to_list fragment
						}
			| VVertex _, _ ->
				Errors.error_p fs.v_pos "This expression is not a fragment shader.";
				None
			| _, _ ->
				Errors.error_p vs.v_pos "This expression is not a vertex shader.";
				None
			end
		| _ ->
			Errors.error_p value.v_pos "Shader must be a pair of vertex and fragment shaders.";
			None
	with
	| Eval_exception   -> None
	| Unfold_exception -> None

(* TODO: better optimizer *)
let optimize s = s
