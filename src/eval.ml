(* File: eval.ml *)

open Misc.Dim

module StrMap = Map.Make(String)

exception Eval_exception

let credits = ref 1024

let target_func = ref (fun _ -> ())

let set_target_func f =
	target_func := f

let value_kind pos value =
	EvalPrim.with_exn Eval_exception (fun () -> EvalPrim.value_kind pos value)

let rec cast_value_to_type pos value typ =
	match typ with
	| MlslAst.TBool ->
		begin match value_kind pos value with
		| TopDef.VBool _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected bool."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TFloat ->
		begin match value_kind pos value with
		| TopDef.VInt n -> TopDef.make_value value.TopDef.v_pos (TopDef.VFloat (float_of_int n))
		| TopDef.VFloat _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected float."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TInt ->
		begin match value_kind pos value with
		| TopDef.VInt n -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected int."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TMat(d1, d2) ->
		begin match value_kind pos value with
		| TopDef.VMat(d1', d2', _) when d1 = d1' && d2 = d2' -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected mat%d%d."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind)
				(int_of_dim d1) (int_of_dim d2);
			raise Eval_exception
		end
	| MlslAst.TSampler2D ->
		begin match value_kind pos value with
		| TopDef.VSampler(_, tt) when tt.MlslAst.tt_typ = MlslAst.TSampler2D -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected sampler2D."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TSamplerCube -> 
		begin match value_kind pos value with
		| TopDef.VSampler(_, tt) when tt.MlslAst.tt_typ = MlslAst.TSamplerCube -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected samplerCube."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TUnit ->
		begin match value_kind pos value with
		| TopDef.VConstrU "()" -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected unit."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TVec d ->
		begin match value_kind pos value with
		| TopDef.VVec(d', _) when d = d' -> value
		| kind -> 
			Errors.error_p pos "Value defined at %s is a %s, but expected vec%d."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind)
				(int_of_dim d);
			raise Eval_exception
		end
	| MlslAst.TArrow _ ->
		begin match value_kind pos value with
		| TopDef.VFunc _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected function."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TPair(t1, t2) ->
		begin match value_kind pos value with
		| TopDef.VPair(v1, v2) ->
			TopDef.make_value value.TopDef.v_pos (TopDef.VPair
				(cast_value_to_type pos v1 t1, cast_value_to_type pos v2 t2) )
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected pair."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TRecord rd ->
		begin match value_kind pos value with
		| TopDef.VRecord rmap ->
			let rmap' = List.fold_left (fun rm (field, tp) ->
				try
					StrMap.add field (cast_value_to_type pos (StrMap.find field rmap) tp) rm
				with
				| Not_found ->
					Errors.error_p pos "Record defined at %s hasn't field %s."
						(Errors.string_of_pos value.TopDef.v_pos) field;
					raise Eval_exception
				) StrMap.empty rd
			in TopDef.make_value value.TopDef.v_pos (TopDef.VRecord rmap')
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected record."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TVertex _ | MlslAst.TVertexTop ->
		begin match value_kind pos value with
		| TopDef.VVertex _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected vertex program."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TFragment _ ->
		begin match value_kind pos value with
		| TopDef.VFragment _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected fragment program."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end

let print_gamma gamma =
	StrMap.iter (fun x v ->
		Printf.printf "%s = %s\n" x (
			match v.TopDef.v_kind with
			| None -> "unevaluated"
			| Some kind -> TopDef.string_of_value_kind kind)
	) gamma;
	print_endline "==============================="

let rec bind_pattern gamma pat value =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny -> gamma
	| MlslAst.PVar x -> StrMap.add x value gamma
	| MlslAst.PTypedVar(x, tp) ->
		StrMap.add x (cast_value_to_type pat.MlslAst.p_pos value tp.MlslAst.tt_typ) gamma
	| MlslAst.PTrue ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VBool true  -> gamma
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PFalse ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VBool false -> gamma
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PPair(pat1, pat2) ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VPair(v1, v2) ->
			let gamma1 = bind_pattern gamma pat1 v1 in
			bind_pattern gamma1 pat2 v2
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PConstrU name ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VConstrU name' when name = name' -> gamma
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PConstrP(name, p) ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VConstrP(name', v) when name = name' -> bind_pattern gamma p v
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end

let rec try_bind_pattern gamma pat value =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny -> Some gamma
	| MlslAst.PVar x -> Some (StrMap.add x value gamma)
	| MlslAst.PTypedVar(x, tp) -> 
		Some (StrMap.add x (cast_value_to_type pat.MlslAst.p_pos value tp.MlslAst.tt_typ) gamma)
	| MlslAst.PTrue ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VBool true -> Some gamma
		| _ -> None
		end
	| MlslAst.PFalse ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VBool false -> Some gamma
		| _ -> None
		end
	| MlslAst.PPair(pat1, pat2) ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VPair(v1, v2) ->
			Misc.Opt.bind (try_bind_pattern gamma pat1 v1) (fun gamma ->
				try_bind_pattern gamma pat2 v2)
		| _ -> None
		end
	| MlslAst.PConstrU name ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VConstrU name' when name = name' -> Some gamma
		| _ -> None
		end
	| MlslAst.PConstrP(name, pat) ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VConstrP(name', v) when name = name' ->
			try_bind_pattern gamma pat v
		| _ -> None
		end

let rec fix_pattern_pre gamma pat =
	let pos = pat.MlslAst.p_pos in
	match pat.MlslAst.p_kind with
	| MlslAst.PAny -> (TopDef.make_value_stub pos, gamma)
	| MlslAst.PVar x | MlslAst.PTypedVar(x, _) ->
		let v = TopDef.make_value_stub pos in
		(v, StrMap.add x v gamma)
	| MlslAst.PTrue | MlslAst.PFalse | MlslAst.PConstrU _ -> 
		(TopDef.make_value_stub pos, gamma)
	| MlslAst.PPair(pat1, pat2) ->
		let (v1, gamma1) = fix_pattern_pre gamma  pat1 in
		let (v2, gamma2) = fix_pattern_pre gamma1 pat2 in
		(TopDef.make_value pos (TopDef.VPair(v1, v2)), gamma2)
	| MlslAst.PConstrP(name, pat) ->
		let (v, gamma') = fix_pattern_pre gamma pat in
		(TopDef.make_value pos (TopDef.VConstrP(name, v)), gamma')

let rec fix_pattern_post pat value0 value =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny | MlslAst.PVar _ ->
		value0.TopDef.v_kind <- Some (value_kind pat.MlslAst.p_pos value);
		value
	| MlslAst.PTypedVar(x, tp) ->
		value0.TopDef.v_kind <- Some (value_kind pat.MlslAst.p_pos
			(cast_value_to_type pat.MlslAst.p_pos value tp.MlslAst.tt_typ));
		value
	| MlslAst.PTrue ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VBool true ->
			value0.TopDef.v_kind <- Some (TopDef.VBool true);
			value
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PFalse ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VBool false ->
			value0.TopDef.v_kind <- Some (TopDef.VBool false);
			value
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PPair(pat1, pat2) ->
		begin match value_kind pat.MlslAst.p_pos value0 with
		| TopDef.VPair(val10, val20) ->
			begin match value_kind pat.MlslAst.p_pos value with
			| TopDef.VPair(val1, val2) ->
				let val1' = fix_pattern_post pat1 val10 val1 in
				let val2' = fix_pattern_post pat2 val20 val2 in
				TopDef.make_value value.TopDef.v_pos (TopDef.VPair(val1', val2'))
			| _ ->
				Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
					(Errors.string_of_pos value.TopDef.v_pos);
				raise Eval_exception
			end
		| _ -> raise (Misc.Internal_error "Invalid fix skeleton.")
		end
	| MlslAst.PConstrU name ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VConstrU name' when name = name' ->
			value0.TopDef.v_kind <- Some (TopDef.VConstrU name);
			value
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PConstrP(name, pat') ->
		begin match value_kind pat.MlslAst.p_pos value0 with
		| TopDef.VConstrP(_, val0) ->
			begin match value_kind pat.MlslAst.p_pos value with
			| TopDef.VConstrP(name', val1) when name = name' ->
				let val1' = fix_pattern_post pat' val0 val1 in
				TopDef.make_value value.TopDef.v_pos (TopDef.VConstrP(name, val1'))
			| _ ->
				Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
					(Errors.string_of_pos value.TopDef.v_pos);
				raise Eval_exception
			end
		| _ -> raise (Misc.Internal_error "Invalid fix skeleton.")
		end

let rec eval gamma expr =
	let pos = expr.MlslAst.e_pos in
	match expr.MlslAst.e_kind with
	| MlslAst.EVar x ->
		begin try
			StrMap.find x gamma
		with
		| Not_found ->
			begin match TopDef.check_name x with
			| Some value -> value
			| None ->
				Errors.error_p pos "Unbound value %s" x;
				raise Eval_exception
			end
		end
	| MlslAst.EVarying _ ->
		Errors.error_p pos 
			"Can not evaluate varying variable in compilation time";
		raise Eval_exception
	| MlslAst.EInt n ->
		TopDef.make_value pos (TopDef.VInt n)
	| MlslAst.EFloat f ->
		TopDef.make_value pos (TopDef.VFloat f)
	| MlslAst.ETrue ->
		TopDef.make_value pos (TopDef.VBool true)
	| MlslAst.EFalse ->
		TopDef.make_value pos (TopDef.VBool false)
	| MlslAst.ESwizzle(e, swizzle) ->
		EvalPrim.with_exn Eval_exception (fun () ->
			let v = eval gamma e in
			EvalPrim.eval_swizzle pos v swizzle
		)
	| MlslAst.ERecord fields ->
		TopDef.make_value pos (TopDef.VRecord(
			List.fold_left (fun record field ->
				StrMap.add field.MlslAst.rfv_name (eval gamma field.MlslAst.rfv_value) record
			) StrMap.empty fields))
	| MlslAst.ESelect(e, field) ->
		let v = eval gamma e in
		begin match value_kind pos v with
		| TopDef.VRecord record ->
			begin try
				StrMap.find field record
			with
			| Not_found ->
				Errors.error_p pos "Record defined at %s hasn't field %s."
					(Errors.string_of_pos v.TopDef.v_pos) field;
				raise Eval_exception
			end
		| kind ->
			Errors.error_p pos "Value defined at %s is not a record."
				(Errors.string_of_pos v.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.EPair(e1, e2) ->
		let v1 = eval gamma e1 in
		let v2 = eval gamma e2 in
		TopDef.make_value pos (TopDef.VPair(v1, v2))
	| MlslAst.EBinOp(op, e1, e2) ->
		let v1 = eval gamma e1 in
		let v2 = eval gamma e2 in
		EvalPrim.with_exn Eval_exception (fun () ->	EvalPrim.eval_binop pos op v1 v2)
	| MlslAst.EUnOp(op, e) ->
		let v = eval gamma e in
		EvalPrim.with_exn Eval_exception (fun () -> EvalPrim.eval_unop pos op v)
	| MlslAst.EAbs(pat, body) ->
		TopDef.make_value pos (TopDef.VFunc(gamma, pat, body))
	| MlslAst.EApp(e1, e2) ->
		let func = eval gamma e1 in
		let arg  = eval gamma e2 in
		begin match value_kind pos func with
		| TopDef.VFunc(gamma', pat, body) ->
			if !credits <= 0 then begin
				Errors.error_p pos
					"Too complex functional code. Evaluation requires more than 1024 function applications.";
				raise Eval_exception
			end else begin
				credits := !credits - 1;
				eval (bind_pattern gamma' pat arg) body
			end
		| TopDef.VSampler _ ->
			Errors.error_p pos "Sampler application is unavailable in compilation time evaluation.";
			raise Eval_exception
		| TopDef.VConstrU name ->
			TopDef.make_value pos (TopDef.VConstrP(name, arg))
		| kind ->
			Errors.error_p pos "Can not apply %s defined at %s."
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos pos);
			raise Eval_exception
		end
	| MlslAst.ELet(pat, e1, e2) ->
		let v1 = eval gamma e1 in
		eval (bind_pattern gamma pat v1) e2
	| MlslAst.EFix(pat, e) ->
		let (v0, gamma') = fix_pattern_pre gamma pat in
		let v = eval gamma' e in
		fix_pattern_post pat v0 v
	| MlslAst.EIf(cnd, e1, e2) ->
		let cnd_val = eval gamma cnd in
		begin match value_kind pos cnd_val with
		| TopDef.VBool b ->
			if b then eval gamma e1 else eval gamma e2
		| kind ->
			Errors.error_p cnd.MlslAst.e_pos "Can not evaluate %s defined at %s to boolean value."
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos cnd_val.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.EMatch(e, patterns) ->
		let mval = eval gamma e in
		bind_match_patterns gamma e.MlslAst.e_pos patterns mval
	| MlslAst.EFragment e ->
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VFragment(gamma, e))
	| MlslAst.EVertex e ->
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VVertex(gamma, e))
	| MlslAst.EConstrU name ->
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VConstrU name)
	| MlslAst.EConstrP(name, e) ->
		let v = eval gamma e in
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VConstrP(name, v))

and bind_match_patterns gamma pos patterns value =
	match patterns with
	| [] ->
		Errors.error_p pos "Value defined at %s is unmatched." (Errors.string_of_pos value.TopDef.v_pos);
		raise Eval_exception
	| mpat :: patterns ->
		begin match bind_match_pattern_list gamma mpat.MlslAst.mp_patterns value
			mpat.MlslAst.mp_condition with
		| None -> bind_match_patterns gamma pos patterns value
		| Some gamma' -> eval gamma' mpat.MlslAst.mp_action
		end

and bind_match_pattern_list gamma pats value cnd_opt =
	match pats with
	| [] -> None
	| pat :: pats ->
		begin match try_bind_pattern gamma pat value, cnd_opt with
		| None, _ -> bind_match_pattern_list gamma pats value cnd_opt
		| Some gamma', None -> Some gamma'
		| Some gamma', Some cnd ->
			let cnd_val = eval gamma' cnd in
			begin match value_kind cnd.MlslAst.e_pos cnd_val with
			| TopDef.VBool b ->
				if b then Some gamma'
				else bind_match_pattern_list gamma pats value cnd_opt
			| kind ->
				Errors.error_p cnd.MlslAst.e_pos "Can not evaluate %s defined at %s to boolean value."
					(TopDef.string_of_value_kind kind) (Errors.string_of_pos cnd_val.TopDef.v_pos);
				raise Eval_exception
			end
		end

let rec bind_top_pattern pat value =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny   -> ()
	| MlslAst.PVar x ->
		TopDef.add x value
	| MlslAst.PTypedVar(x, tp) ->
		TopDef.add x (cast_value_to_type pat.MlslAst.p_pos value tp.MlslAst.tt_typ)
	| MlslAst.PTrue ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VBool true  -> ()
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PFalse ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VBool false -> ()
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PPair(pat1, pat2) ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VPair(v1, v2) ->
			bind_top_pattern pat1 v1;
			bind_top_pattern pat2 v2
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PConstrU name ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VConstrU name' when name = name' -> ()
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PConstrP(name, p) ->
		begin match value_kind pat.MlslAst.p_pos value with
		| TopDef.VConstrP(name', v) when name = name' -> bind_top_pattern p v
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	
let eval_topdef td =
	match td.MlslAst.td_kind with
	| MlslAst.TDAttrDecl(name, semantics, typ) ->
		TopDef.add_attr td.MlslAst.td_pos name semantics typ
	| MlslAst.TDConstDecl(name, typ) ->
		TopDef.add_const td.MlslAst.td_pos name typ
	| MlslAst.TDSamplerDecl(name, typ) ->
		TopDef.add_sampler td.MlslAst.td_pos name typ
	| MlslAst.TDLocalDef(pat, expr) ->
		begin try
			let value = eval StrMap.empty expr in
			bind_top_pattern pat value
		with
		| Eval_exception -> ()
		end
	| MlslAst.TDShader(name, expr) ->
		begin try
			let value = eval StrMap.empty expr in
			TopDef.add name value;
			Misc.Opt.iter (Midlang.unfold_shader td.MlslAst.td_pos name value) (fun mprog ->
			let mprog_opt = Midlang.optimize mprog in
			!target_func mprog_opt
			)
		with
		| Eval_exception -> ()
		end

let rec eval_all td_list =
	match td_list with
	| [] -> ()
	| td :: td_list ->
		eval_topdef td;
		eval_all td_list
