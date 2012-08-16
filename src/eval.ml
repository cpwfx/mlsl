(* File: eval.ml *)

module StrMap = Map.Make(String)

exception Eval_exception

let target_func = ref (fun _ -> ())

let set_target_func f =
	target_func := f

let rec eval gamma expr =
	match expr.MlslAst.e_kind with
	| MlslAst.EVar x ->
		begin try
			StrMap.find x gamma
		with
		| Not_found ->
			begin match TopDef.check_name x with
			| Some value -> value
			| None ->
				Errors.error_p expr.MlslAst.e_pos "Unbound value %s" x;
				raise Eval_exception
			end
		end
	| MlslAst.EVarying _ ->
		Errors.error_p expr.MlslAst.e_pos 
			"Can not evaluate varying variable in compilation time";
		raise Eval_exception
	| MlslAst.EInt _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval EInt.";
		raise Eval_exception
	| MlslAst.ESwizzle _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval ESwizzle.";
		raise Eval_exception
	| MlslAst.ERecord _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval ERecord.";
		raise Eval_exception
	| MlslAst.ESelect _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval ESelect.";
		raise Eval_exception
	| MlslAst.EPair(e1, e2) ->
		let v1 = eval gamma e1 in
		let v2 = eval gamma e2 in
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VPair(v1, v2))
	| MlslAst.EBinOp _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval EBinOp.";
		raise Eval_exception
	| MlslAst.EUnOp _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval EUnOp.";
		raise Eval_exception
	| MlslAst.EAbs(pat, body) ->
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VFunc(gamma, pat, body))
	| MlslAst.EApp _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval EApp.";
		raise Eval_exception
	| MlslAst.ELet _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval ELet.";
		raise Eval_exception
	| MlslAst.EIf _ ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: eval EIf.";
		raise Eval_exception
	| MlslAst.EFragment e ->
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VFragment(gamma, e))
	| MlslAst.EVertex e ->
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VVertex(gamma, e))

let bind_top_pattern pat value =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny   -> ()
	| MlslAst.PVar x | MlslAst.PTypedVar(x, _) ->
		TopDef.add x value
	
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
			Misc.Opt.iter (Midlang.unfold_shader name value) (fun mprog ->
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
