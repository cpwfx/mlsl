(* File: typeCheck.ml *)

module IntSet = Set.Make(Misc.Int)
module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

module Globals = struct
	let glob_map = ref StrMap.empty

	let add name types pos =
		if StrMap.mem name !glob_map then
			let (_, prev) = StrMap.find name !glob_map in
			Errors.error_p pos
				"Redefinition of %s. Previous definition at %s."
				name (Errors.string_of_pos prev)
		else
			glob_map := StrMap.add name (types, pos) !glob_map

	let rec declare_pattern pat =
		match pat.MlslAst.p_kind with
		| MlslAst.PAny | MlslAst.PTrue | MlslAst.PFalse | MlslAst.PConstrU _ -> ()
		| MlslAst.PVar x | MlslAst.PTypedVar(x, _) ->
			add x [] pat.MlslAst.p_pos
		| MlslAst.PPair(pat1, pat2) ->
			declare_pattern pat1;
			declare_pattern pat2
		| MlslAst.PConstrP(_, pat) ->
			declare_pattern pat

	let check_name name =
		try
			Some (StrMap.find name !glob_map)
		with
		| Not_found -> None

	let worlds0 () =
		TypeWorlds.create (StrMap.map fst !glob_map)
end

let declare_builtin name types =
	Globals.add name types Errors.BuiltinPos

let check_field_uniqueness rd =
	let rec check_uniqueness field_map rd =
		match rd with
		| [] -> ()
		| fld :: rd ->
			begin if StrMap.mem fld.MlslAst.rfv_name field_map then
				Errors.error_p fld.MlslAst.rfv_pos
					"Redefinition of field %s. Previous definition at %s."
					fld.MlslAst.rfv_name
					(Errors.string_of_pos (StrMap.find fld.MlslAst.rfv_name field_map));
			end;
			check_uniqueness (StrMap.add fld.MlslAst.rfv_name fld.MlslAst.rfv_pos field_map) rd
	in check_uniqueness StrMap.empty rd

let rec infer_type gamma worlds expr =
	match worlds with
	| [] -> []
	| _ ->
	begin match expr.MlslAst.e_kind with
	| MlslAst.EVar x ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EVar.";
		[]
	| MlslAst.EVarying x ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EVarying.";
		[]
	| MlslAst.EInt n ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EInt.";
		[]
	| MlslAst.ETrue ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type ETrue.";
		[]
	| MlslAst.EFalse ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EFalse.";
		[]
	| MlslAst.ESwizzle(v1, swizzle) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type ESwizzle.";
		[]
	| MlslAst.ERecord rd ->
		check_field_uniqueness rd;
		let (rev_raw_record, worlds') = 
			List.fold_left (fun (rrr, wl) field ->
					let tp = infer_type gamma wl field.MlslAst.rfv_value in
					((field.MlslAst.rfv_name, tp) :: rrr, List.map fst tp)
				) ([], worlds) rd in
		List.map (fun wrld ->
				(wrld, TypeWorlds.TRecord (
					List.rev_map (fun (n, ltp) -> 
							(n, TypeWorlds.select_type wrld ltp)
						) rev_raw_record)
			)) worlds'
	| MlslAst.ESelect(v1, field) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type ESelect.";
		[]
	| MlslAst.EPair(v1, v2) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EPair.";
		[]
	| MlslAst.EBinOp(op, v1, v2) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EBinOp.";
		[]
	| MlslAst.EUnOp(op, v) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EUnOp.";
		[]
	| MlslAst.EAbs(pat, e) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EAbs.";
		[]
	| MlslAst.EApp(e1, e2) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EApp.";
		[]
	| MlslAst.ELet(pat, e1, e2) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type ELet.";
		[]
	| MlslAst.EFix(pat, e1) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EFix.";
		[]
	| MlslAst.EIf(e1, e2, e3) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EIf.";
		[]
	| MlslAst.EMatch(e1, mp) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EMatch.";
		[]
	| MlslAst.EFragment e ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EFragment.";
		[]
	| MlslAst.EVertex e ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EVertex.";
		[]
	| MlslAst.EConstrU name ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EConstrU.";
		[]
	| MlslAst.EConstrP(name, e) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EConstrP.";
		[]
	end

(* ========================================================================= *)

let rec pattern_variables pat =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny -> StrSet.empty
	| MlslAst.PVar x | MlslAst.PTypedVar(x, _) -> StrSet.singleton x
	| MlslAst.PTrue | MlslAst.PFalse -> StrSet.empty
	| MlslAst.PPair(pat1, pat2) -> 
		StrSet.union (pattern_variables pat1) (pattern_variables pat2)
	| MlslAst.PConstrU _ -> StrSet.empty
	| MlslAst.PConstrP(_, pat) -> pattern_variables pat

let rec fast_check_pattern gamma pat =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny -> gamma
	| MlslAst.PVar x | MlslAst.PTypedVar(x, _) -> StrSet.add x gamma
	| MlslAst.PTrue  | MlslAst.PFalse -> gamma
	| MlslAst.PPair(pat1, pat2) ->
		let gamma' = fast_check_pattern gamma pat1 in
		fast_check_pattern gamma' pat2
	| MlslAst.PConstrU _ -> gamma
	| MlslAst.PConstrP(_, pat) -> fast_check_pattern gamma pat

let rec fast_check_code gamma expr =
	match expr.MlslAst.e_kind with
	| MlslAst.EVar x ->
		if StrSet.mem x gamma then ()
		else begin match Globals.check_name x with
		| None ->
			Errors.error_p expr.MlslAst.e_pos "Undefined variable %s." x
		| Some _ -> ()
		end
	| MlslAst.EVarying x -> ()
	| MlslAst.EInt n -> ()
	| MlslAst.ETrue  -> ()
	| MlslAst.EFalse -> ()
	| MlslAst.ESwizzle(v, _) ->
		fast_check_code gamma v
	| MlslAst.ERecord rd ->
		check_field_uniqueness rd;
		List.iter (fun field -> fast_check_code gamma field.MlslAst.rfv_value) rd
	| MlslAst.ESelect(v, _) ->
		fast_check_code gamma v
	| MlslAst.EPair(v1, v2) ->
		fast_check_code gamma v1;
		fast_check_code gamma v2
	| MlslAst.EBinOp(_, v1, v2) ->
		fast_check_code gamma v1;
		fast_check_code gamma v2
	| MlslAst.EUnOp(_, v) ->
		fast_check_code gamma v
	| MlslAst.EAbs(pat, e) ->
		fast_check_code (fast_check_pattern gamma pat) e
	| MlslAst.EApp(e1, e2) ->
		fast_check_code gamma e1;
		fast_check_code gamma e2
	| MlslAst.ELet(pat, e1, e2) ->
		fast_check_code gamma e1;
		fast_check_code (fast_check_pattern gamma pat) e2
	| MlslAst.EFix(pat, e) ->
		fast_check_code (fast_check_pattern gamma pat) e
	| MlslAst.EIf(e1, e2, e3) ->
		fast_check_code gamma e1;
		fast_check_code gamma e2;
		fast_check_code gamma e3
	| MlslAst.EMatch(e, mps) ->
		fast_check_code gamma e;
		List.iter (fast_check_match_pattern gamma) mps
	| MlslAst.EFragment e ->
		fast_check_code gamma e
	| MlslAst.EVertex e ->
		fast_check_code gamma e
	| MlslAst.EConstrU _ -> ()
	| MlslAst.EConstrP(_, e) ->
		fast_check_code gamma e

and fast_check_match_pattern gamma mp =
	let gamma' =
		match mp.MlslAst.mp_patterns with
		| []          -> gamma
		| pat :: pats ->
			let new_vars = pattern_variables pat in
			List.iter (fun p ->
					if not (StrSet.equal new_vars (pattern_variables p)) then
						Errors.error_p p.MlslAst.p_pos "This patterns define different variables."
					else ()
				) pats;
			fast_check_pattern gamma pat
	in
	Misc.Opt.iter mp.MlslAst.mp_condition (fast_check_code gamma');
	fast_check_code gamma' mp.MlslAst.mp_action

(* ========================================================================= *)

let check_semantics semantics =
	match semantics.MlslAst.asem_name with
	| "INPUT0"    -> ()
	| "INPUT1"    -> ()
	| "INPUT2"    -> ()
	| "INPUT3"    -> ()
	| "INPUT4"    -> ()
	| "INPUT5"    -> ()
	| "INPUT6"    -> ()
	| "INPUT7"    -> ()
	| "POSITION"  -> ()
	| "TEXCOORD0" -> ()
	| "TEXCOORD1" -> ()
	| "TEXCOORD2" -> ()
	| "TEXCOORD3" -> ()
	| sn ->
		Errors.error_p semantics.MlslAst.asem_pos "Unknown semantics: %s." sn

let is_vertex_type (world, tp) =
	match tp with
	| TypeWorlds.TRecord r ->
		List.exists (fun (n, t) -> 
				n = "position" && TypeWorlds.to_ast world t = MlslAst.TVec Misc.Dim.Dim4
			) r
	| _ -> false

let make_vertex_shader_type (wrld, tp) =
	match tp with
	| TypeWorlds.TRecord rd ->
		TypeWorlds.to_ast wrld (
				TypeWorlds.TVertex (List.filter (fun (n, _) -> n <> "position") rd)
			)
	| _ -> MlslAst.TUnit

let print_type_list l = 
	List.fold_left (fun s (w, t) -> 
			s ^ "\n" ^ MlslAst.string_of_typ 0 (TypeWorlds.to_ast w t)
		) "" l

(* ========================================================================= *)

let check_attr_decl td name semantics typ =
	begin if not (MlslAst.is_reg_type typ.MlslAst.tt_typ) then
		Errors.error_p typ.MlslAst.tt_pos
			"Attributes with not register type forbidden."
	end;
	check_semantics semantics;
	Globals.add name [typ.MlslAst.tt_typ] td.MlslAst.td_pos

let check_const_decl td name typ =
	begin if not (MlslAst.is_data_type typ.MlslAst.tt_typ) then
		Errors.error_p typ.MlslAst.tt_pos 
			"Constants with not data type are forbidden."
	end;
	Globals.add name [typ.MlslAst.tt_typ] td.MlslAst.td_pos

let check_sampler_decl td name typ =
	begin if not (MlslAst.is_sampler_type typ.MlslAst.tt_typ) then
		Errors.error_p typ.MlslAst.tt_pos
			"Sampler should have a sampler type."
	end;
	Globals.add name [typ.MlslAst.tt_typ] td.MlslAst.td_pos

let fast_check_local_def td pat expr =
	fast_check_code StrSet.empty expr;
	Globals.declare_pattern pat

let check_fragment_shader td name body =
	Errors.error_p td.MlslAst.td_pos "Unimplemented: check_topdef TDFragmentShader."

let fast_check_fragment_shader td name body =
	fast_check_code StrSet.empty body;
	Globals.add name [MlslAst.TFragment []] td.MlslAst.td_pos

let check_vertex_shader td name body =
	begin match infer_type (Globals.worlds0 ()) [TypeWorlds.main_world] body with
	| [] -> ()
	| sol_raw ->
		let sol     = List.filter is_vertex_type sol_raw in
		if Misc.ListExt.is_empty sol then
			Errors.error_p td.MlslAst.td_pos
				("Vertex shader should have a type { position : vec4 ; ... }, " ^^
				"but types of this shader are: %s") (print_type_list sol_raw)
		else
			let types = List.map make_vertex_shader_type sol in
			Globals.add name types td.MlslAst.td_pos
	end

let fast_check_vertex_shader td name body =
	fast_check_code StrSet.empty body;
	Globals.add name [MlslAst.TVertexTop] td.MlslAst.td_pos

let check_shader td name definition =
	Errors.error_p td.MlslAst.td_pos "Unimplemented: check_topdef TDShader."

let fast_check_shader td name definition =
	fast_check_code StrSet.empty definition;
	Globals.add name [MlslAst.TPair(MlslAst.TVertexTop, MlslAst.TFragment [])]
		td.MlslAst.td_pos

(* ========================================================================= *)

let check_topdef td =
	match td.MlslAst.td_kind with
	| MlslAst.TDAttrDecl(name, semantics, typ) ->
		check_attr_decl td name semantics typ
	| MlslAst.TDConstDecl(name, typ) ->
		check_const_decl td name typ
	| MlslAst.TDSamplerDecl(name, typ) ->
		check_sampler_decl td name typ
	| MlslAst.TDLocalDef(pat, expr) ->
		Errors.error_p td.MlslAst.td_pos "Unimplemented: check_topdef TDLocalDef."
	| MlslAst.TDShader(name, definition) ->
		check_shader td name definition

let fast_check_topdef td =
	match td.MlslAst.td_kind with
	| MlslAst.TDAttrDecl(name, semantics, typ) ->
		check_attr_decl td name semantics typ
	| MlslAst.TDConstDecl(name, typ) ->
		check_const_decl td name typ
	| MlslAst.TDSamplerDecl(name, typ) ->
		check_sampler_decl td name typ
	| MlslAst.TDLocalDef(pat, expr) ->
		fast_check_local_def td pat expr
	| MlslAst.TDShader(name, definition) ->
		fast_check_shader td name definition

let check topdef_list =
	List.iter check_topdef topdef_list

let fast_check topdef_list =
	List.iter fast_check_topdef topdef_list
