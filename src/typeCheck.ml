(* File: typeCheck.ml *)

module IntSet = Set.Make(Misc.Int)
module StrMap = Map.Make(String)

let check_field_uniqueness rd =
	let rec check_uniqueness field_map rd =
		match rd with
		| [] -> ()
		| fld :: rd ->
			(if StrMap.mem fld.MlslAst.rfv_name field_map then
				Errors.error_p fld.MlslAst.rfv_pos
					(Printf.sprintf "Redefinition of field %s. Previous definition at %s."
						fld.MlslAst.rfv_name
						(Errors.string_of_pos (StrMap.find fld.MlslAst.rfv_name field_map))
					)
			);
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
	| MlslAst.EPair(v1, v2) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EPair.";
		[]
	| MlslAst.EMul(v1, v2) ->
		Errors.error_p expr.MlslAst.e_pos "Unimplemented: infer_type EMul.";
		[]
	end

(* ========================================================================= *)

let check_semantics semantics =
	match semantics.MlslAst.asem_name with
	| "POSITION" -> ()
	| sn ->
		Errors.error_p semantics.MlslAst.asem_pos (
			Printf.sprintf "Unknown semantics: %s." sn)

let is_vertex_type (world, tp) =
	match tp with
	| TypeWorlds.TRecord r ->
		List.exists (fun (n, t) -> 
				n = "position" && TypeWorlds.to_ast world t = MlslAst.TVec4
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

let check_topdef td =
	match td.MlslAst.td_kind with
	| MlslAst.TDAttrDecl(name, semantics, typ) ->
		begin if not (MlslAst.is_reg_type typ.MlslAst.tt_typ) then
			Errors.error_p typ.MlslAst.tt_pos
				"Attributes with not register type forbidden."
		end;
		check_semantics semantics;
		TopDef.add name [typ.MlslAst.tt_typ] td
	| MlslAst.TDConstDecl(name, typ) ->
		begin if not (MlslAst.is_data_type typ.MlslAst.tt_typ) then
			Errors.error_p typ.MlslAst.tt_pos 
				"Constants with not data type are forbidden."
		end;
		TopDef.add name [typ.MlslAst.tt_typ] td
	| MlslAst.TDFragmentShader(name, body) ->
		Errors.error_p td.MlslAst.td_pos "Unimplemented: check_topdef TDFragmentShader."
	| MlslAst.TDVertexShader(name, body) ->
		begin match infer_type (TopDef.worlds0 ()) [TypeWorlds.main_world] body with
		| [] -> ()
		| sol_raw ->
			let sol     = List.filter is_vertex_type sol_raw in
			if ListExt.is_empty sol then
				Errors.error_p td.MlslAst.td_pos
					("Vertex shader should have a type { position : vec4 ; ... }, " ^
					"but types of this shader are:" ^ print_type_list sol_raw)
			else
				let types = List.map make_vertex_shader_type sol in
				TopDef.add name types td
		end
	| MlslAst.TDShader(name, definition) ->
		Errors.error_p td.MlslAst.td_pos "Unimplemented: check_topdef TDShader."

let check topdef_list =
	List.iter check_topdef topdef_list
