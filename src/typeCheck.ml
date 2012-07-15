(* File: typeCheck.ml *)

let check_semantics semantics =
	match semantics.MlslAst.asem_name with
	| "POSITION" -> ()
	| sn ->
		Errors.error_p semantics.MlslAst.asem_pos (
			Printf.sprintf "Unknown semantics: %s." sn)

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
		Errors.error_p td.MlslAst.td_pos "Unimplemented: check_topdef TDVertexShader."
	| MlslAst.TDShader(name, definition) ->
		Errors.error_p td.MlslAst.td_pos "Unimplemented: check_topdef TDShader."

let check topdef_list =
	List.iter check_topdef topdef_list
