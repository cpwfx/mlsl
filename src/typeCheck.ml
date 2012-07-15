(* File: typeCheck.ml *)

let check_topdef td =
	Errors.error_p td.MlslAst.td_pos "Unimplemented: check_topdef."

let check topdef_list =
	List.iter check_topdef topdef_list
