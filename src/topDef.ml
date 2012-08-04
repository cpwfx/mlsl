(* File: topDef.ml *)

module StrMap = Map.Make(String)

let topdef_map = ref StrMap.empty

let attr_list_r    = ref []
let const_list_r   = ref []
let sampler_list_r = ref []

let check_name name =
	try
		Some (StrMap.find name !topdef_map)
	with
	| Not_found -> None

let add name types def =
	if StrMap.mem name !topdef_map then
		let (_, prev) = StrMap.find name !topdef_map in
		Errors.error_p def.MlslAst.td_pos
			(Printf.sprintf "Redefinition of %s. Previous definition at %s."
				name (Errors.string_of_pos prev.MlslAst.td_pos)
			);
	else begin
		topdef_map := StrMap.add name (types, def) !topdef_map;
		match def.MlslAst.td_kind with
		| MlslAst.TDAttrDecl(name, sem, tp) ->
			attr_list_r := (name, sem, tp) :: !attr_list_r
		| MlslAst.TDConstDecl(name, tp) ->
			const_list_r := (name, tp) :: !const_list_r
		| MlslAst.TDSamplerDecl(name, tp) ->
			sampler_list_r := (name, tp) :: !sampler_list_r
		| _ -> ()
	end

let worlds0 () =
	TypeWorlds.create (StrMap.map fst !topdef_map)

let attr_list ()    = List.rev !attr_list_r
let const_list ()   = List.rev !const_list_r
let sampler_list () = List.rev !sampler_list_r
