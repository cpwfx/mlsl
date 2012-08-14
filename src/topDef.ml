(* File: topDef.ml *)

module StrMap = Map.Make(String)

type value =
	{ v_pos  : Errors.position
	; v_kind : value_kind
	}
and value_kind =
| VAttr     of string  * MlslAst.attr_semantics * MlslAst.typ_term
| VConst    of string  * MlslAst.typ_term
| VSampler  of string  * MlslAst.typ_term
| VFragment of closure * MlslAst.expr
| VVertex   of closure * MlslAst.expr
| VPair     of value * value
| VFunc     of closure * MlslAst.pattern * MlslAst.expr
and closure = value Map.Make(String).t

let empty_context = StrMap.empty

let make_value pos kind =
	{ v_pos  = pos
	; v_kind = kind
	}

let topdef_map = Hashtbl.create 32

let add name value =
	Hashtbl.replace topdef_map name value

let attr_list_r    = ref []
let const_list_r   = ref []
let sampler_list_r = ref []

let check_name name =
	try
		Some (Hashtbl.find topdef_map name)
	with
	| Not_found -> None

let add_attr pos name semantics typ =
	Hashtbl.replace topdef_map name (make_value pos (VAttr(name, semantics, typ)));
	attr_list_r := (name, semantics, typ) :: !attr_list_r

let add_const pos name typ =
	Hashtbl.replace topdef_map name (make_value pos (VConst(name, typ)));
	const_list_r := (name, typ) :: !const_list_r

let add_sampler pos name typ =
	Hashtbl.replace topdef_map name (make_value pos (VSampler(name, typ)));
	sampler_list_r := (name, typ) :: !sampler_list_r

let attr_list ()    = List.rev !attr_list_r
let const_list ()   = List.rev !const_list_r
let sampler_list () = List.rev !sampler_list_r
