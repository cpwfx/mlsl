(* File: topDef.ml *)

module StrMap = Map.Make(String)

type value =
	{         v_pos  : Errors.position
	; mutable v_kind : value_kind option
	}
and value_kind =
| VAttr     of string  * MlslAst.typ_term
| VConst    of string  * MlslAst.typ_term
| VSampler  of string  * MlslAst.typ_term
| VFragment of closure * MlslAst.expr
| VVertex   of closure * MlslAst.expr
| VBool     of bool
| VInt      of int
| VFloat    of float
| VVec      of Misc.Dim.dim * float array
| VMat      of Misc.Dim.dim * Misc.Dim.dim * float array array
| VRecord   of value Map.Make(String).t
| VPair     of value * value
| VFunc     of closure * MlslAst.pattern * MlslAst.expr
| VConstrU  of string
| VConstrP  of string * value
and closure = value Map.Make(String).t

let string_of_value_kind kind =
	match kind with
	| VAttr(name, _) -> "attribute " ^ name
	| VConst(name, _) -> "constant " ^ name
	| VSampler(name, _) -> "sampler " ^ name
	| VFragment _ -> "fragment program"
	| VVertex _ -> "vertex program"
	| VBool _ -> "boolean value"
	| VInt _ -> "integer value"
	| VFloat _ -> "floating point value"
	| VVec(dim, _) -> Printf.sprintf "%dD vector" (Misc.Dim.int_of_dim dim)
	| VMat(d1, d2, _) -> Printf.sprintf "matrix (%dx%d)" (Misc.Dim.int_of_dim d1) (Misc.Dim.int_of_dim d2)
	| VRecord _ -> "record"
	| VPair _ -> "pair"
	| VFunc _ -> "function"
	| VConstrU name -> "constructor " ^ name
	| VConstrP(name, _) -> "constructor " ^ name ^ "with value"

let empty_context = StrMap.empty

let make_value pos kind =
	{ v_pos  = pos
	; v_kind = Some kind
	}

let make_value_stub pos =
	{ v_pos  = pos
	; v_kind = None
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

let add_attr pos name typ =
	Hashtbl.replace topdef_map name (make_value pos (VAttr(name, typ)));
	attr_list_r := (name, typ) :: !attr_list_r

let add_const pos name typ =
	Hashtbl.replace topdef_map name (make_value pos (VConst(name, typ)));
	const_list_r := (name, typ) :: !const_list_r

let add_sampler pos name typ =
	Hashtbl.replace topdef_map name (make_value pos (VSampler(name, typ)));
	sampler_list_r := (name, typ) :: !sampler_list_r

let attr_list ()    = List.rev !attr_list_r
let const_list ()   = List.rev !const_list_r
let sampler_list () = List.rev !sampler_list_r
