(* File: mlslAst.ml *)

type typ =
| TBool
| TFloat
| TInt
| TMat44
| TUnit
| TVec2
| TVec3
| TVec4
| TArrow  of typ * typ
| TRecord of (string * typ) list
| TVertex of (string * typ) list

type typ_term =
	{ tt_pos : Lexing.position
	; tt_typ : typ
	}

type expr =
	{ e_pos  : Lexing.position
	; e_kind : expr_kind
	}
and expr_kind =
| EVar     of string
| EVarying of string
| EInt     of int
| ERecord  of record_field_value list
| EPair    of expr * expr
| EMul     of expr * expr
and record_field_value =
	{ rfv_pos   : Lexing.position
	; rfv_name  : string
	; rfv_value : expr
	}

type attr_semantics =
	{ asem_name : string
	; asem_pos  : Lexing.position
	}

type topdef_kind =
| TDAttrDecl
	of string         (* name      *)
	*  attr_semantics (* semantics *)
	*  typ_term       (* type      *)
| TDConstDecl 
	of string         (* name *)
	*  typ_term       (* type *)
| TDFragmentShader
	of string         (* name *)
	*  expr           (* body *)
| TDVertexShader
	of string         (* name *)
	*  expr           (* body *)
| TDShader
	of string         (* name *)
	*  expr           (* definition *)

type topdef =
	{ td_pos  : Lexing.position
	; td_kind : topdef_kind
	}

let make_expr pos kind =
	{ e_pos  = pos
	; e_kind = kind
	}

let is_reg_type tp =
	match tp with
	| TBool | TFloat | TInt | TUnit | TVec2 | TVec3 | TVec4 -> true
	| TMat44 | TArrow _ | TRecord _ | TVertex _ -> false

let rec is_data_type tp =
	match tp with
	| TBool | TFloat | TInt | TMat44 | TUnit | TVec2 | TVec3 | TVec4 -> true
	| TArrow _ | TVertex _ -> false
	| TRecord r -> List.for_all (fun (_, t) -> is_data_type t) r

let rec string_of_typ p tp =
	match tp with
	| TBool          -> "bool"
	| TFloat         -> "float"
	| TInt           -> "int"
	| TMat44         -> "mat44"
	| TUnit          -> "unit"
	| TVec2          -> "vec2"
	| TVec3          -> "vec3"
	| TVec4          -> "vec4"
	| TArrow(t1, t2) ->
		let r = string_of_typ 1 t1 ^ " -> " ^ string_of_typ 0 t2 in
		if p > 0 then "(" ^ r ^ ")" else r
	| TRecord []     -> "{}"
	| TRecord((n, t) :: r) ->
		Printf.sprintf "{ %s : %s%s }"
			n
			(string_of_typ 0 t)
			(List.fold_left (fun s (n, t) -> s ^ "; " ^ n ^ " : " ^ string_of_typ 0 t) "" r)
	| TVertex [] -> "vertex{}"
	| TVertex((n, t) :: r) ->
		Printf.sprintf "vertex{ %s : %s%s }"
			n
			(string_of_typ 0 t)
			(List.fold_left (fun s (n, t) -> s ^ "; " ^ n ^ " : " ^ string_of_typ 0 t) "" r)
