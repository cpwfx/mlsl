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
| TArrow    of typ * typ
| TPair     of typ * typ
| TRecord   of (string * typ) list
| TVertex   of (string * typ) list
| TFragment of (string * typ) list
| TVertexTop

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
	| TMat44 | TArrow _ | TPair _ | TRecord _ | TVertex _ | TFragment _ 
	| TVertexTop -> false

let rec is_data_type tp =
	match tp with
	| TBool | TFloat | TInt | TMat44 | TUnit | TVec2 | TVec3 | TVec4 -> true
	| TArrow _ | TVertex _ | TFragment _ | TVertexTop -> false
	| TRecord r -> List.for_all (fun (_, t) -> is_data_type t) r
	| TPair(t1, t2) -> is_data_type t1 && is_data_type t2

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
	| TPair(t1, t2) ->
		let r = string_of_typ 1 t1 ^ " * " ^ string_of_typ 2 t2 in
		if p > 1 then "(" ^ r ^ ")" else r
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
	| TFragment [] -> "fragment{}"
	| TFragment((n, t) :: r) ->
		Printf.sprintf "fragment{ %s : %s%s }"
			n
			(string_of_typ 0 t)
			(List.fold_left (fun s (n, t) -> s ^ "; " ^ n ^ " : " ^ string_of_typ 0 t) "" r)
	| TVertexTop -> "vertex_top"

let foreachShader td_list f =
	List.iter (fun td ->
			match td.td_kind with
			| TDShader(name, definition) -> f td name definition
			| _ -> ()
		) td_list
