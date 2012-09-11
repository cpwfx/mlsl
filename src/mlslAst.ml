(* File: mlslAst.ml *)

open Misc.Dim

module Swizzle = struct
	exception Not_swizzle

	type component =
	| X
	| Y
	| Z
	| W

	type t =
	| S1 of component
	| S2 of component * component
	| S3 of component * component * component
	| S4 of component * component * component * component

	let component_of_char c =
		match c with
		| 'x' -> X
		| 'y' -> Y
		| 'z' -> Z
		| 'w' -> W
		| _ -> raise Not_swizzle

	let of_string str =
		match String.length str with
		| 1 -> S1 (component_of_char (str.[0]))
		| 2 -> S2
				( component_of_char (str.[0])
				, component_of_char (str.[1])
				)
		| 3 -> S3
				( component_of_char (str.[0])
				, component_of_char (str.[1])
				, component_of_char (str.[2])
				)
		| 4 -> S4
				( component_of_char (str.[0])
				, component_of_char (str.[1])
				, component_of_char (str.[2])
				, component_of_char (str.[3])
				)
		| _ -> raise Not_swizzle

	let try_of_string str =
		try Some(of_string str) with
		| Not_swizzle -> None

	let component_to_string c =
		match c with
		| X -> "x"
		| Y -> "y"
		| Z -> "z"
		| W -> "w"

	let to_string swizzle =
		match swizzle with
		| S1 c1              -> component_to_string c1
		| S2(c1, c2)         -> component_to_string c1 ^ component_to_string c2
		| S3(c1, c2, c3)     -> 
			component_to_string c1 ^ component_to_string c2 ^ component_to_string c3
		| S4(c1, c2, c3, c4) -> 
			component_to_string c1 ^ component_to_string c2 ^ 
			component_to_string c3 ^ component_to_string c4

	let size swizzle =
		match swizzle with
		| S1 _ -> 1
		| S2 _ -> 2
		| S3 _ -> 3
		| S4 _ -> 4

	let component_id c =
		match c with
		| X -> 0
		| Y -> 1
		| Z -> 2
		| W -> 3

	let max_component_id swizzle =
		match swizzle with
		| S1 c1              -> component_id c1
		| S2(c1, c2)         -> max (component_id c1) (component_id c2)
		| S3(c1, c2, c3)     -> 
			max (component_id c1) (max (component_id c2) (component_id c3))
		| S4(c1, c2, c3, c4) ->  max 
				(max (component_id c1) (component_id c2))
				(max (component_id c3) (component_id c4))
end

type typ =
| TBool
| TFloat
| TInt
| TMat of dim * dim
| TSampler2D
| TSamplerCube
| TUnit
| TVec of dim
| TArrow    of typ * typ
| TPair     of typ * typ
| TRecord   of (string * typ) list
| TVertex   of (string * typ) list
| TFragment of (string * typ) list
| TVertexTop

type typ_term =
	{ tt_pos : Errors.position
	; tt_typ : typ
	}

type pattern =
	{ p_pos  : Errors.position
	; p_kind : pattern_kind
	}
and pattern_kind =
| PAny
| PVar      of string
| PTypedVar of string * typ_term
| PTrue
| PFalse
| PPair     of pattern * pattern
| PConstrU  of string
| PConstrP  of string * pattern

type binop =
| BOEq
| BONeq
| BOLe
| BOLt
| BOGe
| BOGt
| BOAdd
| BOSub
| BOMul
| BODiv
| BOMod
| BODot
| BOCross
| BOPow
(* Operators unavailabe from code (only from builtins) *)
| BOMin

type unop =
| UONeg
| UOPlus

type expr =
	{ e_pos  : Errors.position
	; e_kind : expr_kind
	}
and expr_kind =
| EVar      of string
| EVarying  of string
| EInt      of int
| ETrue
| EFalse
| EFloat    of float
| ESwizzle  of expr * Swizzle.t
| ERecord   of record_field_value list
| ESelect   of expr * string
| EPair     of expr * expr
| EBinOp    of binop * expr * expr
| EUnOp     of unop * expr
| EAbs      of pattern * expr
| EApp      of expr * expr
| ELet      of pattern * expr * expr
| EFix      of pattern * expr
| EIf       of expr * expr * expr
| EMatch    of expr * match_pattern list
| EFragment of expr
| EVertex   of expr
| EConstrU  of string
| EConstrP  of string * expr
and record_field_value =
	{ rfv_pos   : Errors.position
	; rfv_name  : string
	; rfv_value : expr
	}
and match_pattern =
	{ mp_patterns  : pattern list
	; mp_condition : expr option
	; mp_action    : expr
	}

type topdef_kind =
| TDAttrDecl
	of string         (* name      *)
	*  typ_term       (* type      *)
| TDConstDecl 
	of string         (* name *)
	*  typ_term       (* type *)
| TDSamplerDecl
	of string         (* name *)
	*  typ_term       (* type *)
| TDLocalDef
	of pattern        (* pattern *)
	*  expr           (* value   *)
| TDShader
	of string         (* name *)
	*  expr           (* definition *)

type topdef =
	{ td_pos  : Errors.position
	; td_kind : topdef_kind
	}

let make_pattern pos kind =
	{ p_pos  = Errors.UserPos pos
	; p_kind = kind
	}

let make_list_pattern_rev pos l =
	let rec make_acc acc xs =
		match xs with
		| [] -> acc
		| x :: xs -> make_acc (make_pattern pos (PConstrP("::", make_pattern pos (PPair(x, acc))))) xs
	in make_acc (make_pattern pos (PConstrU "[]")) l

let make_expr pos kind =
	{ e_pos  = Errors.UserPos pos
	; e_kind = kind
	}

let rec make_abs_rev pos args body =
	match args with
	| [] -> body
	| arg :: args -> make_abs_rev pos args (make_expr pos (EAbs(arg, body)))

let rec make_app func args =
	match args with
	| [] -> func
	| arg :: args -> make_app ({ e_pos = arg.e_pos; e_kind = EApp(func, arg)}) args

let make_select pos expr field =
	match Swizzle.try_of_string field with
	| None         -> make_expr pos (ESelect(expr, field))
	| Some swizzle -> make_expr pos (ESwizzle(expr, swizzle))

let rec split_let_rec_def pos defs =
	match defs with
	| [] -> (make_pattern pos (PConstrU "()"), make_expr pos (EConstrU "()"))
	| [ (pat, expr) ] -> (pat, expr)
	| (pat, expr) :: defs ->
		let (pats, exprs) = split_let_rec_def pos defs in
			(make_pattern pos (PPair(pat, pats)), make_expr pos (EPair(expr, exprs)))

let make_let_rec pos defs expr =
	let (pat, e1) = split_let_rec_def pos defs in
	make_expr pos (ELet(pat, make_expr pos (EFix(pat, e1)), expr))

let make_list_rev pos l =
	let rec make_acc acc xs =
		match xs with
		| [] -> acc
		| x :: xs -> make_acc (make_expr pos (EConstrP("::", make_expr pos (EPair(x, acc))))) xs
	in make_acc (make_expr pos (EConstrU "[]")) l

let make_topdef_let_rec pos defs =
	let (pat, e1) = split_let_rec_def pos defs in
	{ td_pos  = Errors.UserPos pos
	; td_kind = TDLocalDef(pat, make_expr pos (EFix(pat, e1)))
	}

let is_reg_type tp =
	match tp with
	| TBool | TFloat | TInt | TUnit | TVec _ -> true
	| TSampler2D | TSamplerCube | TMat _ | TArrow _ | TPair _ | TRecord _ 
	| TVertex _ | TFragment _ | TVertexTop -> false

let rec is_data_type tp =
	match tp with
	| TBool | TFloat | TInt | TMat _ | TUnit | TVec _ -> true
	| TSampler2D | TSamplerCube | TArrow _ | TVertex _ | TFragment _ 
	| TVertexTop -> false
	| TRecord r -> List.for_all (fun (_, t) -> is_data_type t) r
	| TPair(t1, t2) -> is_data_type t1 && is_data_type t2

let rec is_sampler_type tp =
	match tp with
	| TSampler2D | TSamplerCube -> true
	| TBool | TFloat | TInt | TMat _ | TUnit | TVec _ | TArrow _ | TPair _ 
	| TRecord _ | TVertex _ | TFragment _ | TVertexTop -> false

let rec string_of_typ p tp =
	match tp with
	| TBool          -> "bool"
	| TFloat         -> "float"
	| TInt           -> "int"
	| TMat(d1, d2)   -> Printf.sprintf "mat%d%d" (int_of_dim d1) (int_of_dim d2)
	| TSampler2D     -> "sampler2D"
	| TSamplerCube   -> "samplerCube"
	| TUnit          -> "unit"
	| TVec d         -> Printf.sprintf "vec%d" (int_of_dim d)
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

let binop_name op =
	match op with
	| BOEq    -> "equality"
	| BONeq   -> "inequality"
	| BOLe    -> "less or equal test"
	| BOLt    -> "less-then test"
	| BOGe    -> "greater of equal test"
	| BOGt    -> "greater-then test"
	| BOAdd   -> "addition"
	| BOSub   -> "subtraction"
	| BOMul   -> "multiplication"
	| BODiv   -> "division"
	| BOMod   -> "modulo"
	| BODot   -> "dot product"
	| BOCross -> "cross product"
	| BOPow   -> "power"
	| BOMin   -> "minimum"

let unop_name op =
	match op with
	| UONeg  -> "unary minus"
	| UOPlus -> "unary plus"

let foreachShader td_list f =
	List.iter (fun td ->
			match td.td_kind with
			| TDShader(name, definition) -> f td name definition
			| _ -> ()
		) td_list
