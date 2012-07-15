(* File: typeWorlds.ml *)

module StrMap = Map.Make(String)
module IntMap = Map.Make(Misc.Int)

type world =
	{ w_id     : int
	; w_parent : world option
	}

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

type t = typ list StrMap.t

let main_world =
	{ w_id     = 0
	; w_parent = None
	}

let rec ancestor w0 w1 =
	if w1.w_id = w0.w_id then true
	else match w1.w_parent with
	| None -> false
	| Some w -> ancestor w0 w

let select_type world tpl =
	snd (List.find (fun (w0, _) -> ancestor w0 world) tpl)

let rec to_ast world tp =
	match tp with
	| TBool  -> MlslAst.TBool
	| TFloat -> MlslAst.TFloat
	| TInt   -> MlslAst.TInt
	| TMat44 -> MlslAst.TMat44
	| TUnit  -> MlslAst.TUnit
	| TVec2  -> MlslAst.TVec2
	| TVec3  -> MlslAst.TVec3
	| TVec4  -> MlslAst.TVec4
	| TArrow(t1, t2) -> MlslAst.TArrow(to_ast world t1, to_ast world t2)
	| TRecord rd -> MlslAst.TRecord(List.map (fun (n, t) -> (n, to_ast world t)) rd)
	| TVertex vd -> MlslAst.TVertex(List.map (fun (n, t) -> (n, to_ast world t)) vd)

let rec of_ast tp =
	match tp with
	| MlslAst.TBool  -> TBool
	| MlslAst.TFloat -> TFloat
	| MlslAst.TInt   -> TInt
	| MlslAst.TMat44 -> TMat44
	| MlslAst.TUnit  -> TUnit
	| MlslAst.TVec2  -> TVec2
	| MlslAst.TVec3  -> TVec3
	| MlslAst.TVec4  -> TVec4
	| MlslAst.TArrow(t1, t2) -> TArrow(of_ast t1, of_ast t2)
	| MlslAst.TRecord rd -> TRecord(List.map (fun (n, t) -> (n, of_ast t)) rd)
	| MlslAst.TVertex vd -> TVertex(List.map (fun (n, t) -> (n, of_ast t)) vd)

let create vmap = StrMap.map (List.map of_ast) vmap
