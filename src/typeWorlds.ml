(* File: typeWorlds.ml *)

open Misc.Dim

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

let tMat22 = TMat(Dim2, Dim2)
let tMat23 = TMat(Dim2, Dim3)
let tMat24 = TMat(Dim2, Dim4)
let tMat32 = TMat(Dim3, Dim2)
let tMat33 = TMat(Dim3, Dim3)
let tMat34 = TMat(Dim3, Dim4)
let tMat42 = TMat(Dim4, Dim2)
let tMat43 = TMat(Dim4, Dim3)
let tMat44 = TMat(Dim4, Dim4)

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
	| TBool        -> MlslAst.TBool
	| TFloat       -> MlslAst.TFloat
	| TInt         -> MlslAst.TInt
	| TMat(d1, d2) -> MlslAst.TMat(d1, d2)
	| TSampler2D   -> MlslAst.TSampler2D
	| TSamplerCube -> MlslAst.TSamplerCube
	| TUnit        -> MlslAst.TUnit
	| TVec d       -> MlslAst.TVec d
	| TArrow(t1, t2) -> MlslAst.TArrow(to_ast world t1, to_ast world t2)
	| TPair(t1, t2) -> MlslAst.TPair(to_ast world t1, to_ast world t2)
	| TRecord rd -> MlslAst.TRecord(List.map (fun (n, t) -> (n, to_ast world t)) rd)
	| TVertex vd -> MlslAst.TVertex(List.map (fun (n, t) -> (n, to_ast world t)) vd)
	| TFragment vd -> MlslAst.TFragment(List.map (fun (n, t) -> (n, to_ast world t)) vd)
	| TVertexTop -> MlslAst.TVertexTop

let rec of_ast tp =
	match tp with
	| MlslAst.TBool        -> TBool
	| MlslAst.TFloat       -> TFloat
	| MlslAst.TInt         -> TInt
	| MlslAst.TMat(d1, d2) -> TMat(d1, d2)
	| MlslAst.TSampler2D   -> TSampler2D
	| MlslAst.TSamplerCube -> TSamplerCube
	| MlslAst.TUnit        -> TUnit
	| MlslAst.TVec d       -> TVec d
	| MlslAst.TArrow(t1, t2) -> TArrow(of_ast t1, of_ast t2)
	| MlslAst.TPair(t1, t2) -> TPair(of_ast t1, of_ast t2)
	| MlslAst.TRecord rd -> TRecord(List.map (fun (n, t) -> (n, of_ast t)) rd)
	| MlslAst.TVertex vd -> TVertex(List.map (fun (n, t) -> (n, of_ast t)) vd)
	| MlslAst.TFragment vd -> TFragment(List.map (fun (n, t) -> (n, of_ast t)) vd)
	| MlslAst.TVertexTop -> TVertexTop

let create vmap = StrMap.map (List.map of_ast) vmap
