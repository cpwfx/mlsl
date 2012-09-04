(* File: typeWorlds.mli *)

type world

type typ =
| TBool
| TFloat
| TInt
| TMat of Misc.Dim.dim * Misc.Dim.dim
| TSampler2D
| TSamplerCube
| TUnit
| TVec of Misc.Dim.dim
| TArrow    of typ * typ
| TPair     of typ * typ
| TRecord   of (string * typ) list
| TVertex   of (string * typ) list
| TFragment of (string * typ) list
| TVertexTop

val tMat22 : typ
val tMat23 : typ
val tMat24 : typ
val tMat32 : typ
val tMat33 : typ
val tMat34 : typ
val tMat42 : typ
val tMat43 : typ
val tMat44 : typ

type t

val main_world : world

val ancestor : world -> world -> bool

val select_type : world -> (world * typ) list -> typ

val to_ast : world -> typ -> MlslAst.typ
val of_ast : MlslAst.typ -> typ

val create : MlslAst.typ list Map.Make(String).t -> t
