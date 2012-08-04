(* File: typeWorlds.mli *)

type world

type typ =
| TBool
| TFloat
| TInt
| TMat44
| TSampler2D
| TSamplerCube
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

type t

val main_world : world

val ancestor : world -> world -> bool

val select_type : world -> (world * typ) list -> typ

val to_ast : world -> typ -> MlslAst.typ
val of_ast : MlslAst.typ -> typ

val create : MlslAst.typ list Map.Make(String).t -> t
