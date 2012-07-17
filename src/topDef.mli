(* File: topDef.mli *)

val check_name : string -> (MlslAst.typ list * MlslAst.topdef) option

val add : string -> MlslAst.typ list -> MlslAst.topdef -> unit

val worlds0 : unit -> TypeWorlds.t
