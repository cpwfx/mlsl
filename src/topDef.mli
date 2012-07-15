(* File: topDef.mli *)

val add : string -> MlslAst.typ list -> MlslAst.topdef -> unit

val worlds0 : unit -> TypeWorlds.t
