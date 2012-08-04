(* File: topDef.mli *)

val check_name : string -> (MlslAst.typ list * MlslAst.topdef) option

val add : string -> MlslAst.typ list -> MlslAst.topdef -> unit

val worlds0 : unit -> TypeWorlds.t

val attr_list : unit -> (string * MlslAst.attr_semantics * MlslAst.typ_term) list
val const_list : unit -> (string * MlslAst.typ_term) list
val sampler_list : unit -> (string * MlslAst.typ_term) list
