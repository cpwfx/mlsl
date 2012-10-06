(* File: typeCheck.mli *)

val declare_builtin : string -> MlslAst.typ list -> unit

val check : MlslAst.topdef list -> unit
