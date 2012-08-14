(* File: eval.mli *)

val eval : TopDef.closure -> MlslAst.expr -> TopDef.value

val eval_all : MlslAst.topdef list -> unit
