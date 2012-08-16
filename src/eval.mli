(* File: eval.mli *)

val set_target_func : (Midlang.shader -> unit) -> unit

val eval : TopDef.closure -> MlslAst.expr -> TopDef.value

val eval_all : MlslAst.topdef list -> unit
