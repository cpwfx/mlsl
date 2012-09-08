(* File: evalPrim.mli *)

exception EvalPrim_exception

val eval_swizzle : Errors.position -> TopDef.value -> MlslAst.Swizzle.t -> TopDef.value

val eval_binop : Errors.position -> MlslAst.binop -> TopDef.value -> TopDef.value -> TopDef.value

val eval_unop : Errors.position -> MlslAst.unop -> TopDef.value -> TopDef.value

val with_exn : exn -> (unit -> 'a) -> 'a
