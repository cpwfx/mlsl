(* File: topDef.mli *)

type value =
	{ v_pos  : Errors.position
	; v_kind : value_kind
	}
and value_kind =
| VAttr     of string  * MlslAst.attr_semantics * MlslAst.typ_term
| VConst    of string * MlslAst.typ_term
| VSampler  of string  * MlslAst.typ_term
| VFragment of closure * MlslAst.expr
| VVertex   of closure * MlslAst.expr
| VPair     of value * value
| VFunc     of closure * MlslAst.pattern * MlslAst.expr
and closure = value Map.Make(String).t

val empty_context : closure

val make_value : Errors.position -> value_kind -> value

val add : string -> value -> unit

val check_name : string -> value option

val add_attr    : Errors.position -> string -> MlslAst.attr_semantics -> MlslAst.typ_term -> unit
val add_const   : Errors.position -> string -> MlslAst.typ_term -> unit
val add_sampler : Errors.position -> string -> MlslAst.typ_term -> unit

val attr_list : unit -> (string * MlslAst.attr_semantics * MlslAst.typ_term) list
val const_list : unit -> (string * MlslAst.typ_term) list
val sampler_list : unit -> (string * MlslAst.typ_term) list
