(* File: json.mli *)

type json_list
type json_obj

type json =
| JsInt    of int
| JsList   of json_list
| JsObj    of json_obj
| JsString of string

val create_list : json list -> json_list
val create_obj  : (string * json) list -> json_obj

val list_add : json_list -> json -> unit

val write : string -> json_obj -> unit
