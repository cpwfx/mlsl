(* File: json.ml *)

type json_list = json Misc.ImpList.t
and  json_obj = (string * json) Misc.ImpList.t

and json =
| JsInt    of int
| JsList   of json_list
| JsObj    of json_obj
| JsString of string

let create_list = Misc.ImpList.of_list
let create_obj = Misc.ImpList.of_list

let list_add = Misc.ImpList.add

let rec write_json out indent json =
	match json with
	| JsInt  n -> output_string out (string_of_int n)
	| JsList l ->
		begin match Misc.ImpList.to_list l with
		| [] -> output_string out "[ ]";
		| ll -> output_string out "\n"; write_list out indent ll
		end
	| JsObj o ->
		begin match Misc.ImpList.to_list o with
		| []  -> output_string out "{ }";
		| obj -> output_string out "\n"; write_obj out indent obj
		end
	| JsString s -> output_string out ("\"" ^ String.escaped s ^ "\"")

and write_list out indent ll =
	output_string out indent;
	match ll with
	| [] -> output_string out "[ ]"
	| x :: xs ->
		output_string out "[ ";
		write_json out (indent ^ "  ") x;
		output_string out "\n";
		write_list_tail out indent xs
and write_list_tail out indent ll =
	output_string out indent;
	match ll with
	| [] -> output_string out "]"
	| x :: xs ->
		output_string out ", ";
		write_json out (indent ^ "  ") x;
		output_string out "\n";
		write_list_tail out indent xs

and write_obj out indent obj =
	output_string out indent;
	match obj with
	| [] -> output_string out "{ }"
	| (name, value) :: tail ->
		output_string out ("{ \"" ^ String.escaped name ^ "\": ");
		write_json out (indent ^ "  ") value;
		output_string out "\n";
		write_obj_tail out indent tail
and write_obj_tail out indent obj =
	output_string out indent;
	match obj with
	| [] -> output_string out "}"
	| (name, value) :: tail ->
		output_string out (", \"" ^ String.escaped name ^ "\": ");
		write_json out (indent ^ "  ") value;
		output_string out "\n";
		write_obj_tail out indent tail

let write path obj =
	Misc.IO.with_out_channel path false (fun out ->
		write_obj out "" (Misc.ImpList.to_list obj)
	)
