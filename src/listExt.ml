(* File: listExt.ml *)

let is_empty l =
	match l with
	| []     -> true
	| _ :: _ -> false
