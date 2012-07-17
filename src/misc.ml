(* File: misc.ml *)

module Int = struct
	type t = int
	let compare (x : t) (y : t) = compare x y
end

module Opt = struct
	type 'a t = 'a option
	let bind a f =
		match a with
		| None -> None
		| Some av -> f av
	let iter a f =
		match a with
		| None -> ()
		| Some av -> f av
end
