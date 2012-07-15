(* File: misc.ml *)

module Int = struct
	type t = int
	let compare (x : t) (y : t) = compare x y
end
