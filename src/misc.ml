(* File: misc.ml *)

exception InternalError

module Fresh = struct
	type t = int ref
	let create () = ref 0
	let next f =
		let res = !f in
		f := res + 1;
		res
end

module Int = struct
	type t = int
	let compare (x : t) (y : t) = compare x y
end

module ImpList = struct
	type 'a t = ('a list) ref
	let add l x = l := x :: !l
	let create () = ref []
	let to_list l = List.rev !l
end

module ListExt = struct
	type 'a t = 'a list
	let is_empty l =
		match l with
		| [] -> true
		| _ :: _ -> false
	let rec map_filter f l =
		match l with
		| [] -> []
		| x::xs ->
			match f x with
			| None -> map_filter f xs
			| Some y -> y :: map_filter f xs
	let rec opt_fold_left f s l =
		match l with
		| [] -> s
		| x::xs ->
			begin match s with
			| None -> None
			| Some sv -> opt_fold_left f (f x sv) xs
			end
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
	let map_f a f =
		match a with
		| None -> None
		| Some av -> Some (f av)
end
