(* File: misc.ml *)

exception Internal_error of string

let fast_pow mul rcp id x n =
	if n = 0 then id
	else
	let rec fast_pow x n =
		if n = 1 then x
		else if (n land 1) = 0 then
			fast_pow (mul x x) (n lsr 1)
		else
			mul x (fast_pow (mul x x) (n lsr 1))
	in if n > 0 then fast_pow x n
	else rcp (fast_pow x (-n))

module ArrayMat = struct
	type t = float array array
	let map2 f m1 m2 =
		Array.init (min (Array.length m1) (Array.length m2)) (fun i ->
			Array.init (min (Array.length m1.(i)) (Array.length m2.(i))) (fun j ->
				f m1.(i).(j) m2.(i).(j) ))
	let map f m = Array.map (Array.map f) m
	let height m col =
		let rec h_loop row =
			if row >= Array.length m then row
			else if col >= Array.length m.(row) then row
			else h_loop (row + 1)
		in h_loop 0
	let square_size m =
		let rec square_size row ml =
			if row >= Array.length m then row
			else if row >= ml then ml
			else square_size (row + 1) (min ml (Array.length m.(row)))
		in if Array.length m <= 0 then 0
		else square_size 0 (Array.length m.(0))

	let add m1 m2 = map2 (+.) m1 m2
	let div_scalar v s = map (fun x -> x /. s) v
	let equal m1 m2 = m1 = m2
	let identity size = Array.init size (fun i -> Array.init size (fun j -> if i = j then 1.0 else 0.0))
	let mul_matrix m1 m2 =
		let size2 = if Array.length m2 > 0 then Array.length m2.(0) else 0 in
		Array.init (Array.length m1) (fun i ->
			Array.init size2 (fun j ->
				let sum_size = min (Array.length m1.(i)) (height m2 j) in
				Array.fold_left (+.) 0.0 (Array.init sum_size (fun k -> 
					m1.(i).(k) *. m2.(k).(j)))))
				
	let mod_scalar v s = map (fun x -> mod_float x s) v
	let mul_scalar v s = map (fun x -> x *. s) v
	let mul_vector m v = Array.map
		(fun row -> Array.fold_left (+.) 0.0 
			(Array.init (min (Array.length row) (Array.length v)) 
				(fun i -> row.(i) *. v.(i)))) m
	let neg m = map (~-.) m
	let scalar_mul s v = map (fun x -> s *. x) v
	let sub m1 m2 = map2 (-.) m1 m2

	let gauss_rcp size mat =
		let sq_mat f = Array.init size (fun i -> Array.init size (f i)) in
		let rec max_col mat j row v col =
			if j > row then (v, col)
			else if abs_float mat.(row).(j) > abs_float v then
				max_col mat (j + 1) row mat.(row).(j) j
			else max_col mat (j + 1) row v col
		in
		let rec gauss1 row mat res =
			if row < 0 then (mat, res)
			else
				let (v, col) = max_col mat 0 row 0.0 0 in
				let m1 = sq_mat (fun i j ->
					if i = col then (if j = row then 1.0 /. v else 0.0)
					else if i = row then (if j = col then 1.0 else 0.0)
					else if i = j then 1.0 else 0.0)
				in
				let mat' = mul_matrix mat m1 in
				let m2   = sq_mat (fun i j ->
					if i = j then 1.0
					else if i = row && j < row then -. mat'.(row).(j)
					else 0.0)
				in gauss1 (row - 1) (mul_matrix mat' m2) (mul_matrix res (mul_matrix m1 m2))
		in
		let rec gauss2 row mat res =
			if row >= size then res
			else
				let m = sq_mat (fun i j ->
					if i = j then 1.0
					else if i = row then -. mat.(row).(j)
					else 0.0)
				in gauss2 (row + 1) (mul_matrix mat m) (mul_matrix res m)
		in
		let (mat1, res1) = gauss1 (size-1) mat (identity size) in
		gauss2 0 mat1 res1
	let rcp m =
		let size = square_size m in
		gauss_rcp size m
end

module ArrayVec = struct
	type t = float array
	let map2 f v1 v2 =
		Array.init (min (Array.length v1) (Array.length v2))
			(fun i -> f v1.(i) v2.(i))
	let map = Array.map

	let add v1 v2 = map2 (+.) v1 v2
	let div_comp v1 v2 = map2 ( /. ) v1 v2
	let div_scalar v s = map (fun x -> x /. s) v
	let dot v1 v2      = Array.fold_left (+.) 0.0 (map2 ( *. ) v1 v2)
	let equal v1 v2    = v1 = v2
	let min_comp v1 v2 = map2 min v1 v2
	let mod_comp v1 v2 = map2 mod_float v1 v2
	let mod_scalar v s = map (fun x -> mod_float x s) v
	let mul_comp v1 v2 = map2 ( *. ) v1 v2
	let mul_scalar v s = map (fun x -> x *. s) v
	let neg v = map (~-.) v
	let pow_comp v1 v2 = map2 ( ** ) v1 v2
	let pow_scalar v s = map (fun x -> x ** s) v
	let rcp v = map (fun x -> 1.0 /. x) v
	let scalar_div s v = map (fun x -> s /. x) v
	let scalar_mod s v = map (mod_float s) v
	let scalar_mul s v = map (fun x -> s *. x) v
	let sub v1 v2 = map2 (-.) v1 v2
end

module Char = struct
	let is_upper c = c >= 'A' && c <= 'Z'
end

module Dim = struct
	type dim =
	| Dim2
	| Dim3
	| Dim4
	let int_of_dim dim =
		match dim with
		| Dim2 -> 2
		| Dim3 -> 3
		| Dim4 -> 4
	let range_of_dim dim =
		match dim with
		| Dim2 -> [ 0; 1 ]
		| Dim3 -> [ 0; 1; 2 ]
		| Dim4 -> [ 0; 1; 2; 3 ]
end

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
	let is_empty l =
		match !l with
		| [] -> true
		| _ :: _ -> false
	let iter f l  = List.iter f (List.rev !l)
	let of_list l = ref (List.rev l)
	let to_list l = List.rev !l
end

module IO = struct
	let try_close_out chan =
		try close_out chan with
		| Sys_error _ -> ()
	let with_out_channel path binary f =
		let chan = if binary then open_out_bin path else open_out path in
		try let res = f chan in try_close_out chan; res with
		| ex ->
			try_close_out chan;
			raise ex
end

module ListExt = struct
	type 'a t = 'a list
	let rec concat_map f l =
		match l with
		| [] -> []
		| x::xs -> f x @ concat_map f xs
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
	let to_string f l =
		match l with
		| [] -> "[]"
		| x :: xs ->
			let rec body_to_string l =
				match l with
				| [] -> ""
				| x :: xs -> "; " ^ f x ^ body_to_string xs
			in "[ " ^ f x ^ body_to_string xs ^ " ]"
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
	let map f a =
		match a with
		| None -> None
		| Some av -> Some (f av)
	let map_f a f =
		match a with
		| None -> None
		| Some av -> Some (f av)
	let value a =
		match a with
		| None -> raise (Internal_error "Opt.value None")
		| Some av -> av
end
