(* File: misc.mli *)

exception Internal_error of string

val fast_pow : ('a -> 'a -> 'a) -> ('a -> 'a) -> 'a -> 'a -> int -> 'a

module ArrayMat : sig
	type t = float array array
	val add : t -> t -> t
	val div_scalar : t -> float -> t
	val equal      : t -> t -> bool
	val identity   : int -> t
	val mod_scalar : t -> float -> t
	val mul_matrix : t -> t -> t
	val mul_scalar : t -> float -> t
	val mul_vector : t -> float array -> float array
	val neg : t -> t
	val rcp : t -> t
	val scalar_mul : float -> t -> t
	val sub : t -> t -> t
end

module ArrayVec : sig
	type t = float array
	val add        : t -> t -> t
	val div_comp   : t -> t -> t
	val div_scalar : t -> float -> t
	val dot        : t -> t -> float
	val equal      : t -> t -> bool
	val min_comp   : t -> t -> t
	val mod_comp   : t -> t -> t
	val mod_scalar : t -> float -> t
	val mul_comp   : t -> t -> t
	val mul_scalar : t -> float -> t
	val neg        : t -> t
	val pow_comp   : t -> t -> t
	val pow_scalar : t -> float -> t
	val rcp        : t -> t
	val scalar_div : float -> t -> t
	val scalar_mod : float -> t -> t
	val scalar_mul : float -> t -> t
	val sub        : t -> t -> t
end

module ByteArray : sig
	type t
	type mode =
	| LittleEndian

	val append_byte  : t -> int -> unit
	val append_short : t -> int -> unit
	val append_int   : t -> int -> unit
	val append_int64 : t -> int64 -> unit
	val create : mode -> t
	val to_int_list : t -> int list
end

module Base64 : sig
	type t = string
	val of_byte_array : ByteArray.t -> t
end

module Char : sig
	val is_upper : char -> bool
end

module Dim : sig
	type dim =
	| Dim2
	| Dim3
	| Dim4
	val int_of_dim : dim -> int
	val range_of_dim : dim -> int list
end

module Fresh : sig
	type t
	val create : unit -> t
	val next : t -> int
end

module Int : sig
	type t = int
	val compare : t -> t -> int
end

module ImpList : sig
	type 'a t
	val add      : 'a t -> 'a -> unit
	val create   : unit -> 'a t
	val is_empty : 'a t -> bool
	val iter     : ('a -> unit) -> 'a t -> unit
	val of_list  : 'a list -> 'a t
	val to_list  : 'a t -> 'a list
end

module IO : sig
	val try_close_out : out_channel -> unit
	val with_out_channel : string -> bool -> (out_channel -> 'a) -> 'a
end

module ListExt : sig
	type 'a t = 'a list
	val concat_map    : ('a -> 'b list) -> 'a list -> 'b list
	val is_empty      : 'a list -> bool
	val map_filter    : ('a -> 'b option) -> 'a list -> 'b list
	val opt_fold_left : ('a -> 'b -> 'b option) -> 'b option -> 'a list -> 'b option
	val to_string     : ('a -> string) -> 'a list -> string
end

module Opt : sig
	type 'a t = 'a option
	val bind  : 'a t -> ('a -> 'b t) -> 'b t
	val iter  : 'a t -> ('a -> unit) -> unit
	val map   : ('a -> 'b) -> 'a t -> 'b t
	val map_f : 'a t -> ('a -> 'b) -> 'b t
	val value : 'a t -> 'a
end
