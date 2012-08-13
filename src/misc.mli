(* File: misc.mli *)

exception InternalError

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
	val add : 'a t -> 'a -> unit
	val create : unit -> 'a t
	val iter   : ('a -> unit) -> 'a t -> unit
	val of_list : 'a list -> 'a t
	val to_list : 'a t -> 'a list
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
end

module Opt : sig
	type 'a t = 'a option
	val bind  : 'a t -> ('a -> 'b t) -> 'b t
	val iter  : 'a t -> ('a -> unit) -> unit
	val map   : ('a -> 'b) -> 'a t -> 'b t
	val map_f : 'a t -> ('a -> 'b) -> 'b t
	val value : 'a t -> 'a
end
