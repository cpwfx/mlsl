(* File: misc.mli *)

module Int : sig
	type t = int
	val compare : t -> t -> int
end

module Opt : sig
	type 'a t = 'a option
	val bind : 'a t -> ('a -> 'b t) -> 'b t
	val iter : 'a t -> ('a -> unit) -> unit
end
