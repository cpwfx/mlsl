(* File: dataflow.mli *)

module type Analysis = sig
	type t
	val compute : Midlang.instr list -> unit
	val clear   : unit -> unit
	val ientry  : int -> t
	val iexit   : int -> t
	val entry   : Midlang.instr -> t
	val exit    : Midlang.instr -> t
end

module type ForwardsTransfer = sig
	type t
	val start_data : t
	val instr : Midlang.instr -> t -> t
end

module ForwardsDataflow(T : ForwardsTransfer) : (Analysis with type t = T.t)

module type BackwardsTransfer = sig
	type t
	val exit_data : t
	val instr : Midlang.instr -> t -> t
end

module BackwardsDataflow(T : BackwardsTransfer) : (Analysis with type t = T.t)
