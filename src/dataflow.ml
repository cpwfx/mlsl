(* File: dataflow.ml *)

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

module ForwardsDataflow(T : ForwardsTransfer) : (Analysis with type t = T.t) = struct
	type t = T.t

	let entry_tab = Hashtbl.create 32
	let exit_tab  = Hashtbl.create 32

	let rec compute_loop last code =
		match code with
		| [] -> ()
		| instr :: code ->
			Hashtbl.replace entry_tab instr.Midlang.ins_id last;
			let next = T.instr instr last in
			Hashtbl.replace exit_tab instr.Midlang.ins_id next;
			compute_loop next code

	let compute code =
		compute_loop T.start_data code

	let clear () =
		Hashtbl.clear entry_tab;
		Hashtbl.clear exit_tab

	let ientry id =
		Hashtbl.find entry_tab id

	let iexit id =
		Hashtbl.find exit_tab id

	let entry instr = ientry (instr.Midlang.ins_id)
	let exit  instr = iexit (instr.Midlang.ins_id)
end

module type BackwardsTransfer = sig
	type t
	val exit_data : t
	val instr : Midlang.instr -> t -> t
end

module BackwardsDataflow(T : BackwardsTransfer) : (Analysis with type t = T.t) = struct
	type t = T.t

	let entry_tab = Hashtbl.create 32
	let exit_tab  = Hashtbl.create 32

	let rec compute_loop last code =
		match code with
		| [] -> ()
		| instr :: code ->
			Hashtbl.replace exit_tab instr.Midlang.ins_id last;
			let next = T.instr instr last in
			Hashtbl.replace entry_tab instr.Midlang.ins_id next;
			compute_loop next code

	let compute code =
		compute_loop T.exit_data (List.rev code)

	let clear () =
		Hashtbl.clear entry_tab;
		Hashtbl.clear exit_tab

	let ientry id =
		Hashtbl.find entry_tab id

	let iexit id =
		Hashtbl.find exit_tab id

	let entry instr = ientry (instr.Midlang.ins_id)
	let exit  instr = iexit (instr.Midlang.ins_id)
end
