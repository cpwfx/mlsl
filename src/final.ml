(* File: final.ml *)

let actions = Misc.ImpList.create ()

let add_action f =
	Misc.ImpList.add actions f

let perform_actions () =
	Misc.ImpList.iter (fun f -> f ()) actions
