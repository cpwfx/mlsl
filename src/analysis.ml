(* File: analysis.ml *)

module VarSet = Set.Make(Midlang.Variable)

module LiveVarTransfer : (Dataflow.BackwardsTransfer with type t = VarSet.t) = struct
	type t = VarSet.t

	let exit_data = VarSet.empty
	
	let instr instr a =
		match instr.Midlang.ins_kind with
		| Midlang.IMov(rv, rs) -> VarSet.add rs (VarSet.remove rv a)
		| Midlang.IMulFF(rv, rs1, rs2) | Midlang.IMulMV(rv, rs1, rs2, _, _) 
		| Midlang.IMulVF(rv, rs1, rs2, _) ->
			VarSet.add rs1 (VarSet.add rs2 (VarSet.remove rv a))
		| Midlang.IRet rs -> VarSet.add rs a
end

module LiveVar = Dataflow.BackwardsDataflow(LiveVarTransfer)
