(* File: analysis.ml *)

module VarSet = Set.Make(Midlang.Variable)

module LiveVarTransfer : (Dataflow.BackwardsTransfer with type t = VarSet.t) = struct
	type t = VarSet.t

	let exit_data = VarSet.empty
	
	let instr instr a =
		match instr.Midlang.ins_kind with
		| Midlang.IConstBool(rv, _) | Midlang.IConstInt(rv, _) 
		| Midlang.IConstFloat(rv, _) | Midlang.IConstVec(rv, _, _) 
		| Midlang.IConstMat(rv, _, _, _) ->
			VarSet.remove rv a
		| Midlang.IMov(rv, rs) | Midlang.IUnOp(rv, rs, _) 
		| Midlang.ISwizzle(rv, rs, _) | Midlang.ITex(rv, rs, _) -> 
			VarSet.add rs (VarSet.remove rv a)
		| Midlang.IBinOp(rv, rs1, rs2, _) ->
			VarSet.add rs1 (VarSet.add rs2 (VarSet.remove rv a))
		| Midlang.IRet rs -> VarSet.add rs a
end

module LiveVar = Dataflow.BackwardsDataflow(LiveVarTransfer)
