(* File: analysis.mli *)

module LiveVar : (Dataflow.Analysis with type t = Set.Make(Midlang.Variable).t)
