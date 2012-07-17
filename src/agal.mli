(* File: agal.mli *)

type shader =
	{ sh_name : string
	}

val build : Midlang.shader -> shader option

val optimize : shader -> shader

val write : shader -> unit -> unit
