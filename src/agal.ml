(* File: agal.ml *)

type shader =
	{ sh_name : string
	}

let build sh =
	Errors.error "Unimplemented: Agal.build.";
	None

(* TODO: better optimizer *)
let optimize sh = sh

let write sh () =
	Errors.error "Unimplemented: Agal.write."
