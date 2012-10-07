(* File: settings.ml *)

let check_types = ref false

let concat_path dir file =
	if Filename.is_relative file then
		Filename.concat dir file
	else file

let mlsl_lib_path =
	try Sys.getenv "MLSL_LIB_PATH" with
	| Not_found -> "/usr/local/lib/mlsl/"

let rec find_source libdirs source =
	match libdirs with
	| [] ->
		Errors.error "Ivalid MLSL configuration: Can not find prelude library";
		raise Exit
	| libpath :: libdirs ->
		let path = concat_path libpath source in
		if Sys.file_exists path && not (Sys.is_directory path) then
			path
		else find_source libdirs source

let prelude_file =
	find_source [ mlsl_lib_path; "lib" ] "prelude.mlsl"
