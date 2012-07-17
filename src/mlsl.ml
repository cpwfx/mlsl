
let check_types = false

let _ =
	if Array.length Sys.argv <= 1 then
		print_endline "Usage: mlsl <file>"
	else begin
		begin try match Parser.parse Sys.argv.(1) with
		| Some td_list ->
			if check_types then
				TypeCheck.check td_list
			else
				TypeCheck.fast_check td_list;
			if Errors.ok () then
				print_endline "OK"
			else ()
		| None -> ()
		with
		| Parsing.Parse_error -> ()
		end;
		Errors.print_status ()
	end
