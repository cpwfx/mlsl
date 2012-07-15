
let _ =
	if Array.length Sys.argv <= 1 then
		print_endline "Usage: mlsl <file>"
	else begin
		begin try match Parser.parse Sys.argv.(1) with
		| Some td_list ->
			TypeCheck.check td_list;
			if Errors.ok () then
				print_endline "OK"
			else ()
		| None -> ()
		with
		| Parsing.Parse_error -> ()
		end;
		Errors.print_status ()
	end
