
let check_types = false

let _ =
	Builtin.init()

let _ =
	if Array.length Sys.argv <= 1 then
		print_endline "Usage: mlsl <file>"
	else begin
		for i = 1 to Array.length Sys.argv - 1 do
			begin try match Parser.parse Sys.argv.(i) with
			| Some td_list ->
				if check_types then
					TypeCheck.check td_list
				else
					TypeCheck.fast_check td_list;
				if Errors.ok () then
					Eval.eval_all td_list
				else ()
			| None -> ()
			with
			| Parsing.Parse_error -> ()
			end;
		done;
		Final.perform_actions ();
		Errors.print_status ()
	end
