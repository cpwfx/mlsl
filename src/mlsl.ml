
let check_types = false

let final_actions = ref []
let add_final_action f =
	final_actions := f :: !final_actions
let perform_final_actions () =
	List.iter (fun f -> f ()) (List.rev !final_actions)

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
				MlslAst.foreachShader td_list (fun td name definition ->
					Misc.Opt.iter (Midlang.unfold_shader name definition) (fun mprog ->
					let mprog_opt = Midlang.optimize mprog in
					Misc.Opt.iter (Agal.build mprog_opt) (fun aprog ->
					let aprog_opt = Agal.optimize aprog in
					Misc.Opt.iter (Agal.finalize aprog_opt) (fun aprog_fin ->
					add_final_action (Agal.write aprog_fin)
					))))
			else ()
		| None -> ()
		with
		| Parsing.Parse_error -> ()
		end;
		perform_final_actions ();
		Errors.print_status ()
	end
