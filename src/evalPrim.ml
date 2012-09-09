(* File: evalPrim.ml *)

open Misc.Dim

exception EvalPrim_exception

let value_kind pos value =
	match value.TopDef.v_kind with
	| None ->
		Errors.error_p pos "Ivalid fixpoint: This value (defined at %s) was used during its evaluation."
			(Errors.string_of_pos value.TopDef.v_pos);
		raise EvalPrim_exception
	| Some kind -> kind

let get_component pos value comp =
	match comp with
	| MlslAst.Swizzle.X ->
		begin match value_kind pos value with
		| TopDef.VInt n   -> float_of_int n
		| TopDef.VFloat v -> v
		| TopDef.VVec(_, v) -> v.(0)
		| kind ->
			Errors.error_p pos "Can not get component x from %s defined at %s"
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos value.TopDef.v_pos);
			raise EvalPrim_exception
		end
	| MlslAst.Swizzle.Y ->
		begin match value_kind pos value with
		| TopDef.VVec(_, v) -> v.(1)
		| kind ->
			Errors.error_p pos "Can not get component y from %s defined at %s"
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos value.TopDef.v_pos);
			raise EvalPrim_exception
		end
	| MlslAst.Swizzle.Z ->
		begin match value_kind pos value with
		| TopDef.VVec(dim, v) when int_of_dim dim >= 3 -> v.(2)
		| kind ->
			Errors.error_p pos "Can not get component z from %s defined at %s"
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos value.TopDef.v_pos);
			raise EvalPrim_exception
		end
	| MlslAst.Swizzle.W ->
		begin match value_kind pos value with
		| TopDef.VVec(dim, v) when int_of_dim dim >= 4 -> v.(3)
		| kind ->
			Errors.error_p pos "Can not get component w from %s defined at %s"
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos value.TopDef.v_pos);
			raise EvalPrim_exception
		end

let eval_swizzle pos v swizzle =
	match swizzle with
	| MlslAst.Swizzle.S1 c ->
		TopDef.make_value pos (TopDef.VFloat (get_component pos v c))
	| MlslAst.Swizzle.S2(c1, c2) ->
		TopDef.make_value pos (TopDef.VVec(Dim2, 
			[| get_component pos v c1; get_component pos v c2 |]))
	| MlslAst.Swizzle.S3(c1, c2, c3) ->
		TopDef.make_value pos (TopDef.VVec(Dim3,
			[| get_component pos v c1; get_component pos v c2; get_component pos v c3 |]))
	| MlslAst.Swizzle.S4(c1, c2, c3, c4) ->
		TopDef.make_value pos (TopDef.VVec(Dim4,
			[| get_component pos v c1; get_component pos v c2
			;  get_component pos v c3; get_component pos v c4
			|] ))

(* ========================================================================= *)

let eval_binop pos op v1 v2 =
	match op with
	| MlslAst.BOEq ->
		Errors.error_p pos "Unimpleneted: eval_binop BOEq";
		raise EvalPrim_exception
	| MlslAst.BONeq ->
		Errors.error_p pos "Unimpleneted: eval_binop BONeq";
		raise EvalPrim_exception
	| MlslAst.BOLe ->
		Errors.error_p pos "Unimpleneted: eval_binop BOLe";
		raise EvalPrim_exception
	| MlslAst.BOLt ->
		Errors.error_p pos "Unimpleneted: eval_binop BOLt";
		raise EvalPrim_exception
	| MlslAst.BOGe ->
		Errors.error_p pos "Unimpleneted: eval_binop BOGe";
		raise EvalPrim_exception
	| MlslAst.BOGt ->
		Errors.error_p pos "Unimpleneted: eval_binop BOGt";
		raise EvalPrim_exception
	| MlslAst.BOAdd ->
		begin match value_kind pos v1, value_kind pos v2 with
		| TopDef.VInt n1, TopDef.VInt n2 -> 
			TopDef.make_value pos (TopDef.VInt (n1 + n2))
		| TopDef.VInt n1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (float_of_int n1 +. f2))
		| TopDef.VFloat f1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VFloat (f1 +. float_of_int n2))
		| TopDef.VFloat f1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (f1 +. f2))
		| TopDef.VVec(dim1, vv1), TopDef.VVec(dim2, vv2) when dim1 = dim2 ->
			TopDef.make_value pos (TopDef.VVec(dim1, Misc.ArrayVec.add vv1 vv2))
		| TopDef.VMat(d1, d2, vm1), TopDef.VMat(d1', d2', vm2) when d1 = d1' && d2 = d2' ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.add vm1 vm2))
		| kind1, kind2 ->
			Errors.error_p pos "Addition for %s and %s is not defined"
				(TopDef.string_of_value_kind kind1) (TopDef.string_of_value_kind kind2);
			raise EvalPrim_exception
		end
	| MlslAst.BOSub ->
		begin match value_kind pos v1, value_kind pos v2 with
		| TopDef.VInt n1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VInt (n1 - n2))
		| TopDef.VInt n1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (float_of_int n1 -. f2))
		| TopDef.VFloat f1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VFloat (f1 -. float_of_int n2))
		| TopDef.VFloat f1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (f1 -. f2))
		| TopDef.VVec(dim1, vv1), TopDef.VVec(dim2, vv2) when dim1 = dim2 ->
			TopDef.make_value pos (TopDef.VVec(dim1, Misc.ArrayVec.sub vv1 vv2))
		| TopDef.VMat(d1, d2, vm1), TopDef.VMat(d1', d2', vm2) when d1 = d1' && d2 = d2' ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.sub vm1 vm2))
		| kind1, kind2 ->
			Errors.error_p pos "Subtraction for %s and %s is not defined"
				(TopDef.string_of_value_kind kind1) (TopDef.string_of_value_kind kind2);
			raise EvalPrim_exception
		end
	| MlslAst.BOMul ->
		begin match value_kind pos v1, value_kind pos v2 with
		| TopDef.VInt n1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VInt (n1 * n2))
		| TopDef.VInt n1, TopDef.VFloat f1 ->
			TopDef.make_value pos (TopDef.VFloat (float_of_int n1 *. f1))
		| TopDef.VInt n1, TopDef.VVec(dim, vv) ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.scalar_mul (float_of_int n1) vv))
		| TopDef.VInt n1, TopDef.VMat(d1, d2, vm) ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.scalar_mul (float_of_int n1) vm))
		| TopDef.VFloat f1, TopDef.VInt n2 -> 
			TopDef.make_value pos (TopDef.VFloat (f1 *. float_of_int n2))
		| TopDef.VFloat f1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (f1 *. f2))
		| TopDef.VFloat f1, TopDef.VVec(dim, vv) ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.scalar_mul f1 vv))
		| TopDef.VFloat f1, TopDef.VMat(d1, d2, vm) ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.scalar_mul f1 vm))
		| TopDef.VVec(dim, vv), TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.mul_scalar vv (float_of_int n2)))
		| TopDef.VVec(dim, vv), TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.mul_scalar vv f2))
		| TopDef.VVec(dim1, vv1), TopDef.VVec(dim2, vv2) when dim1 = dim2 ->
			TopDef.make_value pos (TopDef.VVec(dim1, Misc.ArrayVec.mul_comp vv1 vv2))
		| TopDef.VMat(d1, d2, vm), TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VMat(d1, d1, Misc.ArrayMat.mul_scalar vm (float_of_int n2)))
		| TopDef.VMat(d1, d2, vm), TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.mul_scalar vm f2))
		| TopDef.VMat(d1, d2, vm), TopDef.VVec(dim, vv) when d2 = dim ->
			TopDef.make_value pos (TopDef.VVec(d1, Misc.ArrayMat.mul_vector vm vv))
		| TopDef.VMat(d1, d2, vm1), TopDef.VMat(d2', d3, vm2) when d2 = d2' ->
			TopDef.make_value pos (TopDef.VMat(d1, d3, Misc.ArrayMat.mul_matrix vm1 vm2))
		| kind1, kind2 ->
			Errors.error_p pos "Multiplication for %s and %s is not defined"
				(TopDef.string_of_value_kind kind1) (TopDef.string_of_value_kind kind2);
			raise EvalPrim_exception
		end
	| MlslAst.BODiv ->
		begin match value_kind pos v1, value_kind pos v2 with
		| TopDef.VInt n1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VInt (n1 / n2))
		| TopDef.VInt n1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (float_of_int n1 /. f2))
		| TopDef.VInt n1, TopDef.VVec(dim, vv) ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.scalar_div (float_of_int n1) vv))
		| TopDef.VFloat f1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VFloat (f1 /. float_of_int n2))
		| TopDef.VFloat f1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (f1 /. f2))
		| TopDef.VFloat f1, TopDef.VVec(dim, vv) ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.scalar_div f1 vv))
		| TopDef.VVec(dim, vv), TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.div_scalar vv (float_of_int n2)))
		| TopDef.VVec(dim, vv), TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.div_scalar vv f2))
		| TopDef.VVec(dim1, vv1), TopDef.VVec(dim2, vv2) when dim1 = dim2 ->
			TopDef.make_value pos (TopDef.VVec(dim1, Misc.ArrayVec.div_comp vv1 vv2))
		| TopDef.VMat(d1, d2, vm), TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.div_scalar vm (float_of_int n2)))
		| TopDef.VMat(d1, d2, vm), TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.div_scalar vm f2))
		| kind1, kind2 ->
			Errors.error_p pos "Division for %s and %s is not defined"
				(TopDef.string_of_value_kind kind1) (TopDef.string_of_value_kind kind2);
			raise EvalPrim_exception
		end
	| MlslAst.BOMod ->
		begin match value_kind pos v1, value_kind pos v2 with
		| TopDef.VInt n1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VInt (n1 mod n2))
		| TopDef.VInt n1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (mod_float (float_of_int n1) f2))
		| TopDef.VInt n1, TopDef.VVec(dim, vv) ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.scalar_mod (float_of_int n1) vv))
		| TopDef.VFloat f1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VFloat (mod_float f1 (float_of_int n2)))
		| TopDef.VFloat f1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (mod_float f1 f2))
		| TopDef.VFloat f1, TopDef.VVec(dim, vv) ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.scalar_mod f1 vv))
		| TopDef.VVec(dim, vv), TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.mod_scalar vv (float_of_int n2)))
		| TopDef.VVec(dim, vv), TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.mod_scalar vv f2))
		| TopDef.VVec(dim1, vv1), TopDef.VVec(dim2, vv2) when dim1 = dim2 ->
			TopDef.make_value pos (TopDef.VVec(dim1, Misc.ArrayVec.mod_comp vv1 vv2))
		| TopDef.VMat(d1, d2, vm), TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.mod_scalar vm (float_of_int n2)))
		| TopDef.VMat(d1, d2, vm), TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.mod_scalar vm f2))
		| kind1, kind2 ->
			Errors.error_p pos "Modulo for %s and %s is not defined"
				(TopDef.string_of_value_kind kind1) (TopDef.string_of_value_kind kind2);
			raise EvalPrim_exception
		end
	| MlslAst.BODot ->
		begin match value_kind pos v1, value_kind pos v2 with
		| TopDef.VInt n1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VInt(n1 * n2))
		| TopDef.VInt n1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat(float_of_int n1 *. f2))
		| TopDef.VFloat f1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VFloat(f1 *. float_of_int n2))
		| TopDef.VFloat f1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat(f1 *. f2))
		| TopDef.VVec(dim1, vv1), TopDef.VVec(dim2, vv2) when dim1 = dim2 ->
			TopDef.make_value pos (TopDef.VFloat (Misc.ArrayVec.dot vv1 vv2))
		| kind1, kind2 ->
			Errors.error_p pos "Dot product of %s and %s is not defined"
				(TopDef.string_of_value_kind kind1) (TopDef.string_of_value_kind kind2);
			raise EvalPrim_exception
		end
	| MlslAst.BOCross ->
		begin match value_kind pos v1, value_kind pos v2 with
		| TopDef.VVec(Dim2, vv1), TopDef.VVec(Dim2, vv2) ->
			TopDef.make_value pos (TopDef.VFloat
				(vv1.(0) *. vv2.(1) -. vv1.(1) *. vv2.(0)))
		| TopDef.VVec(Dim3, vv1), TopDef.VVec(Dim3, vv2) ->
			TopDef.make_value pos (TopDef.VVec(Dim3,
				[| vv1.(1) *. vv2.(2) -. vv1.(2) *. vv2.(1)
				;  vv1.(2) *. vv2.(0) -. vv1.(0) *. vv2.(2)
				;  vv1.(0) *. vv2.(1) -. vv1.(1) *. vv2.(0)
				|]))
		| kind1, kind2 ->
			Errors.error_p pos "Cross product of %s and %s is not defined"
				(TopDef.string_of_value_kind kind1) (TopDef.string_of_value_kind kind2);
			raise EvalPrim_exception
		end
	| MlslAst.BOPow ->
		begin match value_kind pos v1, value_kind pos v2 with
		| TopDef.VInt n1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VFloat (Misc.fast_pow ( *. ) (fun x -> 1.0 /. x) 1.0 
				(float_of_int n1) n2))
		| TopDef.VInt n1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (float_of_int n1 ** f2))
		| TopDef.VFloat f1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VFloat (Misc.fast_pow ( *. ) (fun x -> 1.0 /. x) 1.0 f1 n2))
		| TopDef.VFloat f1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (f1 ** f2))
		| TopDef.VVec(dim, vv), TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.fast_pow Misc.ArrayVec.mul_comp 
				Misc.ArrayVec.rcp (Array.create (int_of_dim dim) 1.0) vv n2))
		| TopDef.VVec(dim, vv), TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VVec(dim, Misc.ArrayVec.pow_scalar vv f2))
		| TopDef.VVec(dim1, vv1), TopDef.VVec(dim2, vv2) when dim1 = dim2 ->
			TopDef.make_value pos (TopDef.VVec(dim1, Misc.ArrayVec.pow_comp vv1 vv2))
		| TopDef.VMat(d1, d2, vm), TopDef.VInt n2 when d1 = d2 ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.fast_pow Misc.ArrayMat.mul_matrix
				Misc.ArrayMat.rcp (Misc.ArrayMat.identity (int_of_dim d1)) vm n2))
		| kind1, kind2 ->
			Errors.error_p pos "Power for %s and %s is not defined"
				(TopDef.string_of_value_kind kind1) (TopDef.string_of_value_kind kind2);
			raise EvalPrim_exception
		end
	| MlslAst.BOMin ->
		begin match value_kind pos v1, value_kind pos v2 with
		| TopDef.VInt n1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VInt (min n1 n2))
		| TopDef.VInt n1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (min (float_of_int n1) f2))
		| TopDef.VFloat f1, TopDef.VInt n2 ->
			TopDef.make_value pos (TopDef.VFloat (min f1 (float_of_int n2)))
		| TopDef.VFloat f1, TopDef.VFloat f2 ->
			TopDef.make_value pos (TopDef.VFloat (min f1 f2))
		| TopDef.VVec(dim1, vv1), TopDef.VVec(dim2, vv2) when dim1 = dim2 ->
			TopDef.make_value pos (TopDef.VVec(dim1, Misc.ArrayVec.min_comp vv1 vv2))
		| kind1, kind2 ->
			Errors.error_p pos "Minimum for %s and %s is not defined"
				(TopDef.string_of_value_kind kind1) (TopDef.string_of_value_kind kind2);
			raise EvalPrim_exception
		end

(* ========================================================================= *)

let eval_unop pos op v =
	match op with
	| MlslAst.UONeg ->
		begin match value_kind pos v with
		| TopDef.VInt n ->
			TopDef.make_value pos (TopDef.VInt (-n))
		| TopDef.VFloat f ->
			TopDef.make_value pos (TopDef.VFloat (-.f))
		| TopDef.VVec(d, v) ->
			TopDef.make_value pos (TopDef.VVec(d, Misc.ArrayVec.neg v))
		| TopDef.VMat(d1, d2, m) ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, Misc.ArrayMat.neg m))
		| kind ->
			Errors.error_p pos "Unary minus for %s is not defined"
				(TopDef.string_of_value_kind kind);
			raise EvalPrim_exception
		end
	| MlslAst.UOPlus ->
		begin match value_kind pos v with
		| TopDef.VInt n ->
			TopDef.make_value pos (TopDef.VInt n)
		| TopDef.VFloat f ->
			TopDef.make_value pos (TopDef.VFloat f)
		| TopDef.VVec(d, v) ->
			TopDef.make_value pos (TopDef.VVec(d, v))
		| TopDef.VMat(d1, d2, m) ->
			TopDef.make_value pos (TopDef.VMat(d1, d2, m))
		| kind ->
			Errors.error_p pos "Unary plus for %s is not defined"
				(TopDef.string_of_value_kind kind);
			raise EvalPrim_exception
		end

(* ========================================================================= *)

let with_exn ex f =
	try
		f ()
	with
	| EvalPrim_exception -> raise ex
