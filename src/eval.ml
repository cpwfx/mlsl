(* File: eval.ml *)

open Misc.Dim

module StrMap = Map.Make(String)

exception Eval_exception

let credits = ref 1024

let target_func = ref (fun _ -> ())

let set_target_func f =
	target_func := f

let get_component pos value comp =
	match comp with
	| MlslAst.Swizzle.X ->
		begin match value.TopDef.v_kind with
		| TopDef.VInt n   -> float_of_int n
		| TopDef.VFloat v -> v
		| TopDef.VVec(_, v) -> v.(0)
		| kind ->
			Errors.error_p pos "Can not get component x from %s defined at %s"
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.Swizzle.Y ->
		begin match value.TopDef.v_kind with
		| TopDef.VVec(_, v) -> v.(1)
		| kind ->
			Errors.error_p pos "Can not get component y from %s defined at %s"
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.Swizzle.Z ->
		begin match value.TopDef.v_kind with
		| TopDef.VVec(dim, v) when int_of_dim dim >= 3 -> v.(2)
		| kind ->
			Errors.error_p pos "Can not get component z from %s defined at %s"
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.Swizzle.W ->
		begin match value.TopDef.v_kind with
		| TopDef.VVec(dim, v) when int_of_dim dim >= 4 -> v.(3)
		| kind ->
			Errors.error_p pos "Can not get component w from %s defined at %s"
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end

let eval_binop pos op v1 v2 =
	match op with
	| MlslAst.BOEq ->
		Errors.error_p pos "Unimpleneted: eval_binop BOEq";
		raise Eval_exception
	| MlslAst.BONeq ->
		Errors.error_p pos "Unimpleneted: eval_binop BONeq";
		raise Eval_exception
	| MlslAst.BOLe ->
		Errors.error_p pos "Unimpleneted: eval_binop BOLe";
		raise Eval_exception
	| MlslAst.BOLt ->
		Errors.error_p pos "Unimpleneted: eval_binop BOLt";
		raise Eval_exception
	| MlslAst.BOGe ->
		Errors.error_p pos "Unimpleneted: eval_binop BOGe";
		raise Eval_exception
	| MlslAst.BOGt ->
		Errors.error_p pos "Unimpleneted: eval_binop BOGt";
		raise Eval_exception
	| MlslAst.BOAdd ->
		begin match v1.TopDef.v_kind, v2.TopDef.v_kind with
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
			raise Eval_exception
		end
	| MlslAst.BOSub ->
		begin match v1.TopDef.v_kind, v2.TopDef.v_kind with
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
			raise Eval_exception
		end
	| MlslAst.BOMul ->
		begin match v1.TopDef.v_kind, v2.TopDef.v_kind with
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
			raise Eval_exception
		end
	| MlslAst.BODiv ->
		begin match v1.TopDef.v_kind, v2.TopDef.v_kind with
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
			raise Eval_exception
		end
	| MlslAst.BOMod ->
		begin match v1.TopDef.v_kind, v2.TopDef.v_kind with
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
			raise Eval_exception
		end
	| MlslAst.BODot ->
		begin match v1.TopDef.v_kind, v2.TopDef.v_kind with
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
			raise Eval_exception
		end
	| MlslAst.BOCross ->
		begin match v1.TopDef.v_kind, v2.TopDef.v_kind with
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
			raise Eval_exception
		end
	| MlslAst.BOPow ->
		begin match v1.TopDef.v_kind, v2.TopDef.v_kind with
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
			raise Eval_exception
		end
	| MlslAst.BOMin ->
		begin match v1.TopDef.v_kind, v2.TopDef.v_kind with
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
			raise Eval_exception
		end

let eval_unop pos op v =
	match op with
	| MlslAst.UONeg ->
		begin match v.TopDef.v_kind with
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
			raise Eval_exception
		end
	| MlslAst.UOPlus ->
		begin match v.TopDef.v_kind with
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
			raise Eval_exception
		end

let rec fix_value value =
	match value.TopDef.v_kind with
	| TopDef.VFixed r ->
		begin match !r with
		| Some v -> fix_value v
		| None ->
			Errors.error_p value.TopDef.v_pos 
				"Invalid fixpoint: This value was used during its evaluation.";
			raise Eval_exception
		end
	| _ -> value

let rec cast_value_to_type pos value typ =
	match typ with
	| MlslAst.TBool ->
		begin match value.TopDef.v_kind with
		| TopDef.VBool _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected bool."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TFloat ->
		begin match value.TopDef.v_kind with
		| TopDef.VInt n -> TopDef.make_value value.TopDef.v_pos (TopDef.VFloat (float_of_int n))
		| TopDef.VFloat _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected float."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TInt ->
		begin match value.TopDef.v_kind with
		| TopDef.VInt n -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected int."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TMat(d1, d2) ->
		begin match value.TopDef.v_kind with
		| TopDef.VMat(d1', d2', _) when d1 = d1' && d2 = d2' -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected mat%d%d."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind)
				(int_of_dim d1) (int_of_dim d2);
			raise Eval_exception
		end
	| MlslAst.TSampler2D ->
		begin match value.TopDef.v_kind with
		| TopDef.VSampler(_, tt) when tt.MlslAst.tt_typ = MlslAst.TSampler2D -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected sampler2D."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TSamplerCube -> 
		begin match value.TopDef.v_kind with
		| TopDef.VSampler(_, tt) when tt.MlslAst.tt_typ = MlslAst.TSamplerCube -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected samplerCube."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TUnit ->
		begin match value.TopDef.v_kind with
		| TopDef.VConstrU "()" -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected unit."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TVec d ->
		begin match value.TopDef.v_kind with
		| TopDef.VVec(d', _) when d = d' -> value
		| kind -> 
			Errors.error_p pos "Value defined at %s is a %s, but expected vec%d."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind)
				(int_of_dim d);
			raise Eval_exception
		end
	| MlslAst.TArrow _ ->
		begin match value.TopDef.v_kind with
		| TopDef.VFunc _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected function."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TPair(t1, t2) ->
		begin match value.TopDef.v_kind with
		| TopDef.VPair(v1, v2) ->
			TopDef.make_value value.TopDef.v_pos (TopDef.VPair
				(cast_value_to_type pos (fix_value v1) t1, cast_value_to_type pos (fix_value v2) t2) )
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected pair."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TRecord rd ->
		begin match value.TopDef.v_kind with
		| TopDef.VRecord rmap ->
			let rmap' = List.fold_left (fun rm (field, tp) ->
				try
					StrMap.add field (cast_value_to_type pos (fix_value (StrMap.find field rmap)) tp) rm
				with
				| Not_found ->
					Errors.error_p pos "Record defined at %s hasn't field %s."
						(Errors.string_of_pos value.TopDef.v_pos) field;
					raise Eval_exception
				) StrMap.empty rd
			in TopDef.make_value value.TopDef.v_pos (TopDef.VRecord rmap')
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected record."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TVertex _ | MlslAst.TVertexTop ->
		begin match value.TopDef.v_kind with
		| TopDef.VVertex _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected vertex program."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end
	| MlslAst.TFragment _ ->
		begin match value.TopDef.v_kind with
		| TopDef.VFragment _ -> value
		| kind ->
			Errors.error_p pos "Value defined at %s is a %s, but expected fragment program."
				(Errors.string_of_pos value.TopDef.v_pos) (TopDef.string_of_value_kind kind);
			raise Eval_exception
		end

let print_gamma gamma =
	StrMap.iter (fun x v ->
		Printf.printf "%s = %s\n" x (TopDef.string_of_value_kind v.TopDef.v_kind)
	) gamma;
	print_endline "==============================="

let rec bind_pattern gamma pat value =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny -> gamma
	| MlslAst.PVar x -> StrMap.add x value gamma
	| MlslAst.PTypedVar(x, tp) ->
		StrMap.add x (cast_value_to_type pat.MlslAst.p_pos value tp.MlslAst.tt_typ) gamma
	| MlslAst.PTrue ->
		begin match value.TopDef.v_kind with
		| TopDef.VBool true  -> gamma
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PFalse ->
		begin match value.TopDef.v_kind with
		| TopDef.VBool false -> gamma
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PPair(pat1, pat2) ->
		begin match value.TopDef.v_kind with
		| TopDef.VPair(v1, v2) ->
			let gamma1 = bind_pattern gamma pat1 (fix_value v1) in
			bind_pattern gamma1 pat2 (fix_value v2)
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PConstrU name ->
		begin match value.TopDef.v_kind with
		| TopDef.VConstrU name' when name = name' -> gamma
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PConstrP(name, p) ->
		begin match value.TopDef.v_kind with
		| TopDef.VConstrP(name', v) when name = name' -> bind_pattern gamma p (fix_value v)
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end

let rec try_bind_pattern gamma pat value =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny -> Some gamma
	| MlslAst.PVar x -> Some (StrMap.add x value gamma)
	| MlslAst.PTypedVar(x, tp) -> 
		Some (StrMap.add x (cast_value_to_type pat.MlslAst.p_pos value tp.MlslAst.tt_typ) gamma)
	| MlslAst.PTrue ->
		begin match value.TopDef.v_kind with
		| TopDef.VBool true -> Some gamma
		| _ -> None
		end
	| MlslAst.PFalse ->
		begin match value.TopDef.v_kind with
		| TopDef.VBool false -> Some gamma
		| _ -> None
		end
	| MlslAst.PPair(pat1, pat2) ->
		begin match value.TopDef.v_kind with
		| TopDef.VPair(v1, v2) ->
			Misc.Opt.bind (try_bind_pattern gamma pat1 (fix_value v1)) (fun gamma ->
				try_bind_pattern gamma pat2 (fix_value v2))
		| _ -> None
		end
	| MlslAst.PConstrU name ->
		begin match value.TopDef.v_kind with
		| TopDef.VConstrU name' when name = name' -> Some gamma
		| _ -> None
		end
	| MlslAst.PConstrP(name, pat) ->
		begin match value.TopDef.v_kind with
		| TopDef.VConstrP(name', v) when name = name' ->
			try_bind_pattern gamma pat (fix_value v)
		| _ -> None
		end

let rec fix_pattern_pre gamma pat =
	let pos = pat.MlslAst.p_pos in
	match pat.MlslAst.p_kind with
	| MlslAst.PAny -> (TopDef.make_value pos (TopDef.VFixed (ref None)), gamma)
	| MlslAst.PVar x | MlslAst.PTypedVar(x, _) ->
		let v = TopDef.make_value pos (TopDef.VFixed (ref None)) in
		(v, StrMap.add x v gamma)
	| MlslAst.PTrue | MlslAst.PFalse | MlslAst.PConstrU _ -> 
		(TopDef.make_value pos (TopDef.VFixed (ref None)), gamma)
	| MlslAst.PPair(pat1, pat2) ->
		let (v1, gamma1) = fix_pattern_pre gamma  pat1 in
		let (v2, gamma2) = fix_pattern_pre gamma1 pat2 in
		(TopDef.make_value pos (TopDef.VPair(v1, v2)), gamma2)
	| MlslAst.PConstrP(name, pat) ->
		let (v, gamma') = fix_pattern_pre gamma pat in
		(TopDef.make_value pos (TopDef.VConstrP(name, v)), gamma')

let rec fix_pattern_post pat value0 value =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny | MlslAst.PVar _ ->
		begin match value0.TopDef.v_kind with
		| TopDef.VFixed vr -> 
			vr := Some value;
			value
		| _ -> raise Misc.Internal_error
		end
	| MlslAst.PTypedVar(x, tp) ->
		begin match value0.TopDef.v_kind with
		| TopDef.VFixed vr -> 
			vr := Some (cast_value_to_type pat.MlslAst.p_pos value tp.MlslAst.tt_typ);
			value
		| _ -> raise Misc.Internal_error
		end
	| MlslAst.PTrue ->
		begin match value0.TopDef.v_kind with
		| TopDef.VFixed vr ->
			begin match value.TopDef.v_kind with
			| TopDef.VBool true ->
				vr := Some value;
				value
			| _ ->
				Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
					(Errors.string_of_pos value.TopDef.v_pos);
				raise Eval_exception
			end
		| _ -> raise Misc.Internal_error
		end
	| MlslAst.PFalse ->
		begin match value0.TopDef.v_kind with
		| TopDef.VFixed vr ->
			begin match value.TopDef.v_kind with
			| TopDef.VBool false ->
				vr := Some value;
				value
			| _ ->
				Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
					(Errors.string_of_pos value.TopDef.v_pos);
				raise Eval_exception
			end
		| _ -> raise Misc.Internal_error
		end
	| MlslAst.PPair(pat1, pat2) ->
		begin match value0.TopDef.v_kind with
		| TopDef.VPair(val10, val20) ->
			begin match value.TopDef.v_kind with
			| TopDef.VPair(val1, val2) ->
				let val1' = fix_pattern_post pat1 val10 (fix_value val1) in
				let val2' = fix_pattern_post pat2 val20 (fix_value val2) in
				TopDef.make_value value.TopDef.v_pos (TopDef.VPair(val1', val2'))
			| _ ->
				Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
					(Errors.string_of_pos value.TopDef.v_pos);
				raise Eval_exception
			end
		| _ -> raise Misc.Internal_error
		end
	| MlslAst.PConstrU name ->
		begin match value0.TopDef.v_kind with
		| TopDef.VFixed vr ->
			begin match value.TopDef.v_kind with
			| TopDef.VConstrU name' when name = name' ->
				vr := Some value;
				value
			| _ ->
				Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
					(Errors.string_of_pos value.TopDef.v_pos);
				raise Eval_exception
			end
		| _ -> raise Misc.Internal_error
		end
	| MlslAst.PConstrP(name, pat') ->
		begin match value0.TopDef.v_kind with
		| TopDef.VConstrP(_, val0) ->
			begin match value.TopDef.v_kind with
			| TopDef.VConstrP(name', val1) when name = name' ->
				let val1' = fix_pattern_post pat' val0 (fix_value val1) in
				TopDef.make_value value.TopDef.v_pos (TopDef.VConstrP(name, val1'))
			| _ ->
				Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
					(Errors.string_of_pos value.TopDef.v_pos);
				raise Eval_exception
			end
		| _ -> raise Misc.Internal_error
		end

let rec eval gamma expr =
	let pos = expr.MlslAst.e_pos in
	match expr.MlslAst.e_kind with
	| MlslAst.EVar x ->
		begin try
			fix_value (StrMap.find x gamma)
		with
		| Not_found ->
			begin match TopDef.check_name x with
			| Some value -> fix_value value
			| None ->
				Errors.error_p pos "Unbound value %s" x;
				raise Eval_exception
			end
		end
	| MlslAst.EVarying _ ->
		Errors.error_p pos 
			"Can not evaluate varying variable in compilation time";
		raise Eval_exception
	| MlslAst.EInt n ->
		TopDef.make_value pos (TopDef.VInt n)
	| MlslAst.EFloat f ->
		TopDef.make_value pos (TopDef.VFloat f)
	| MlslAst.ETrue ->
		TopDef.make_value pos (TopDef.VBool true)
	| MlslAst.EFalse ->
		TopDef.make_value pos (TopDef.VBool false)
	| MlslAst.ESwizzle(e, swizzle) ->
		let v = eval gamma e in
		begin match swizzle with
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
		end
	| MlslAst.ERecord fields ->
		TopDef.make_value pos (TopDef.VRecord(
			List.fold_left (fun record field ->
				StrMap.add field.MlslAst.rfv_name (eval gamma field.MlslAst.rfv_value) record
			) StrMap.empty fields))
	| MlslAst.ESelect(e, field) ->
		let v = eval gamma e in
		begin match v.TopDef.v_kind with
		| TopDef.VRecord record ->
			begin try
				fix_value (StrMap.find field record)
			with
			| Not_found ->
				Errors.error_p pos "Record defined at %s hasn't field %s."
					(Errors.string_of_pos v.TopDef.v_pos) field;
				raise Eval_exception
			end
		| kind ->
			Errors.error_p pos "Value defined at %s is not a record."
				(Errors.string_of_pos v.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.EPair(e1, e2) ->
		let v1 = eval gamma e1 in
		let v2 = eval gamma e2 in
		TopDef.make_value pos (TopDef.VPair(v1, v2))
	| MlslAst.EBinOp(op, e1, e2) ->
		let v1 = eval gamma e1 in
		let v2 = eval gamma e2 in
		eval_binop pos op v1 v2
	| MlslAst.EUnOp(op, e) ->
		let v = eval gamma e in
		eval_unop pos op v
	| MlslAst.EAbs(pat, body) ->
		TopDef.make_value pos (TopDef.VFunc(gamma, pat, body))
	| MlslAst.EApp(e1, e2) ->
		let func = eval gamma e1 in
		let arg  = eval gamma e2 in
		begin match func.TopDef.v_kind with
		| TopDef.VFunc(gamma', pat, body) ->
			if !credits <= 0 then begin
				Errors.error_p pos
					"Too complex functional code. Evaluation requires more than 1024 function applications.";
				raise Eval_exception
			end else begin
				credits := !credits - 1;
				eval (bind_pattern gamma' pat arg) body
			end
		| TopDef.VSampler _ ->
			Errors.error_p pos "Sampler application is unavailable in compilation time evaluation.";
			raise Eval_exception
		| TopDef.VConstrU name ->
			TopDef.make_value pos (TopDef.VConstrP(name, arg))
		| kind ->
			Errors.error_p pos "Can not apply %s defined at %s."
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos pos);
			raise Eval_exception
		end
	| MlslAst.ELet(pat, e1, e2) ->
		let v1 = eval gamma e1 in
		eval (bind_pattern gamma pat v1) e2
	| MlslAst.EFix(pat, e) ->
		let (v0, gamma') = fix_pattern_pre gamma pat in
		let v = eval gamma' e in
		fix_pattern_post pat v0 v
	| MlslAst.EIf(cnd, e1, e2) ->
		let cnd_val = eval gamma cnd in
		begin match cnd_val.TopDef.v_kind with
		| TopDef.VBool b ->
			if b then eval gamma e1 else eval gamma e2
		| kind ->
			Errors.error_p cnd.MlslAst.e_pos "Can not evaluate %s defined at %s to boolean value."
				(TopDef.string_of_value_kind kind) (Errors.string_of_pos cnd_val.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.EMatch(e, patterns) ->
		let mval = eval gamma e in
		bind_match_patterns gamma e.MlslAst.e_pos patterns mval
	| MlslAst.EFragment e ->
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VFragment(gamma, e))
	| MlslAst.EVertex e ->
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VVertex(gamma, e))
	| MlslAst.EConstrU name ->
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VConstrU name)
	| MlslAst.EConstrP(name, e) ->
		let v = eval gamma e in
		TopDef.make_value expr.MlslAst.e_pos (TopDef.VConstrP(name, v))

and bind_match_patterns gamma pos patterns value =
	match patterns with
	| [] ->
		Errors.error_p pos "Value defined at %s is unmatched." (Errors.string_of_pos value.TopDef.v_pos);
		raise Eval_exception
	| mpat :: patterns ->
		begin match bind_match_pattern_list gamma mpat.MlslAst.mp_patterns value
			mpat.MlslAst.mp_condition with
		| None -> bind_match_patterns gamma pos patterns value
		| Some gamma' -> eval gamma' mpat.MlslAst.mp_action
		end

and bind_match_pattern_list gamma pats value cnd_opt =
	match pats with
	| [] -> None
	| pat :: pats ->
		begin match try_bind_pattern gamma pat value, cnd_opt with
		| None, _ -> bind_match_pattern_list gamma pats value cnd_opt
		| Some gamma', None -> Some gamma'
		| Some gamma', Some cnd ->
			let cnd_val = eval gamma' cnd in
			begin match cnd_val.TopDef.v_kind with
			| TopDef.VBool b ->
				if b then Some gamma'
				else bind_match_pattern_list gamma pats value cnd_opt
			| kind ->
				Errors.error_p cnd.MlslAst.e_pos "Can not evaluate %s defined at %s to boolean value."
					(TopDef.string_of_value_kind kind) (Errors.string_of_pos cnd_val.TopDef.v_pos);
				raise Eval_exception
			end
		end

let rec bind_top_pattern pat value =
	match pat.MlslAst.p_kind with
	| MlslAst.PAny   -> ()
	| MlslAst.PVar x ->
		TopDef.add x value
	| MlslAst.PTypedVar(x, tp) ->
		TopDef.add x (cast_value_to_type pat.MlslAst.p_pos value tp.MlslAst.tt_typ)
	| MlslAst.PTrue ->
		begin match value.TopDef.v_kind with
		| TopDef.VBool true  -> ()
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PFalse ->
		begin match value.TopDef.v_kind with
		| TopDef.VBool false -> ()
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PPair(pat1, pat2) ->
		begin match value.TopDef.v_kind with
		| TopDef.VPair(v1, v2) ->
			bind_top_pattern pat1 (fix_value v1);
			bind_top_pattern pat2 (fix_value v2)
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PConstrU name ->
		begin match value.TopDef.v_kind with
		| TopDef.VConstrU name' when name = name' -> ()
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	| MlslAst.PConstrP(name, p) ->
		begin match value.TopDef.v_kind with
		| TopDef.VConstrP(name', v) when name = name' -> bind_top_pattern p (fix_value v)
		| _ ->
			Errors.error_p pat.MlslAst.p_pos "Can not bind value defined at %s to this pattern."
				(Errors.string_of_pos value.TopDef.v_pos);
			raise Eval_exception
		end
	
let eval_topdef td =
	match td.MlslAst.td_kind with
	| MlslAst.TDAttrDecl(name, semantics, typ) ->
		TopDef.add_attr td.MlslAst.td_pos name semantics typ
	| MlslAst.TDConstDecl(name, typ) ->
		TopDef.add_const td.MlslAst.td_pos name typ
	| MlslAst.TDSamplerDecl(name, typ) ->
		TopDef.add_sampler td.MlslAst.td_pos name typ
	| MlslAst.TDLocalDef(pat, expr) ->
		begin try
			let value = eval StrMap.empty expr in
			bind_top_pattern pat value
		with
		| Eval_exception -> ()
		end
	| MlslAst.TDShader(name, expr) ->
		begin try
			let value = eval StrMap.empty expr in
			TopDef.add name value;
			Misc.Opt.iter (Midlang.unfold_shader name value) (fun mprog ->
			let mprog_opt = Midlang.optimize mprog in
			!target_func mprog_opt
			)
		with
		| Eval_exception -> ()
		end

let rec eval_all td_list =
	match td_list with
	| [] -> ()
	| td :: td_list ->
		eval_topdef td;
		eval_all td_list
