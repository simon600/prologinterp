open Types;;

exception Unsuported_term;;
exception Type_error;;

let rec term_to_string term =
  match term with
      TermOr(t1,t2) -> (term_to_string t1)^"; "^(term_to_string t2)
    | TermAnd(t1,t2) -> (term_to_string t1)^", "^(term_to_string t2)
    | TermVariable v -> v
    | TermString str -> str
    | TermConstant const -> (match const with
				  ConstantAtom str -> str
				| ConstantNumber num -> (match num with
							      Integer n -> string_of_int n
							    | Float f -> string_of_float f))
    | TermFunctor(nam,args) ->
	let rec string_of_arguments args =
	  match args with
	      [] -> ""
	    | [t] -> (term_to_string t)
	    | t::terms -> (term_to_string t)^","^(string_of_arguments terms)
	in
	  nam^"("^(string_of_arguments args)^")"
    | TermIs(t1,t2) -> (term_to_string t1)^" is "^(term_to_string t2)
    | TermIfThen(t1,t2) -> "if "^(term_to_string t1)^" then "^(term_to_string t2)
    | TermIfThenElse(t1,t2,t3) -> "if "^(term_to_string t1)^" then "^(term_to_string t2)^" else "^(term_to_string t3)
    | TermArithmeticPlus(t1,t2) -> (term_to_string t1)^" + "^(term_to_string t2)
    | TermArithmeticMinus(t1,t2) -> (term_to_string t1)^" - "^(term_to_string t2)
    | TermArithmeticMult(t1,t2) -> (term_to_string t1)^" * "^(term_to_string t2)
    | TermArithmeticDiv(t1,t2) -> (term_to_string t1)^" / "^(term_to_string t2)
    | TermArithmeticIntDiv(t1,t2) -> (term_to_string t1)^" // "^(term_to_string t2)
    | TermArithmeticEquality(t1,t2) -> (term_to_string t1)^" =:= "^(term_to_string t2)
    | TermArithmeticInequality(t1,t2) -> (term_to_string t1)^" =\\= "^(term_to_string t2)
    | TermArithmeticLess(t1,t2) -> (term_to_string t1)^" < "^(term_to_string t2)
    | TermArithmeticGreater(t1,t2) -> (term_to_string t1)^" >= "^(term_to_string t2)
    | TermArithmeticLeq(t1,t2) -> (term_to_string t1)^" <= "^(term_to_string t2)
    | TermArithmeticGeq(t1,t2) -> (term_to_string t1)^" >= "^(term_to_string t2)
    | TermTermEquality(t1,t2) -> (term_to_string t1)^" == "^(term_to_string t2)
    | TermTermUnify(t1,t2) -> (term_to_string t1)^" = "^(term_to_string t2)
    | TermNegation t -> "not "^(term_to_string t)
    | TermTermNotUnify(t1,t2) -> (term_to_string t1)^" /= "^(term_to_string t2)
    | _ -> "";;

let rec print_replacement rep =
  match rep with
      [] -> ()
    | (v,rep_term)::trep -> print_string (v^" = "^(term_to_string rep_term)^"   "); print_replacement trep;;



(* replaces occurances of var in term with rep_term *)
(*let rec replace_var term var rep_term =
  let rep term =
    replace_var term var rep_term
  in
  match term with
      TermOr(t1,t2) -> TermOr(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermAnd(t1,t2) -> TermAnd(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermVariable v ->
	if v = var then rep_term else term
    | TermFunctor(nam,args) -> TermFunctor(nam,List.map rep args)
    | TermIs(t1,t2) -> TermIs(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticPlus(t1,t2) -> TermArithmeticPlus(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticMinus(t1,t2) -> TermArithmeticPlus(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticMult(t1,t2) -> TermArithmeticMinus(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticDiv(t1,t2) -> TermArithmeticMult(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticEquality(t1,t2) -> TermArithmeticEquality(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermTermEquality(t1,t2) -> TermTermEquality(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | _ -> term*)

(* appends a replacement to a term *)
let rec replace term replacement = 
  let rep term' =
      replace term' replacement
  in
  match term with
      TermOr(t1,t2) -> TermOr(replace t1 replacement, replace t2 replacement)
    | TermAnd(t1,t2) -> TermAnd(replace t1 replacement, replace t2 replacement)
    | TermVariable var ->
	 (match replacement with
	     [] -> term
	   | (v,rep)::replacement' -> 
	       if v = var then rep
	       else replace term replacement')
    | TermFunctor(nam,args) -> TermFunctor(nam,List.map rep args)
    | TermIs(t1,t2) -> TermIs(replace t1 replacement, replace t2 replacement)
    | TermArithmeticPlus(t1,t2) -> TermArithmeticPlus(replace t1 replacement, replace t2 replacement)
    | TermArithmeticMinus(t1,t2) -> TermArithmeticMinus(replace t1 replacement, replace t2 replacement)
    | TermArithmeticMult(t1,t2) -> TermArithmeticMult(replace t1 replacement, replace t2 replacement)
    | TermArithmeticDiv(t1,t2) -> TermArithmeticDiv(replace t1 replacement, replace t2 replacement)
    | TermArithmeticIntDiv(t1,t2) -> TermArithmeticIntDiv(replace t1 replacement, replace t2 replacement)
    | TermArithmeticEquality(t1,t2) -> TermArithmeticEquality(replace t1 replacement, replace t2 replacement)
    | TermArithmeticInequality(t1,t2) -> TermArithmeticInequality(replace t1 replacement, replace t2 replacement)
    | TermArithmeticLess(t1,t2) -> TermArithmeticLess(replace t1 replacement, replace t2 replacement)
    | TermArithmeticGreater(t1,t2) -> TermArithmeticGreater(replace t1 replacement, replace t2 replacement)
    | TermArithmeticLeq(t1,t2) -> TermArithmeticLeq(replace t1 replacement, replace t2 replacement)
    | TermArithmeticGeq(t1,t2) -> TermArithmeticGeq(replace t1 replacement, replace t2 replacement)
    | TermTermEquality(t1,t2) -> TermTermEquality(replace t1 replacement, replace t2 replacement)
    | TermTermUnify(t1,t2) -> TermTermUnify(replace t1 replacement, replace t2 replacement)
    | TermTermNotUnify(t1,t2) -> TermTermNotUnify(replace t1 replacement, replace t2 replacement)
    | TermNegation t -> TermNegation (replace t replacement)
    | _ -> term


(* adds new variable replacement to given replacement *)
let rec add_replacement (var,term) replacement =
  let replace_rep (var',term') =
    (var',replace term' [(var,term)])
  in
    (var,term)::(List.map replace_rep replacement)
    

(* tries to unify two terms, returns if terms can be unified and replacement needed for unification *)
let rec unify term1 term2 rep =
  let rterm1 = replace term1 rep
  and rterm2 = replace term2 rep
  in
    if rterm1 = rterm2 then (true,rep)
    else
      match rterm1 with
	  TermVariable v1 -> (true,(add_replacement (v1,rterm2) rep))
	| _ ->
	    (match rterm2 with
		 TermAnd(t21,t22) -> (match rterm1 with
					 TermAnd(t11,t12) -> let uni = unify t11 t21 rep in
					   if fst uni then unify t12 t22 (snd uni)
					   else (false,[])
				       | _ -> (false,[]))
	       | TermOr(t21,t22) -> (match rterm1 with
					 TermOr(t11,t12) -> let uni = unify t11 t21 rep in
					   if fst uni then unify t12 t22 (snd uni)
					   else (false,[])
				       | _ -> (false,[]))
	       | TermVariable v2 -> (true,(add_replacement (v2,rterm1) rep))
	       | TermIs(t21,t22) -> (match rterm1 with
					 TermIs(t11,t12) -> let uni = unify t11 t21 rep in
					   if fst uni then unify t12 t22 (snd uni)
					   else (false,[])
				       | _ -> (false,[]))
	       | TermArithmeticPlus(t21,t22) -> (match rterm1 with
						     TermArithmeticPlus(t11,t12) -> let uni = unify t11 t21 rep in
						       if fst uni then unify t12 t22 (snd uni)
						       else (false,[])
						   | _ -> (false,[]))
						  
	       | TermArithmeticMinus(t21,t22) -> (match rterm1 with
						      TermArithmeticMinus(t11,t12) -> let uni = unify t11 t21 rep in
							if fst uni then unify t12 t22 (snd uni)
							else (false,[])
						    | _ -> (false,[]))
	       | TermArithmeticMult(t21,t22) -> (match rterm1 with
						     TermArithmeticMult(t11,t12) -> let uni = unify t11 t21 rep in
						       if fst uni then unify t12 t22 (snd uni)
						       else (false,[])
						   | _ -> (false,[]))
	       | TermArithmeticDiv(t11,t12) -> (match rterm2 with
						    TermArithmeticDiv(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticIntDiv(t11,t12) -> (match rterm2 with
						    TermArithmeticIntDiv(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticEquality(t11,t12) -> (match rterm2 with
						    TermArithmeticEquality(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticInequality(t11,t12) -> (match rterm2 with
						    TermArithmeticInequality(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticLess(t11,t12) -> (match rterm2 with
						    TermArithmeticLess(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticGreater(t11,t12) -> (match rterm2 with
						    TermArithmeticGreater(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticLeq(t11,t12) -> (match rterm2 with
						    TermArithmeticLeq(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticGeq(t11,t12) -> (match rterm2 with
						    TermArithmeticGeq(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermTermEquality(t11,t12) -> (match rterm2 with
						    TermTermEquality(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermTermUnify(t11,t12) -> (match rterm2 with
						    TermTermUnify(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermTermNotUnify(t11,t12) -> (match rterm2 with
						    TermTermNotUnify(t21,t22) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermNegation t1 -> (match rterm2 with
						    TermNegation t2 -> unify t1 t2 rep
						  | _ -> (false,[]))
	       | TermFunctor(nam2,args2) -> 
		   let rec unify_args args1 args2 rep =
		     (match args1 with
			 [] -> (true,rep)
		       | term1::terms1 -> (match args2 with
					       [] -> raise Type_error
					     | term2::terms2 -> let uni = unify term1 term2 rep
					       in
						 if fst uni then unify_args terms1 terms2 (snd uni) else (false,[])))
		   in
		     (match rterm1 with
			  TermFunctor(nam1,args1) -> if nam1 = nam2 && (List.length args1) = (List.length args2) then unify_args args1 args2 rep else (false,[])
			| _ -> (false,[]))
	       | _ -> if rterm1 = rterm2 then (true,rep) else (false,[]))






