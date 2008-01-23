open Types;;


let rec term_to_string term =
  match term with
      TermVariable v -> v
    | TermString str -> str
    | TermConstant const -> (match const with
				  ConstantAtom str -> str
				| ConstantNumber num -> (match num with
							      Integer n -> string_of_int n
							    | Float f -> string_of_float f))
    | TermIs(t1,t2) -> (term_to_string t1)^" is "^(term_to_string t2)
    | TermIfThen(t1,t2) -> "if "^(term_to_string t1)^" then "^(term_to_string t2)
    | TermIfThenElse(t1,t2,t3) -> "if "^(term_to_string t1)^" then "^(term_to_string t2)^" else "^(term_to_string t3)
    | TermArithmeticPlus(t1,t2) -> (term_to_string t1)^" + "^(term_to_string t2)
    | TermArithmeticMinus(t1,t2) -> (term_to_string t1)^" - "^(term_to_string t2)
    | TermArithmeticMult(t1,t2) -> (term_to_string t1)^" * "^(term_to_string t2)
    | TermArithmeticDiv(t1,t2) -> (term_to_string t1)^" / "^(term_to_string t2)
    | TermArithmeticEquality(t1,t2) -> (term_to_string t1)^" =:= "^(term_to_string t2)
    | _ -> "";;

let rec print_replacement rep =
  match rep with
      [] -> ()
    | (v,rep_term)::trep -> print_string (v^" = "^(term_to_string rep_term)^"   "); print_replacement trep;;



(* replaces occurances of var in term with rep_term *)
let rec replace_var term var rep_term =
  let rep term =
    replace_var term var rep_term
  in
  match term with
      TermVariable v ->
	if v = var then rep_term else term
    | TermFunctor(n,args) -> TermFunctor(n,List.map rep args)
    | TermIs(t1,t2) -> TermIs(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticPlus(t1,t2) -> TermArithmeticPlus(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticMinus(t1,t2) -> TermArithmeticPlus(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticMult(t1,t2) -> TermArithmeticMinus(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticDiv(t1,t2) -> TermArithmeticMult(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | TermArithmeticEquality(t1,t2) -> TermArithmeticDiv(replace_var t1 var rep_term, replace_var t2 var rep_term)
    | _ -> term

(* appends a replacement to a term *)
let rec replace term replacement = 
  match replacement with
      [] -> term
    | (v,rep)::trep -> replace (replace_var term v rep) trep

(* tries to unify two terms, returns if terms can be unified and replacement needed for unification *)
let unify term1 term2 =
  let rec unifyr term1 term2 rep =
    let rterm1 = replace term1 rep
    and rterm2 = replace term2 rep
    in
      if rterm1 = rterm2 then (true,rep)
      else
      match rterm2 with
	  TermVariable v2 -> (true,(v2,rterm1)::rep)
	| _ ->
	    (match rterm1 with
		TermVariable v1 -> (true,(v1,rterm2)::rep)
	      | TermIs(t11,t12) -> (match rterm2 with
					TermIs(t21,t22) -> let uni1 = unifyr t11 t21 rep in
					  if fst uni1 then unifyr t12 t22 (snd uni1)
					  else (false,[])
				      | _ -> (false,[]))
	      | TermArithmeticPlus(t11,t12) -> (match rterm2 with
						  TermArithmeticPlus(t21,t22) -> let uni1 = unifyr t11 t21 rep in
						    if fst uni1 then unifyr t12 t22 (snd uni1)
						    else (false,[])
						  | _ -> (false,[]))
						  
	      | TermArithmeticMinus(t11,t12) -> (match rterm2 with
						  TermArithmeticMinus(t21,t22) -> let uni1 = unifyr t11 t21 rep in
						    if fst uni1 then unifyr t12 t22 (snd uni1)
						    else (false,[])
						   | _ -> (false,[]))
	      | TermArithmeticMult(t11,t12) -> (match rterm2 with
						  TermArithmeticMult(t21,t22) -> let uni1 = unifyr t11 t21 rep in
						    if fst uni1 then unifyr t12 t22 (snd uni1)
						    else (false,[])
						  | _ -> (false,[]))
	      | TermArithmeticDiv(t11,t12) -> (match rterm2 with
						  TermArithmeticDiv(t21,t22) -> let uni1 = unifyr t11 t21 rep in
						    if fst uni1 then unifyr t12 t22 (snd uni1)
						    else (false,[])
						 | _ -> (false,[]))

	      | _ -> (false,[]))
  in
    unifyr term1 term2 []





