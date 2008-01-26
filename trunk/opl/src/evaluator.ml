open Types;;
open Unificator;;

exception Not_a_number;;
exception Cant_evaluate;;
exception No_more_conts;;
exception Not_integer;;
exception Cut of (bool * (name * term) list);;

let c = ref 0 (* used by get_unique_var *)

(* gets unique variable *)
let get_unique_var() =
    (c := !c+1; "___UNIQUE_VAR"^(string_of_int !c))

(* gets list of variables in term *)
let rec get_variables term list =
  let rec get_vars_from_args args list = (* gets variables from arguments *)
    match args with
	[] -> list
      | t::terms -> get_vars_from_args terms (get_variables t list)
  in
    match term with
	TermOr(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermAnd(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermVariable v -> if List.exists (fun var -> var = v) list then list else v::list
      | TermString str -> list
      | TermConstant const -> list
      | TermFunctor(nam,args) -> 	  
	    get_vars_from_args args list
      | TermIs(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticPlus(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticMinus(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticMult(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticDiv(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticIntDiv(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticEquality(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticInequality(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticLess(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticGreater(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticLeq(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticGeq(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermTermUnify(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermTermNotUnify(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermNegation t -> get_variables t list
      | TermTermEquality(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermList listterm ->
	  (match listterm with
	       EmptyList -> list
	     | NormalList args -> get_vars_from_args args list
	     | DividedList (args,term) -> get_variables term (get_vars_from_args args list))
      | _ -> list


(* makes variables in clause unique *)
let make_unique clause =
  let var_list = ref []
  and replacement = ref []
  in 
  (match clause with
       SingleClause term -> var_list := get_variables term []
     | ClauseImplication (term1,term2) ->	
	var_list := get_variables term2 (get_variables term1 []));
  replacement := List.map (fun var -> (var, TermVariable (get_unique_var()))) !var_list;
  (match clause with
      SingleClause term -> SingleClause (replace term !replacement)
    | ClauseImplication (term1,term2) ->
	ClauseImplication (replace term1 !replacement, replace term2 !replacement))



(* evaluates arithmetic expression *)
let rec arithmetic_eval term = 
  match term with
      TermConstant const -> (match const with
				 ConstantNumber n -> n
			       | _ -> raise Not_a_number)
    | TermArithmeticPlus(t1,t2) -> let n1 = (arithmetic_eval t1) 
				   and n2 = (arithmetic_eval t2)
      in
	(match n1 with
	    Float fl1 -> (match n2 with
			     Float fl2 -> Float (fl1 +. fl2)
			    | Integer i2 -> Float (fl1 +. (float_of_int i2)))
	  | Integer i1 -> (match n2 with
			       Float fl2 -> Float ((float_of_int i1) +. fl2)
			     | Integer i2 -> Integer (i1 + i2)))
    | TermArithmeticMinus(t1,t2) -> let n1 = (arithmetic_eval t1) 
				   and n2 = (arithmetic_eval t2)
      in
	(match n1 with
	    Float fl1 -> (match n2 with
			     Float fl2 -> Float (fl1 -. fl2)
			    | Integer i2 -> Float (fl1 -. (float_of_int i2)))
	  | Integer i1 -> (match n2 with
			       Float fl2 -> Float ((float_of_int i1) -. fl2)
			     | Integer i2 -> Integer (i1 - i2)))
    | TermArithmeticMult(t1,t2) -> let n1 = (arithmetic_eval t1) 
				   and n2 = (arithmetic_eval t2)
      in
	(match n1 with
	    Float fl1 -> (match n2 with
			     Float fl2 -> Float (fl1 *. fl2)
			    | Integer i2 -> Float (fl1 *. (float_of_int i2)))
	  | Integer i1 -> (match n2 with
			       Float fl2 -> Float ((float_of_int i1) *. fl2)
			     | Integer i2 -> Integer (i1 * i2)))
    | TermArithmeticDiv(t1,t2) -> let n1 = (arithmetic_eval t1) 
				   and n2 = (arithmetic_eval t2)
      in
	(match n1 with
	    Float fl1 -> (match n2 with
			     Float fl2 -> Float (fl1 /. fl2)
			    | Integer i2 -> Float (fl1 /. (float_of_int i2)))
	  | Integer i1 -> (match n2 with
			       Float fl2 -> Float ((float_of_int i1) /. fl2)
			     | Integer i2 -> Integer (i1 / i2)))
    | TermArithmeticIntDiv(t1,t2) -> let n1 = (arithmetic_eval t1) 
				   and n2 = (arithmetic_eval t2)
      in
	(match n1 with
	    Float fl1 -> raise Not_integer
	  | Integer i1 -> (match n2 with
			       Float fl2 -> raise Not_integer
			     | Integer i2 -> Integer (i1 / i2)))
    | _ -> raise Not_a_number
      

(* asks if evaluation should be continued *)
let more() =
  print_string "More?\n";
  if read_line() = ";" then true
  else false

let conts = ref []    (* stores calculations to continue *)
let add_cont cont =   (* adds calculations continuation to conts *)
  conts := cont::!conts
let get_cont() =      (* gets next continuation and removes it from conts *)
  match !conts with
      [] -> raise No_more_conts
    | cont::conts' -> (conts := conts'; cont)


(* evaluates functor 
functor_term is a term to evaluate
database is a database loaded into the program 
rep is a replacement
clauses is a list of clauses from database that haven't beed checked yet
cont is a continuation *)
let rec functor_eval functor_term database rep clauses cont =
  let term = replace functor_term rep in (* replace variables in term *)
    match clauses with
	[] -> cont (false,[]) (* no more facts or implications in database *)
      | dclause::clauses' -> 
	  let clause = make_unique dclause in
	  (match clause with
	       SingleClause dterm -> let uni = (unify term dterm rep)   (* found a fact in database *)
	       in
		 if fst uni then
		   (add_cont (fun () -> functor_eval term database rep clauses' cont); cont uni) (* term unifies with fact in database, so store rest of possible calculations and return result of unification *)
		 else functor_eval term database rep clauses' cont (* term doesn't unify with fact, so try another possibilities *)
	     | ClauseImplication(dterm,condition) -> let uni = (unify term dterm rep) (* found an implication in database, try to unificate with it's resault (left side term) *)
	       in
		 if fst uni then
		   try
		      (* if term unifies with left side of implication then try to evaluate it's condition *)		     
		      evaluate condition database (snd uni) database 
			(fun vt -> (add_cont (fun () -> functor_eval term database rep clauses' cont); cont vt))
		   with
		       Cut ret_value -> cont ret_value (* handle cut operator *)
		 else functor_eval term database rep clauses' cont)
	  (* evaluates terms *)
and evaluate term database rep clauses cont =
  let repterm = replace term rep             (* apply replacement to the term *)
  in
  match repterm with
      TermTermUnify(term1,term2) -> cont (unify term1 term2 rep)
    | TermTermNotUnify(term1,term2) -> 
	let uni = unify term1 term2 rep in
	  cont (not (fst uni), snd uni)
    | TermArithmeticEquality(t1,t2) -> let n1 = arithmetic_eval t1 (* find value of left  term *)
				       and n2 = arithmetic_eval t2 (* find value of right term *)
      in
	if n1 = n2 then cont (true,rep)
      else cont (false,[])
    | TermArithmeticInequality(t1,t2) -> let n1 = arithmetic_eval t1
				       and n2 = arithmetic_eval t2
      in
	if n1 = n2 then cont (false,[])
      else cont (true,rep)
    | TermArithmeticLess(t1,t2) -> let n1 = arithmetic_eval t1
				   and n2 = arithmetic_eval t2
      in
	(match n1 with
	    Float x1 ->
	      (match n2 with
		   Float x2 -> (x1 < x2,rep)
		 | Integer i2 -> (x1 < float_of_int i2,rep))
	  | Integer i1 ->
	      (match n2 with
		   Float x2 -> (float_of_int i1 < x2,rep)
		 | Integer i2 -> (i1 < i2,rep)))
    | TermArithmeticGreater(t1,t2) -> let n1 = arithmetic_eval t1
				      and n2 = arithmetic_eval t2
      in
	(match n1 with
	    Float x1 ->
	      (match n2 with
		   Float x2 -> (x1 > x2,rep)
		 | Integer i2 -> (x1 > float_of_int i2,rep))
	  | Integer i1 ->
	      (match n2 with
		   Float x2 -> (float_of_int i1 > x2,rep)
		 | Integer i2 -> (i1 > i2,rep)))
    | TermArithmeticLeq(t1,t2) -> let n1 = arithmetic_eval t1
				  and n2 = arithmetic_eval t2
      in
	(match n1 with
	    Float x1 ->
	      (match n2 with
		   Float x2 -> (x1 <= x2,rep)
		 | Integer i2 -> (x1 <= float_of_int i2,rep))
	  | Integer i1 ->
	      (match n2 with
		   Float x2 -> (float_of_int i1 <= x2,rep)
		 | Integer i2 -> (i1 <= i2,rep)))
    | TermArithmeticGeq(t1,t2) -> let n1 = arithmetic_eval t1
				  and n2 = arithmetic_eval t2
      in
	(match n1 with
	    Float x1 ->
	      (match n2 with
		   Float x2 -> (x1 >= x2,rep)
		 | Integer i2 -> (x1 >= float_of_int i2,rep))
	  | Integer i1 ->
	      (match n2 with
		   Float x2 -> (float_of_int i1 >= x2,rep)
		 | Integer i2 -> (i1 >= i2,rep)))
    | TermNegation t -> evaluate t database rep clauses (fun vt -> cont (not (fst vt), snd vt))
    | TermTermEquality(t1,t2) -> cont (t1 = t2,rep)
    | TermIs(t1,t2) -> let n2 = TermConstant (ConstantNumber (arithmetic_eval t2))
      in
	cont (unify t1 n2 [])
    | TermFunctor(nam,args) -> functor_eval repterm database rep clauses cont
    | TermAnd(t1,t2) -> 
	evaluate t1 database rep clauses (* evaluate first term *)
	  (fun vt1 ->
	     if fst vt1 then
	       evaluate t2 database (snd vt1) clauses (fun vt2 -> cont vt2) (* if first term returns true in evaluation then the other one will be tried to be evaluated *)
	     else cont (false,[]))
    | TermOr(t1,t2) -> (add_cont (fun () -> evaluate t2 database rep clauses cont); (* add another evaluation possibility to conts *)
			evaluate t1 database rep clauses (fun vt -> cont vt)) (* evaluate first term *)
    | TermCut -> raise (Cut (true,rep))
    | _ -> raise Cant_evaluate

(* evaluates all possible ways a term given a specific database *)
let interpret term database =
  let continue = ref true
  and eval = ref (false,[]) (* stores result of evaluation *)
  and cont = ref (fun() -> (false,[])) (* stores calculations *)
  and filter replacement = (* filters replacement that only variables that exist in term remains *)
    let variables = (get_variables term []) in
    List.filter (fun (var,_) -> List.exists (fun v -> v = var) variables) replacement
  in
    (conts := []; (* clear conts *)
     add_cont (fun () -> evaluate term database [] database (fun v -> v)); (* add first evaluation to conts *)
     (* evaluate paths of calculations while they are availible and user whishes to continue *)
     while (List.length !conts > 0 && !continue) do
       (cont := get_cont(); (* get another evaluation *)
	eval := !cont(); (* invoke it *)
	if (fst !eval) then
	  (print_string "Yes\n";
	   print_replacement (filter (snd !eval));
	   print_endline "";
	   if List.length !conts > 0 then
	     continue := more()
	   else ())
	else
	  if (List.length !conts) = 0 then
	    (print_string "No\n";
	     continue := true)
	  else ())
     done)
