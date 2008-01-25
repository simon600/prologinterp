open Types;;
open Unificator;;

exception Not_a_number;;
exception Cant_evaluate;;
exception No_more_conts;;

let c = ref 0

(* gets unique variable *)
let get_unique_var() =
    (c := !c+1; "___UNIQUE_VAR"^(string_of_int !c))

(* gets list of variables in term *)
let rec get_variables term list =
    match term with
	TermOr(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermAnd(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermVariable v -> if List.exists (fun var -> var = v) list then list else v::list
      | TermString str -> list
      | TermConstant const -> list
      | TermFunctor(nam,args) -> 
	  let rec get_vars_from_args args list =
	    match args with
		[] -> list
	      | t::terms -> get_vars_from_args terms (get_variables t list)
	  in
	    get_vars_from_args args list
      | TermIs(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticPlus(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticMinus(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticMult(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticDiv(t1,t2) -> get_variables t2 (get_variables t1 list)
      | TermArithmeticEquality(t1,t2) -> get_variables t2 (get_variables t1 list)
      | _ -> list

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
    | _ -> raise Not_a_number
      


let more() =
  print_string "More?\n";
  if read_line() = ";" then true
  else false

let conts = ref []
let add_cont cont =
  conts := cont::!conts
let get_cont() =
  match !conts with
      [] -> raise No_more_conts
    | cont::conts' -> (conts := conts'; cont)

(* evaluates functor *)
let rec functor_eval functor_term database rep clauses cont =
    let term = replace functor_term rep in
  match clauses with
      [] -> cont (false,[])
    | clause::clauses' -> (match clause with
			      SingleClause dterm -> let uni = (unify term dterm rep)   (* found a fact in database *)
			      in
				if fst uni then
				  (add_cont (fun () -> functor_eval term database rep clauses' cont); cont uni)
				else functor_eval term database rep clauses' cont
			    | ClauseImplication(dterm,condition) -> let uni = (unify term dterm rep)
			      in
				(add_cont (fun () -> functor_eval term database rep clauses' cont);
				if fst uni then
				  evaluate condition database (snd uni) database (fun vt -> cont vt)
				else cont (false,[])))


and evaluate term database rep clauses cont =
  let repterm = replace term rep             (* apply replacement to the term *)
  in
  match repterm with
      TermTermUnify(term1,term2) -> let uni = (unify term1 term2 rep)
      in
	if fst uni then cont uni
	else cont (false,[])
    | TermArithmeticEquality(t1,t2) -> let n1 = arithmetic_eval t1
				       and n2 = arithmetic_eval t2
      in
	if n1 = n2 then cont (true,rep)
      else cont (false,[])
    | TermIs(t1,t2) -> let n2 = TermConstant (ConstantNumber (arithmetic_eval t2))
      in
	cont (unify t1 n2 [])
    | TermFunctor(nam,args) -> functor_eval repterm database rep clauses cont
    | TermAnd(t1,t2) -> 
	evaluate t1 database rep clauses
	  (fun vt1 ->
	     if fst vt1 then
	       evaluate t2 database (snd vt1) clauses (fun vt2 -> cont vt2)
	     else cont (false,[]))
    | TermOr(t1,t2) -> (add_cont (fun () -> evaluate t2 database rep clauses cont);
			evaluate t1 database rep clauses (fun vt -> cont vt))
    | _ -> raise Cant_evaluate

let interpret term database =
  let continue = ref true
  and eval = ref (false,[])
  and cont = ref (fun() -> (false,[]))
  and filter replacement = 
    let variables = (get_variables term []) in
    List.filter (fun (var,_) -> List.exists (fun v -> v = var) variables) replacement
  in
    (conts := [];
     add_cont (fun () -> evaluate term database [] database (fun v -> v));
     while (List.length !conts > 0 && !continue) do
       (cont := get_cont();
	eval := !cont();
	if (fst !eval) then
	  (print_string "Yes\n";
	   print_replacement (filter (snd !eval));
	   print_endline "";
	   if List.length !conts > 0 then
	     continue := more()
	   else ())
	else
	  if (List.length !conts) = 0 then
	    print_string "No\n"
	  else ())
     done)


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
