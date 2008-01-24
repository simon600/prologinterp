open Types;;
open Unificator;;

exception Not_a_number;;
exception Cant_evaluate;;

let print_evaluation (b,rep) =
  if b then (print_string "Yes\n"; print_replacement rep; print_string "\n")
  else print_string "No\n"

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

(* evaluates functor *)
let rec functor_eval functor_term database rep clauses rest =
    let term = replace functor_term rep in
  match clauses with
      [] -> ((false,[]),rest)
    | clause::clauses' -> (match clause with
			      SingleClause dterm -> let uni = (unify term dterm rep)   (* found a fact in database *)
			      in
				if fst uni then (uni,(term,clauses',rep)::rest)
				else functor_eval term database rep clauses' rest
			    | ClauseImplication(dterm,condition) -> let uni = (unify term dterm rep)
			      in
				if fst uni then
				  let (eval_condition,rest') = (evaluate condition database (snd uni) database rest) (* evaluate condition *)
				  in				   			    	    
				    if fst eval_condition then 
				      (eval_condition,(term,clauses',rep)::rest')
				    else functor_eval term database rep clauses' rest
				else ((false,[]),rest))


and evaluate term database rep clauses rest =
  let repterm = replace term rep             (* apply replacement to the term *)
  in
  match repterm with
      TermTermUnify(term1,term2) -> let uni = (unify term1 term2 rep)
      in
	if fst uni then (uni,rest)
	else ((false,[]),rest)
    | TermArithmeticEquality(t1,t2) -> let n1 = arithmetic_eval t1
				       and n2 = arithmetic_eval t2
      in
	if n1 = n2 then ((true,rep),rest)
      else ((false,[]),rest)
    | TermIs(t1,t2) -> let n2 = TermConstant (ConstantNumber (arithmetic_eval t2))
      in
	(unify t1 n2 [],rest)
    | TermFunctor(nam,args) -> functor_eval repterm database rep clauses rest
    | TermAnd(t1,t2) -> let (env,_) = (evaluate t1 database rep clauses rest)
      in
	(print_string "and tu: ";
	if fst env then evaluate t2 database (snd env) clauses rest
	else ((false,[]),rest))
    | TermOr(t1,t2) -> let (env,rest') = (evaluate t1 database rep database rest)
      in
	if fst env then
	  (env,(t2,database,rep)::rest)	
	else evaluate t2 database rep database rest
    | _ -> print_string(term_to_string repterm); raise Cant_evaluate


let interpret term database =
  let eval = ref (evaluate term database [] database [])
  and continue = ref false
  in
    (print_evaluation (fst !eval);
     if (fst (fst !eval)) then continue := more()
     else ();
    while List.length (snd !eval) > 0 && !continue do
       (if (fst (fst !eval)) then ()
	else print_string "No\n";
	continue := false;
	 ((match (snd !eval) with
	      (term,clauses,rep)::rest -> eval := evaluate term database rep clauses rest
	    | [] -> ());
	  if (fst (fst !eval)) then
	    (print_evaluation (fst !eval);
	     continue := more())
	  else
	    continue := true))
    done)
