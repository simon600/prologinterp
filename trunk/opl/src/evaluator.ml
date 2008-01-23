open Types;;
open Unificator;;

exception Not_a_number;;

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
      

let evaluate term =
  match term with
      TermTermUnify(term1,term2) -> let uni = (unify term1 term2)
      in
	if fst uni then (true,snd uni)
	else (false,[])
    | TermArithmeticEquality(t1,t2) -> let n1 = arithmetic_eval t1
				       and n2 = arithmetic_eval t2
      in
	if n1 = n2 then (true,[])
      else (false,[])
    | TermIs(t1,t2) -> let n2 = TermConstant (ConstantNumber (arithmetic_eval t2))
      in
	unify t1 n2
    | _ -> (false,[])
