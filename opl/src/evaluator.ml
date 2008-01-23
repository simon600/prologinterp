open Types;;
open Unificator;;

let evaluate term =
  match term with
      TermTermUnify(term1,term2) -> let uni = (unify term1 term2)
      in
	if fst uni then (print_string "Yes\n"; print_replacement (snd uni); print_string "\n")
	else print_string "No\n"
	
    | _ -> ()
