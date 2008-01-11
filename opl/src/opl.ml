open Parser;;
open Lexer;;

let main () =
    let buff = ref (Lexing.from_string(""))
    in 
    	try 
	   while (true) do					(* while not EOF *)
	    	print_string ":- ";				(* print prompt *)
		buff := Lexing.from_string(read_line());	(* read the expression *)
		Parser.sentence Lexer.token !buff	(* create its syntax tree*)                    
           done
	with
	    | End_of_file -> (print_string "\n"; exit 0)
;;

let _ = main ()  
             
