%{

(* --- Header --- *)
  
 
%}

/* ocamlyacc declarations */

%token WHITESPACE   /* ' ', '\t', '\n' */
%token TERM         /* 't' */

%% /* grammar rules */

sentence:
    | sentence_list { $1 }
;

sentence_list:
    | clause WHITESPACE sentence_list { }
    | clause {  }
;

clause:
    | unit_clause   
    { }
  /*  
    | non_unit_clause 
    { }*/
;

unit_clause:
    | head { $1 }
;

non_unit_clause:
    | head ":-" body
    { }
;

head:
    | goal 
    { }
;

body:
/*    | body "->" body ";" body 
    {   }
    
    | body "->" body 
    { $1 }
    
    | body ";" body
    { $1 }

    | body "," body
    { $1 }
*/

    | goal
    { $1 }
;

goal: 
    | term '.'
    { $1 }
;

term:
    | TERM 
    { $1 }
;

%%  

(* --- Trailer --- *)

%{
let main () =
    let buff = ref (Lexing.from_string(""))
    in 
    	try 
	   while (true) do							(* while not EOF *)
	    	print_string ":- ";						(* print prompt *)
		buff := Lexing.from_string(read_line());			(* read the expression *)
		let syntree = Parser.program Lexer.lexer !buff			(* create its syntax tree*)
	   	in print_string "t."
	   done
	with
	    | Lexer.EOF -> (print_string "\n"; exit 0)				(* input ends *)
	    | End_of_file -> (print_string "\n"; exit 0)
;;

let _ = main ()  
              

%}
