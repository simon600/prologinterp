open Parser;;
open Lexer;;
open Types;;
open Unificator;;
open Evaluator;;

(* 
 * reading database from input files.
 * params - program parameters
 *)

let read_database params = 

    let database = ref []           (* database we create *)
    in          

    (* 
     * function reading a file and extendind actual database 
     * filename: file to read from.
     *)

    let extend_database filename =                          
        try
            let source      = open_in filename in           (* input channel *)
            let file_length = in_channel_length source in   (* total file length *)
            let buffer      = String.create file_length     (* buffer we read into *)
            in begin    
                    really_input source buffer 0 (file_length - 2);     (* reading from file, forcing to read everything into buffer *)
                    database :=                                         (* extending database with parsed input *) 
                        (Parser.clause_list Lexer.token (Lexing.from_string
                        buffer)) @ !database;
                        print_endline "ok";
               end
        with 
            | Sys_error s -> print_endline ((Filename.basename filename)^ ": " ^ s)        (* case of system error *)
            | End_of_file -> print_endline ((Filename.basename filename)^ 
                        ": could not read from file" )                                     (* shouldn't happen, but who knows *)
            | Lexer.EOF   -> ()                                                            (* lexer has nothing to lex left *)
     (*     |     _       -> print_endline ((Filename.basename filename) ^ ": " ^ " Error occured.") (* handling other cases *) *)
    in

    begin
        let parameters = match Array.length params with              (* the first argument of the program is it's name *)
                            |  1  -> Array.make 0 ""                 (* omit the first parameter - case it's the only one *)
                            |  i  -> Array.sub params 1 (i - 1)      (* omit the first parameter - case of more than 1 parameter *)
        in
            Array.iter (fun filename -> if Sys.file_exists filename         (* check if the file in param exists in filesystem *)
                                        then extend_database filename       (* try to extend database with file contents *)
                                        else print_endline                  (* warn user *)
                                                ("File " ^ (Filename.basename filename) ^ " does not exist."))   
                        parameters;
            print_int (List.length !database);
            !database                                           (* return created database *)
    end
;;


(*
 *  main interpreter function.
 *)
let main () =

    let database = read_database Sys.argv                       (* first, we have to read knowledge database *) 
    and buff     = ref (Lexing.from_string(""))                 (* then we run interpreter *)
    in 
    	try 
	   while (true) do					(* while not EOF *)
	    	print_string ":- ";				(* print prompt *)
                flush stdout;

                try
		    buff := Lexing.from_string(read_line());	(* read the expression *)
		    print_evaluation (evaluate (Parser.query Lexer.token !buff));    (* create it's syntax tree*)		    
                with
                    | Lexer.EOF -> print_endline "L-EOF!"       (* lexer finished his job on this input *)       
                     
                    | Failure ("lexing: empty token")           (* lexing failure *)
                    | Parsing.Parse_error ->                    (* parsing failure *)
                                    print_endline "Parse error. Did you forget a dot?"
                    | Failure s -> print_endline ("Failed: " ^ s) 
           done
	with
	    | End_of_file -> (print_string "\n"; exit 0)
            (*|   Failure s -> (print_endline s; exit 0)*)
           (* |      _      -> (print_endline "Error occured."; exit 0)*)
;;

let _ = main ()  
             
