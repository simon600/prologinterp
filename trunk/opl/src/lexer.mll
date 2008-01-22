(* header section *)
{
        open Parser;;

        exception EOF;;         (* raised when parsing ends *)

        (* predefined operators list.  
         * semantics at: 
         * http://www.trinc-prolog.com/doc/pl_pred.htm
         * http://www.amzi.com/manuals/amzi7/pro/ref_math.htm
         *)   

        let keywords = Hashtbl.create 32;;
   
        let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok)
            [(".",      DOT);                   (* sentence delimiter*)          
             ("rem",    REM);                   (* division remainder *)
             ("mod",    MOD);                   (* modulo division *)
             ("divs",   DIVS);                  (* integer division, rounded answer *)
             ("divu",   DIVU);                  (* integer division, truncated answer *)
             ("mods",   MODS);                  (* integer modulo, corresponding to divs *)
             ("modu",   MODU);                  (* integer modulo, corresponding to divu *)
             ("not",    NOT);                   (* boolean negation *)
             ("=:=",    ARITH_EQ);              (* arithmetical equality *)
             ("=\\=",   ARITH_INEQ);            (* arithmetical inequality *)
             ("->",     ARROW);                 (* if then [else] *)
             ("\\=",    TERM_NOTUNIFY);         (* terms do not unify *)
             ("=..",    TERM_DECOMP);           (* term composition/decomposition *)
             ("==",     TERM_EQ);               (* term equality *)
             ("@=<",    TERM_ORDER_LEQ);        (* term less or equal to (order of terms) *)
             ("@>=",    TERM_ORDER_GEQ);        (* term greater or equal to (order of terms) *)
             ("@=",     TERM_ORDER_EQ);         (* term equality (order of terms) *)
             ("@\\=",   TERM_ORDER_INEQ);       (* term inequality (order of terms) *)
             ("@<",     TERM_ORDER_LESS);       (* term less than (order of terms) *)
             ("@>",     TERM_ORDER_GREATER);    (* term greater than (order of terms) *)
             ("**",     POWER);                 (* arithmetical power *)
             (">=",     ARITH_GEQ);             (* arithmetical greater or equal to *)
             ("<=",     ARITH_LEQ);             (* arithmetical less or equal to *)
             ("//",     INTDIV);                (* integer division *)
             ("<<",     LEFT_SHIFT);            (* bitwise left shift *)
             (">>",     RIGHT_SHIFT);           (* bitwise right shift *)
             ("is",     IS);                    (* variable instantiation *)
             ("::",     DOUBLECOLON);           (* module(database) specifier *)
             ("\\/",    BITWISE_AND);           (* bitwise and *)
             ("/\\",    BITWISE_OR);            (* bitwise or *)
             ("\\",     BITWISE_NOT);           (* bitwise not *)
             ("^",      VAR_INSTANTIATED);      (* is variable instantiated? *)
             ("+",      PLUS);                  (* arithmetical plus *)
             ("-",      MINUS);                 (* arithmetical minus *)
             ("*",      MULT);                  (* arithmetical multiplication *)
             ("/",      DIV);                   (* arithmetical division *)
             ("(",      LPAREN);                (* left parenthesis *)
             (")",      RPAREN);                (* right parenthesis *)
             (":",      COLON);                 (* else *)
             (",",      COMMA);                 (* logical and *)        
             (";",      SEMICOLON);             (* logical or *)
             ("=",      TERM_UNIFY);            (* unify terms *)
             ("<",      ARITH_LESS);            (* arithmetical less than *)
             (">",      ARITH_GREATER);         (* arithmetical greater than *)
             ("!",      CUT);                   (* cut operator *)
             (":-",     COLONHYPHEN);           (* logical implication *)
             ("..",     DOUBLEDOT);             (* database end *)
             ("[",      LBRACKET);              (* left bracket for lists *)
             ("]",      RBRACKET);              (* right bracket for lists *)
             ("|",      PIPE)]                  (* head-tail delimiter for lists *)
      ;;
        
}

(* definitions section *)

let capital = ['A'-'Z']         (* capital letters *)
let small = ['a'-'z']           (* small letters *)
let digit = ['0'-'9']           (* digits *)
let underline = ['_']           (* underline character *)

let alpha = capital | small | digit | underline          (* any alphanumeric character*)

let word = small alpha*                                  (* prolog words *)
let quoted_name = '\'' [^ '\''] '\''                     (* quoted names *)
let symbol = ['+' '-' '*' '/' '\\' '^' '<' '>' '=' '~' ':' '?' '@' '#' '$' '&'] 
let solo_char = ['!' ';' '.' '[' ']' '(' ')' ',' '|']               

let name = quoted_name | word | symbol+ | solo_char      (* valid prolog names *)

let variable = (capital | underline) alpha*              (* prolog variables *)

let nstring = '"' [^ '"']* '"'                           (* prolog strings *)

let sign = '+' | '-'                                     (* signs *)
let exp = ('e' | 'E') sign? digit+                       (* optional exponent *)
let simple_float = digit* '.' digit+                     (* simplest float *)
let simple_integer = digit+                              (* simplest integer *)
let float_number = sign? simple_float exp?               (* valid prolog float *)
let integer_number = sign? simple_integer exp?           (* valid prolog integer *)

let whitespace = [' ' '\t' '\n']

rule token = parse
        | eof
        {       raise EOF       }

        | whitespace 
        {       token lexbuf    }

        | '%' 
        {       single_line_comment lexbuf    }

        | "/*"
        {       multiline_comment 0 lexbuf    }

        | name as id            
        {       
                print_endline ("NAME rule -> <" ^ id ^ ">");

                try
                    Hashtbl.find keywords id
                with
                    | Not_found -> NAME (id) 
        }

        | float_number as fl         
        {       
                print_endline ("FLOAT rule -> <" ^ fl^ ">");
                flush stdout;
                        
                FLOATNUMBER (float_of_string (Lexing.lexeme lexbuf))    
        } 

        | integer_number as inte       
        {       
                print_endline ("INTEGER rule -> <" ^ inte^ ">");
                flush stdout;
                INTEGERNUMBER (int_of_string (Lexing.lexeme lexbuf))    } 

        | nstring as str               
        {
                print_endline ("STRING rule -> <" ^ str^ ">");
                STRING (Lexing.lexeme lexbuf)        
        }
        
        | variable as var               
        {
                print_endline ("VARIABLE rule -> <" ^ var^ ">");
                VARIABLE (Lexing.lexeme lexbuf)
        }

and single_line_comment = parse 
        | "\n" 
        {       token lexbuf    }
       
        | eof
        {       raise EOF       }

        |   _ 
        {       single_line_comment lexbuf       }

and multiline_comment level = parse
        | "*/"
        {       if level = 0 
                    then token lexbuf
                    else multiline_comment (level - 1) lexbuf    
        }
       
        | "/*"
        {       multiline_comment (level + 1) lexbuf    }

        | eof
        {       failwith "Unclosed comment!";           }

        |  _    
        {       multiline_comment level lexbuf          }




(* trailer section *)
{
}

