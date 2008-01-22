(* header section *)
{
        open Parser;;

        exception EOF;;         (* raised when parsing ends *)

        (* predefined operators list.  
         * semantics at: 
         * http://www.trinc-prolog.com/doc/pl_p50.htm
         * http://www.amzi.com/manuals/amzi7/pro/ref_math.htm
         *)   

        let keywords = Hashtbl.create 32;;
   
        let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok)
            [(".",      DOT);                   
             ("rem",    REM);
             ("mod",    MOD);
             ("divs",   DIVS);
             ("divu",   DIVU);
             ("mods",   MODS);
             ("modu",   MODU);
             ("not",    NOT);
             ("=:=",    ARITH_EQ);
             ("=\\=",   ARITH_INEQ);
             ("->",     ARROW);
             ("\\=",    TERM_NOTUNIFY);
             ("=..",    TERM_DECOMP);
             ("==",     TERM_EQ);
             ("@=<",    TERM_ORDER_LEQ);
             ("@>=",    TERM_ORDER_GEQ);
             ("@=",     TERM_ORDER_EQ);
             ("@\\=",   TERM_ORDER_INEQ);
             ("@<",     TERM_ORDER_LESS);
             ("@>",     TERM_ORDER_GREATER);
             ("**",     POWER);
             (">=",     ARITH_GEQ);
             ("<=",     ARITH_LEQ);
             ("//",     INTDIV);
             ("<<",     LEFT_SHIFT);
             (">>",     RIGHT_SHIFT);
             ("is",     IS);
             ("::",     DOUBLECOLON);
             ("\\/",    BITWISE_AND);
             ("/\\",    BITWISE_OR);
             ("\\",     BITWISE_NOT);
             ("^",      VAR_INSTANTIATED);
             ("+",      PLUS);
             ("-",      MINUS);
             ("*",      MULT);
             ("/",      DIV);
             ("(",      LPAREN);
             (")",      RPAREN);
             (":",      COLON);
             (",",      COMMA);
             (";",      SEMICOLON);
             ("=",      TERM_UNIFY);
             ("<",      ARITH_LESS);
             (">",      ARITH_GREATER);
             ("!",      CUT);
             (":-",     COLONHYPHEN);
             ("..",     DOUBLEDOT)]
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
let symbol = ['+' '-' '*' '/' '\\' '^' '<' '>' '=' '~' ':' '?' '@' '#' '$' '&' '.'] 
let solo_char = ['!' ';']               

let name = quoted_name | word | symbol+ | solo_char      (* valid prolog names *)

let variable = (capital | underline) alpha*              (* prolog variables *)

let nstring = '"' [^ '"'] '"'                            (* prolog strings *)

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

        | float_number          
        {       FLOATNUMBER (float_of_string (Lexing.lexeme lexbuf))    } 

        | integer_number        
        {       INTEGERNUMBER (int_of_string (Lexing.lexeme lexbuf))    }

        | name as id            
        {       
                try
                    Hashtbl.find keywords id
                with
                    | Not_found -> NAME (id) 
        }
        
        | nstring                
        {       STRING (Lexing.lexeme lexbuf)           }
        
        | variable              
        {       VARIABLE (Lexing.lexeme lexbuf)         }

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

