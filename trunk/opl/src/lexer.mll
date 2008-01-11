(* header section *)
{
        open Parser;;
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
let solo_char = ['!' ';']               

let name = quoted_name | word | symbol | solo_char       (* valid prolog names *)

let variable = (capital | underline) alpha*              (* prolog variables *)

let nstring = '"' [^ '"'] '"'                            (* prolog strings *)

let sign = '+' | '-'                            (* signs *)
let exp = ('e' | 'E') sign? digit+              (* optional exponent *)
let simple_float = digit* '.' digit+            (* simplest float *)
let simple_integer = digit+                     (* simplest integer *)
let float_number = sign? simple_float exp?      (* valid prolog float *)
let integer_number = sign? simple_integer exp?  (* valid prolog integer *)

let multiline_comment = "/*" ( [^'*'] [^'\\'] )* "*/"     (* multiline comment *)
let single_line_comment = '%' [^'\n']* '\n'               (* single line comment *)
let comment = multiline_comment | single_line_comment     (* valid prolog comments *)

let whitespace = [' ' '\t' '\n']                (* whitespaces we allow in text *)

rule token = parse
        | comment               
        {       
                print_endline "/* */";
                token lexbuf    
        }

        | whitespace            
        {       token lexbuf    }

        | "." { DOT }  
        
        | float_number          
        {
                print_string "FLOAT: ";
                print_endline (Lexing.lexeme lexbuf);  
                FLOATNUMBER (float_of_string (Lexing.lexeme lexbuf)) 
        } 

        | integer_number        
        { 
                print_string "INT: ";
                print_endline (Lexing.lexeme lexbuf); 
                INTEGERNUMBER (int_of_string (Lexing.lexeme lexbuf)) 
        }

        | "is"  { IS }
        | "+"   { PLUS }
        | "-"   { MINUS }
        | "*"   { MULT }
        | "/"   { DIV }
        | '('   { LPAREN }
        | ')'   { RPAREN }
        | ":-"  { COLONHYPHEN }

        | name                  
        {       
                print_string "NAME: ";
                print_endline (Lexing.lexeme lexbuf);
                NAME (Lexing.lexeme lexbuf)     
        }
        
        | nstring                
        {       
                print_string "STRING: ";
                print_endline (Lexing.lexeme lexbuf);
                STRING (Lexing.lexeme lexbuf)
        }
        
        | variable              
        {       
                print_string "VAR: ";
                print_endline (Lexing.lexeme lexbuf);
                VARIABLE (Lexing.lexeme lexbuf) 
        }

(* trailer section *)
{
}

