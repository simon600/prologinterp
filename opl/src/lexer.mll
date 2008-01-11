(* header section *)
{  
        open Parser;;
        open Printf;;

        let keyword_table = Hashtbl.create 72
        let _ = List.iter (fun (keyword, token) -> 
                        Hashtbl.add keyword_table keyword token)
                [ ]
}

(* definitions section *)

let layout_char = ['\t' '\n' ' ']
let small_letter = ['a'-'z']
let capital_letter = ['A'-'Z']
let digit = ['0'-'9']
let symbol_char = ['+' '-' '*' '/' '\' '^' '<' '>' '=' '`' '~' ':' '.' '?' '@' '#' '$' '&']
let solo_char = ['!' ';']
let punctuation_char = ['%' '(' ')' ',' '[' ']' '{' '|' '}' ]
let quote_char = ['"' ''']
let underline = ['_']

let term = parse
    | 't' { TERM }
;

(* trailer section *)
{

}

