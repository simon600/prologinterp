%{

(* --- Header --- *)
   open Lexer;;
 
%}

/* ocamlyacc declarations */

%token <string> STRING 
%token <string> CONSTANT 
%token <string> VARIABLE
%token <string> NAME
%token <float> FLOATNUMBER 
%token <int> INTEGERNUMBER
%token DOT
%token COLONHYPHEN
%token ARROW 
%token SEMICOLON COMMA
%token PLUS MINUS
%token MULT DIV
%token IS
%token LPAREN RPAREN 

%left SEMICOLON COMMA
%left PLUS MINUS
%left MULT DIV
%right ARROW
%nonassoc COLONHYPHEN
%nonassoc DOT

%type <unit> sentence
%start sentence


/* grammar rules */

%%

sentence: 
    | clause DOT {()} 
;

clause:
    | head COLONHYPHEN body {}
    | head {}
;

head:
    | goal {}
;

body:
    | goal ARROW body {}
    | goal SEMICOLON body {}
    | goal COMMA body {}
    | goal {}
;

goal:
    | term {}
;

term:
    | term3 {}
;

term3: 
    | term2 {}
;

term2:
    | term1 {}
;

term1:
    | VARIABLE IS arithmetic {ignore $1 }
    | term0 {}
;

term0:
    | LPAREN term3 RPAREN { }
    | STRING { ignore $1 }
    | constant { ignore $1 }
    | VARIABLE { ignore $1 }
    | functorr LPAREN arguments RPAREN {}
;

arithmetic:
    | arithmetic0 {}
;

arithmetic0:
    | arithmetic1 PLUS arithmetic1 {}
    | arithmetic1 MINUS arithmetic1 {}
    | arithmetic1 {}
;

arithmetic1:
    | arithmetic2 MULT arithmetic1 {}
    | arithmetic2 DIV arithmetic1 {}
    | arithmetic2 {}
;

arithmetic2:
    | INTEGERNUMBER {}
    | FLOATNUMBER  {}
    | LPAREN arithmetic0 RPAREN {} 
;

functorr:
    | name {} 
;

arguments:
    | term3 COMMA arguments {}
    | term3 {}
;

constant:
    | atom {}
    | number {}
;

atom: 
    | name {}
;

name:
    | NAME {ignore $1}
;

number:
    | FLOATNUMBER {ignore $1}
    | INTEGERNUMBER {ignore $1}
%%  


(* --- Trailer --- *)

 

