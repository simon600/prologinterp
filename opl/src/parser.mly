%{

(* --- Header --- *)

        open Types;;

        (* predefined operators list.  
         * semantics at: 
         * http://www.trinc-prolog.com/doc/pl_p50.htm
         * http://www.amzi.com/manuals/amzi7/pro/ref_math.htm
         *)       

        let operators_table = Hashtbl.create 16;;
        let _ = List.iter (fun (name, op) -> Hashtbl.add operators_table name op)
                [ 
                  (":-", Types.XFX (":-", 0));         (* implication (goal :- subgoals) *)
                  (";",  Types.XFY (";", 1));           (* logical or *)
                  ("->", Types.XFY ("->", 2));          (* if term1 then term2 *)
                  (",",  Types.XFY (",", 3));           (* logical and *)
                  ("not", Types.FY ("not", 4));         (* logical not *)
                  ("=",  Types.XFX ("=", 5));           (* unify terms *)
                  ("\\=", Types.XFX ("\\=", 5));        (* true, if terms are not unifiable *)
                  ("is",  Types.XFX ("is", 5));         (* evaluate term2, evaluate term1, then unify *)
                  ("=..",  Types.XFX ("=..", 5));       (* term composition/decomposition *)
                  ("==",  Types.XFX ("==", 5));         (* test of term equality *)
                  ("\\==",  Types.XFX ("\\==", 5));     (* test for term inequality *)
                  ("=:=", Types.XFX ("=:=", 5));        (* arithmetical equality *)
                  ("=\\=", Types.XFX ("=\\=", 5));      (* arithmetical inequality *)
                  ("<", Types.XFX ("<", 5));            (* arithmetical less than *)
                  (">", Types.XFX ("=<", 5));           (* arithmetical greater than *)
                  (">=", Types.XFX (">=", 5));          (* arithmetical greater or equal to *)
                  ("<=", Types.XFX ("<=", 5));          (* arithmetical less or equal to *)
                  ("@=", Types.XFX ("@=", 5));          (* term1 equal term2 (order of terms) *)
                  ("@<", Types.XFX ("@<", 5));          (* term1 less than term2 (order of terms) *)
                  ("@>", Types.XFX ("@>", 5));          (* term1 greater than term2 (order of terms) *)
                  ("@=<", Types.XFX ("@=<", 5));        (* term1 less or equal than term2 (order of terms) *)
                  ("@>=", Types.XFX ("@>=", 5));        (* term1 greater or equal than term2 (order of terms) *)
                  ("@\\=", Types.XFX("@!=", 5));        (* term inequality *)
                  (":", Types.XFY (":", 6));            (* namespace operator. ex.: module_name:module_elem *)
                  ("+", Types.YFX ("+", 7));            (* arithmetical addition *)
                  ("-", Types.YFX ("-", 7));            (* arithmetical minus *)
                  ("rem", Types.YFX ("rem", 8));        (* integer remainder *)
                  ("mod", Types.YFX ("mod", 8));        (* integer modulo *)
                  ("divs", Types.YFX ("divs", 8));      (* integer division, rounded answer *)
                  ("mods", Types.YFX ("mods", 8));      (* remainder corresponding to divs *)
                  ("divu", Types.YFX ("divu", 8));      (* integer division, truncated answer *)
                  ("modu", Types.YFX ("modu", 8));      (* remainder corresponding to divu *)
                  ("/", Types.YFX ("/", 8));            (* arithmetical division *)
                  ("//", Types.YFX ("//", 8));          (* integer division *) 
                  ("*", Types.YFX ("*", 8));            (* arithmetical multiplication *)
                  (">>", Types.YFX (">>", 8));          (* bit shift to the right (division by 2) *)
                  ("<<", Types.YFX ("<<", 8));          (* bit shift to the left (multiply by 2) *)
                  ("**", Types.XFX ("**", 8));          (* power *)
                  ("^", Types.XFY ("^", 8));            (* this-implementation-specific: is the variable instantiated *)
                  ("/\\", Types.YFX ("/\\", 8));        (* bitwise and *)
                  ("\\/", Types.YFX ("\\/", 8));        (* bitwise or *)
                  ("\\"), Types.FX ("\\", 8);           (* bitwise negation *)
                 ]
        ;;
   
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

%type <Types.term list> sentence_list
%start sentence_list


/* grammar rules */

%%

sentence_list: 
    | clause DOT sentence_list { $1 :: $3 }
    | clause DOT { [$1] } 
;

clause:
    | head COLONHYPHEN body { $1 }
    | head { $1 }
;

head:
    | goal { $1 }
;

body:
    | goal ARROW body { $1 }
    | goal SEMICOLON body { $1 }
    | goal COMMA body { $1 }
    | goal { $1 }
;

goal:
    | term { $1 }
;

term:
    | term10 { $1 }
;

term10:
    | term9 { $1 }
;

term9:
    | term8 { $1 }
;

term8:
    | term7 { $1 }
;

term7: 
    | term6 { $1 }
;

term6:
    | term5 { $1 }
;

term5:
    | term4 { $1 }
;

term4:
    | term3 { $1 }
;

term3: 
    | term2 { $1 }
;

term2:
    | term1 { $1 }
;

/* the semantic action below should change according to the way assignment is solved */

term1:
    | VARIABLE IS arithmetic { ignore $1; Types.TermConstant (Types.ConstantNumber $3) }
    | term0 { $1 }
;

term0:
    | LPAREN term10 RPAREN { $2 }
    | STRING { Types.TermString $1 }
    | constant { Types.TermConstant $1 }
    | VARIABLE { Types.TermVariable $1 }
    | functor_name LPAREN arguments RPAREN { Types.TermFunctor ($1, $3) }
;

arithmetic:
    | arithmetic0 { $1 }
;

arithmetic0:
    | arithmetic1 PLUS arithmetic1 
    { 
        match $1, $3 with 
          | Types.Float f1, Types.Float f2 -> Types.Float (f1 +. f2)
          | Types.Float f1, Types.Integer i2 -> Types.Float (f1 +. float_of_int i2)
          | Types.Integer i1, Types.Float f2 -> Types.Float (float_of_int i1 +. f2)
          | Types.Integer i1, Types.Integer i2 -> Types.Integer ( i1 + i2 )
    }

    | arithmetic1 MINUS arithmetic1 
    { 
        match $1, $3 with 
          | Types.Float f1, Types.Float f2 -> Types.Float (f1 -. f2)
          | Types.Float f1, Types.Integer i2 -> Types.Float (f1 -. float_of_int i2)
          | Types.Integer i1, Types.Float f2 -> Types.Float (float_of_int i1 -. f2)
          | Types.Integer i1, Types.Integer i2 -> Types.Integer ( i1 - i2 )
    }

    | arithmetic1 { $1 }
;

arithmetic1:
    | arithmetic2 MULT arithmetic1
    { 
        match $1, $3 with 
          | Types.Float f1, Types.Float f2 -> Types.Float (f1 *. f2)
          | Types.Float f1, Types.Integer i2 -> Types.Float (f1 *. float_of_int i2)
          | Types.Integer i1, Types.Float f2 -> Types.Float (float_of_int i1 *. f2)
          | Types.Integer i1, Types.Integer i2 -> Types.Integer ( i1 * i2 )
    }

    | arithmetic2 DIV arithmetic1
    { 
        match $1, $3 with 
          | Types.Float f1, Types.Float f2 -> Types.Float (f1 /. f2)
          | Types.Float f1, Types.Integer i2 -> Types.Float (f1 /. float_of_int i2)
          | Types.Integer i1, Types.Float f2 -> Types.Float (float_of_int i1 /. f2)
          | Types.Integer i1, Types.Integer i2 -> Types.Integer ( i1 / i2 )
    }

    | arithmetic2 { $1 }
;

arithmetic2:
    | number { $1 }
    | LPAREN arithmetic0 RPAREN { $2 } 
;

functor_name:
    | name { $1 } 
;

arguments:
    | term10 COMMA arguments { ($1) :: ($3) }
    | term10 { [$1] }
;

constant:
    | atom { ConstantAtom $1 }
    | number { ConstantNumber $1 }
;

atom: 
    | name { $1 }
;

name:
    | NAME { $1 }
;

number:
    | FLOATNUMBER { Types.Float $1 }
    | INTEGERNUMBER { Types.Integer $1 }
%%  


(* --- Trailer --- *)

 

