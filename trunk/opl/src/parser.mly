%{

(* --- Header --- *)

        open Types;;
   
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
%token NOT
%token TERM_EQ TERM_INEQ IS TERM_DECOMP TERM_UNIFY TERM_NOTUNIFY
       ARITH_EQ ARITH_INEQ ARITH_LESS ARITH_GREATER ARITH_GEQ
       ARITH_LEQ TERM_ORDER_EQ TERM_ORDER_INEQ TERM_ORDER_GREATER
       TERM_ORDER_LESS TERM_ORDER_GEQ TERM_ORDER_LEQ
%token DOUBLECOLON
%token PLUS MINUS
%token MULT DIV INTDIV LEFT_SHIFT RIGHT_SHIFT 
       REM MOD DIVS MODS DIVU MODU POWER BITWISE_AND
       BITWISE_OR BITWISE_NOT VAR_INSTANTIATED
%token SEMICOLON COMMA COLON
%token LPAREN RPAREN 

%left SEMICOLON COMMA
%left PLUS MINUS
%left MULT DIV INTDIV LEFT_SHIFT RIGHT_SHIFT
%left BITWISE_AND BITWISE_OR BITWISE_NOT POWER
%left TERM_EQ 
%left TERM_UNIFY
%left TERM_INEQ 
%left TERM_NOTUNIFY
%left ARITH_EQ ARITH_INEQ ARITH_LESS ARITH_GREATER
      ARITH_LEQ ARITH_GEQ
%left TERM_ORDER_EQ TERM_ORDER_INEQ TERM_ORDER_LESS TERM_ORDER_GREATER
      TERM_ORDER_GEQ TERM_ORDER_LEQ
%left IS 
%left TERM_DECOMP
%left DOUBLECOLON
%right ARROW
%right REM MOD DIVS MODS DIVU MODU
%nonassoc COLONHYPHEN VAR_INSTANTIATED
%nonassoc DOT COLON

%type <Types.term list> sentence_list
%start sentence_list

%type <Types.term> query
%start query

/* grammar rules */

%%

sentence_list: 
    | clause DOT sentence_list 
    { 
        print_endline "multiclause"; 
        $1 :: $3 
    }
    | clause DOT 
    { 
        print_endline "single_clause"; 
        [$1] 
    } 
;

query:
    | clause DOT 
    { 
        print_endline "query clause"; 
        $1 
    }
;

clause:
    | head COLONHYPHEN body 
    { 
        print_endline "head :- body"; 
        $1 
    }
    | head
    { 
        print_endline "head";
        $1 
    }
;

head:
    | goal
    {
        print_endline "head goal";
        $1 
    }
;

/* we let only boolean operators appear in body part */

body:
    | goal SEMICOLON body 
    { 
        print_endline "goal ; body";
        $1 
    }
    | goal COMMA body 
    { 
        print_endline "goal , body";
        $1 
    }
    | goal 
    { 
        print_endline "body goal"; 
        $1 
    }
;

goal:
    | term
    {
        print_endline "goal"; 
        $1 
    }
;

term:
    | term0 { $1 }
;

term0:
    | term1 { $1 }
;

term1:
    | term2 { $1 }
;

term2:
    | term2 ARROW term3 
    { 
        print_endline "term2 -> term3"; 
        Types.TermIfThen ($1, $3)  
    } 
    | term2 ARROW term3 COLON term3 
    {
        print_endline "term2 -> term3; term4"; 
        Types.TermIfThenElse ($1, $3, $5)
    }
    | term3 
    { 
        $1 
    }
;

term3: 
    | term4 
    { 
        $1 
    }
;

term4:
    | NOT term5 
    { 
        Types.TermNegation $2
    }
    | term5 
    { 
        $1 
    }
;

term5:
    | term5 ARITH_EQ term5
    {
        Types.TermArithmeticEquality ($1, $3)
    }
    | term5 ARITH_INEQ term5
    {
        Types.TermArithmeticInequality ($1, $3)
    }
    | term5 TERM_UNIFY term5
    {
        Types.TermTermUnify ($1, $3)
    }
    | term5 TERM_NOTUNIFY term5
    {
        Types.TermTermNotUnify ($1, $3)
    }
    | term5 TERM_EQ term5 
    {
        Types.TermTermEquality ($1, $3)
    }
    | term5 TERM_INEQ term5
    {
        Types.TermTermInequality ($1, $3)
    }
    | term5 IS term5 
    {
        Types.TermIs ($1, $3) 
    }
    | term5 TERM_DECOMP term5
    {
        Types.TermDecomposition ($1, $3)
    } 
    | term5 ARITH_GEQ term5 
    {
        Types.TermArithmeticGeq ($1, $3)
    }
    | term5 ARITH_LEQ term5 
    {
        Types.TermArithmeticLeq ($1, $3)
    }
    | term5 ARITH_LESS term5
    {
        Types.TermArithmeticLess ($1, $3)
    }
    | term5 ARITH_GREATER term5
    {
        Types.TermArithmeticGreater ($1, $3)
    }
    | term5 TERM_ORDER_EQ term5
    {
        Types.TermTermOrderEquality ($1, $3)
    }
    | term5 TERM_ORDER_INEQ term5
    {
        Types.TermTermOrderInequality ($1, $3)
    }
    | term5 TERM_ORDER_LESS term5
    {
        Types.TermTermOrderLess ($1, $3)
    }
    | term5 TERM_ORDER_GREATER term5
    {
        Types.TermTermOrderGreater ($1, $3)
    }
    | term5 TERM_ORDER_GEQ term5
    { 
        Types.TermTermOrderGeq ($1, $3)
    }
    | term5 TERM_ORDER_LEQ term5
    {
        Types.TermTermOrderLeq ($1, $3)
    }
    | term6 
    { 
        $1 
    }
;

term6:
    | term6 DOUBLECOLON term6 
    {
        Types.TermModule ($1, $3)
    }
    | term7 { $1 }
;

term7:
    | term7 PLUS term7
    {
        Types.TermArithmeticPlus ($1, $3)
    }
    | term7 MINUS term7
    {
        Types.TermArithmeticMinus ($1, $3)
    } 
    | term8 { $1 }
;

term8:
    | term8 REM term8
    {
        Types.TermArithmeticRemainder ($1, $3)
    }
    | term8 MOD term8
    {
        Types.TermArithmeticModulo ($1, $3)
    }
    | term8 DIVS term8
    {
        Types.TermArithmeticDivs ($1, $3)
    }
    | term8 MODS term8
    {
        Types.TermArithmeticMods ($1, $3)
    }
    | term8 DIVU term8
    {
        Types.TermArithmeticDivu ($1, $3)
    }
    | term8 MODU term8
    {
        Types.TermArithmeticModu ($1, $3)
    }
    | term8 DIV term8
    {
        Types.TermArithmeticDiv ($1, $3)
    }
    | term8 INTDIV term8
    {
        Types.TermArithmeticIntDiv ($1, $3)
    }
    | term8 MULT term8
    {
        Types.TermArithmeticMult ($1, $3)
    }
    | term8 RIGHT_SHIFT term8
    {
        Types.TermArithmeticRightShift ($1, $3)
    }
    | term8 LEFT_SHIFT term8
    {
        Types.TermArithmeticLeftShift ($1, $3)
    }
    | term8 POWER term8
    {
        Types.TermArithmeticPower ($1, $3)
    }
    | VAR_INSTANTIATED term8
    {
        Types.TermVariableInstantiated $2
    }
    | term8 BITWISE_AND term8
    {
        Types.TermBitwiseAnd ($1, $3)
    }
    | term8 BITWISE_OR term8
    {
        Types.TermBitwiseOr ($1, $3)
    }
    | term8 BITWISE_NOT term8
    {
        Types.TermBitwiseNot ($1, $3)
    }
    | term9 { $1 }
;

term9:
    | term10 
    {
        print_endline "term10";
        $1 
    }
;

term10:
    | LPAREN term0 RPAREN 
    { 
        print_endline "(term0)"; 
        $2 
    }
    | STRING 
    { 
        print_endline "string"; 
        Types.TermString $1 
    }
    | constant 
    { 
        print_endline "constant"; 
        Types.TermConstant $1 
    }
    | VARIABLE 
    { 
        print_endline "variable"; 
        Types.TermVariable $1 
    }
    | functor_name LPAREN arguments RPAREN 
    { 
        print_endline "functor(arguments)";
        Types.TermFunctor ($1, $3) 
    }
;

functor_name:
    | name 
    { 
        print_endline "functor name"; 
        $1
    } 
;

arguments:
    | term0 COMMA arguments 
    { 
        print_endline "term10, arguments"; 
        ($1) :: ($3) 
    }
    | term0 
    { 
        print_endline "term10";
        [$1] 
    }
;

constant:
    | atom 
    { 
        print_endline "atom";
        ConstantAtom $1 
    }
    | number 
    {
        print_endline "number"; 
        ConstantNumber $1 
    }
;

atom: 
    | name
    { 
        print_endline "atom name"; 
        $1 
    }
;

name:
    | NAME 
    { 
        print_endline "name";
        $1 
    }
;

number:
    | FLOATNUMBER
    {
        print_endline "float"; 
        Types.Float $1 
    }
    | INTEGERNUMBER 
    { 
        print_endline "int"; 
        Types.Integer $1 
    }
%%  


(* --- Trailer --- *)

 

