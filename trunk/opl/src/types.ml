
(* --- Prolog syntax elements constructors --- *)

type term =                             (* prolog term types *)
  | TermString of string                (* prolog string term *)
  | TermConstant of constant            (* prolog constant term *)
  | TermVariable of name                (* prolog variable terms *)
  | TermFunctor of name * arguments     (* functor term *)

and arguments = term list               (* functor arguments *)

and constant =  
  | ConstantAtom of name                (* constants: atom *)
  | ConstantNumber of number            (* constant: number *)

and number =
  | Float of float      (* prolog float *)
  | Integer of int      (* prolog integers *)

and name = string;;     (* prolog names *)



(* --- Types of operators in Prolog --- *)

type operator = 

  (* X has precedence of less than f *)
  (* Y has precedence of less or equal to f *)
  
  | FX of name * precedence     (* prefix, no associativity *)
  | FY of name * precedence     (* prefix, right-associative *)
  | XF of name * precedence     (* postfix, no associativity *)
  | YF of name * precedence     (* postfix, left-associative *)
  | XFX of name * precedence    (* infix, no associativity *)
  | XFY of name * precedence    (* infix, right-associative *)
  | YFX of name * precedence    (* infix, left-associative *)

and precedence = int    (* operators precedence *)
;;
