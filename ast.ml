open Sexplib.Std

type program = Program of arith_expr list
(*and funcdef = DEF of string list * suite
and stmt = SIMPLE_STMT of simple_stmt | COMPOUND_STMT of compound_stmt
and simple_stmt = SINGLE of small_stmt | BEGIN of small_stmt list
and small_stmt = EXPR
                   ...*)

and expr = Exp of xor_expr list
and xor_expr = Xor_exp of and_expr list
and and_expr = And_exp of shift_expr list
and shift_expr = Shift of arith_expr * (shift_op * arith_expr) list
and shift_op = Dlt | Dgt


and arith_expr = Arith of term * (arith_op * term) list
and arith_op = Plus | Minus
and term = Term of factor * (factor_op * factor) list
and factor_op = Star | Fslash | Percent | Dfslash
and factor = Power of power | Uapp of unary_op * factor
and unary_op = Uplus | Uminus | Utilde
and indexed = IndAtom of atom * trailer list
and power = Pow_index of indexed | Pow_factor of indexed * factor
and atom = (*T_OR_T of tuple_or_test | TUPLE of tuple
         |  T_LST of testlist
         |  DCT of dict
         |  SET of set*)
         |  Name of string
         |  Number of number
         |  String of string
         |  Ellipsis
         |  None
         |  True
         |  False
and number = int
and trailer = unit
                with sexp
(*and trailer = CALLED of arglist 
            |  SUBSCRIPT of tuple_or_test
            |  DOT of string
and testlist = test list
(*and tuple_or_test = TEST of test | TUPLE of test list*)
and dict = (test * test) list 
and set = SSET of test list
and arglist = ARGLIST of test list   *)
let get_sexp p = sexp_of_program p

let rec print_sexp = function
    Sexplib.Sexp.List l -> print_string "("; List.iter print_sexp l; print_string ")"; 
  | Sexplib.Sexp.Atom a -> print_string a; print_string " "; 

