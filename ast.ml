open Sexplib.Std

type program = Program of stmt list
(*and funcdef = Def of (string list) * suite*)
and stmt = Smpl_stmt of simple_stmt 
  (* | Cmpnd_stmt of compound_stmt*)
and simple_stmt = Single of small_stmt 
  (*| Begin of small_stmt list*)
and small_stmt = Expr_stmt of expr_stmt
                                (* more to come.... *)
and expr_stmt = Assignment of assign_op * (test list) * tuple_or_test
  | ToT of tuple_or_test
and assign_op = Pluseq | Minuseq | Stareq | Slasheq | Percenteq
| Ampeq | Pipeeq | Careteq | Dlteq | Dgteq | Dstareq | Dslasheq
             (* lots of stuff missing here... *)
and test = If_test of or_test * or_test * (test list)
  | Or_test of or_test
  (* lambdadef *)
and or_test = Or of and_test list
and and_test = And of not_test list
and not_test = Comp of comparison | Not of not_test
and comparison = Cmp_star_exp of star_exp | Cmp_cmp of star_exp * (comp_op * star_exp) list
and comp_op = LT | GT | EQEQ | GTEQ | LTEQ | LTGT | NOTEQ | IN | NOTIN | IS | ISNOT
and star_expr = Starexp_exp of expr | Starexp_sexp of expr
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

