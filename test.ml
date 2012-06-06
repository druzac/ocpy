open Ast

let bpath f_name = "./tests/" ^ f_name;;

let path = "./tests/";; 

let ass left right =
  assert (left = right)

let pr_msg test_name =
  Printf.printf "Passed %s test!\n" test_name


let parse_file f =
  let lexer = Lexer.gen_lexer () 
  and lexbuf = Lexing.from_channel (open_in f) in
    Parser.start lexer lexbuf

let get_tree f_name = parse_file (bpath f_name);;

let test exp f_name t_name =
  try let ast = parse_file (path ^ f_name) in
    if ast = exp then Printf.printf "Test %s passed\n" t_name
    else Printf.printf "Test %s failed\n" t_name
  with Parsing.Parse_error -> Printf.printf "Failed to parse %s test\n" t_name
    | Sys_error msg -> print_string msg; print_string "\n"


(* builder functions for the ast *)
let int2atom n = Number n
let string2atom s = String s
let name2atom name = Name name

let ind atom = function
    [] -> Atom atom
  | trailers -> Indexed (atom, trailers)

let stmt2prog stmts = Program stmts

let smpl2prog smpl = stmt2prog [Simple smpl]

let smalls2prog smalls = 
  let smpl = match smalls with 
      [smstmt] -> Single (smstmt)
    | smlls -> Begin smlls
  in 
    smpl2prog smpl

let tot2prog tot = smalls2prog [Expr tot]

let tests2prog tests = 
  let tot = match tests with
      [t] -> Test t
    | ts -> Tuple ts
  in tot2prog tot

let or2prog o = tests2prog [Or_test o]

let ands2prog ands = or2prog (Or ands)

let nots2prog nots = ands2prog [And nots]

let stexps2prog ste comps = nots2prog [(Comparison (ste, comps))]

let expr2prog e = stexps2prog (Star_exp e) []

let xor2prog x = expr2prog (Bitwise_or x)

let and2prog a = xor2prog [(Bitwise_xor a)]

let shift2prog shfts = and2prog [(Bitwise_and shfts)]

let arith2prog a shftas = shift2prog [Shift (a, shftas)]

let term2prog t aops_ts = arith2prog (Arith (t, aops_ts)) []

let fact2prog f fops_ts = term2prog (Term (f, fops_ts)) []

let pow2prog p = fact2prog (Fact_single p) []
  (* let uapp2prog uapp = *)

let ind2prog ind = pow2prog (Pow_single ind)

let atom2prog a = ind2prog (Atom a)

(* how can I structure these testing functions??? *)

(* empty test *)

let exp_e = Program [];;
let act_e = get_tree "empty";;

assert (exp_e = act_e);;

(* Atom tests *)

(* just 1 test *)

let a1 = Number 1;;
let exp1 = atom2prog a1;;
let act1 = get_tree "t_one";;

assert (act1 = exp1);;

(* str test *)

let str1 = String "oh"
(* this is a problem with the lexer.. *)

let expstr = atom2prog str1;;

let actstr = get_tree "str";;

test expstr "str" "str";;

(* str concat test *)

let strcat = String "ab"
let expstrcat = atom2prog strcat;;
test expstrcat "strcat" "concat";;

(* empty tuple test *)

let nil = Empty_tuple
let expnil = atom2prog nil;;
test expnil "nil" "empty tuple";;

(* name test *)

let name1 = Name "foo"
let expname = atom2prog name1;;
test expname "name" "name";;

(* ellipsis test *)

let ellp = Ellipsis
let expellp = atom2prog ellp;;
test expellp "ellipsis" "ellipsis"

(* none test *)

let astnone = Ast.None
let expnone = atom2prog astnone;;
test expnone "none" "none";;

(* true test *)
let asttrue = Ast.True
let exptrue = atom2prog asttrue;;
test exptrue "True" "true";;

(* false test *)
let astfalse = Ast.False
let expfalse = atom2prog astfalse;;
test expfalse "False" "false";;


(* testing for putting lists into proper order *)
(* indexed reverse *)

let indtrail = Indexed (name2atom "yak", [Dot "foo"; Dot "bar"]);;
let expind = ind2prog indtrail;;

test expind "indrev" "indrev";;

(* 
*)
