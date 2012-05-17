open Printf

type tree = T of expr list
and expr = SUM of expr * factor | DIFF of expr * factor | E_F of factor
and factor = F_R of result | PROD of factor * result
and result = R of int

let sandwich s meat =
  printf "(";
  printf s;
  printf " ";
  meat ();
  printf ")";;

let rec print_t = function
    T(exprs) -> sandwich "ast" (fun () -> (List.iter (fun e -> print_e e) exprs))

and print_e = function
    SUM(e,f) -> sandwich "sum" (fun () -> print_e e; print_f f)
  |DIFF(e,f) -> sandwich "diff" (fun () -> print_e e; print_f f)
  |E_F f     -> sandwich "expr" (fun () -> print_f f)

and print_f = function
    F_R r -> sandwich "factor" (fun () -> print_r r)
  | PROD(f, r) -> printf "(prod ";
                  print_f f;
                  print_r r;
                  printf ")";
and print_r = function
    R num -> print_int num;
