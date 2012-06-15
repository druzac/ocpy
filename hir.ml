(* The types for HIR *)
type name = string
and var = name
and number = int
and binop = Bplus | Bminus
and unop = Uplus | Uminus
and multop = BitAnd | BitOr | BitXor

type exp = Void
	   | Error of exp
	   | Lambda of (name list) * exp
	   | CallEC of name * exp
	   | Var of name
	   | Number of number
	   | String of string
	   | IsInt
	   | IsString
	   | IsTuple
	   | IsDict
	   | IsPList
	   | IsSet
	   | Set of (exp list)
	   | Dict of (exp * exp) list
	   | Tuple of (exp list)
	   | PList of exp list
	   | Let of (var * exp) list * (exp list)
	   | Setq of var * exp
	   | PListRef of exp * exp
	   | PListSet of exp * exp * exp
	   | PListRem of exp * exp
	   | TupleRef of exp * exp
	   | TupleSet of exp * exp * exp
	   | DictRef of exp * exp
	   | DictSet of exp * exp * exp
	   | DictRem of exp * exp
	   | FieldGet of exp * var
	   | FieldSet of exp * exp * var
	   | FieldRem of exp * var
	   | GlobalGet of var
	   | GlobalSet of var * exp
	   | Throw of exp
	   | Try of exp * exp
	   | Assert of exp * (exp option)
	   | Cond of (exp * exp) list * (exp option)
	   | If of exp * exp * exp
	   | And of (exp list)
	   | Or of (exp list)
	   | Not of exp
	   | While of exp * exp * (exp option)
	   | For of var * exp * exp * (exp option)
	   | Break
	   | Continue
	   | Begin of exp list
	   | Binop of binop * exp * exp
	   | Unop of unop * exp
	   | Multop of multop * (exp list)
	   | PPrint | Exception | Object | None | Ellipsis | True | False
		 (* confused about function application, so I'm adding this *)
	   | App of exp * (exp list)

type topForm = Define of var * exp
	       | Exp of exp
