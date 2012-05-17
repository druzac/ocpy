type token =
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EOF
  | NEWLINE
  | NUMBER of (int)

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Syntax.tree
