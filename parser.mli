type token =
  | NEWLINE
  | INDENT
  | DEDENT
  | ENDMARKER
  | INDENT_SPACE of (int)
  | FALSE
  | CLASS
  | FINALLY
  | IS
  | RETURN
  | NONE
  | CONTINUE
  | FOR
  | LAMBDA
  | TRY
  | TRUE
  | DEF
  | FROM
  | NONLOCAL
  | WHILE
  | AND
  | DEL
  | GLOBAL
  | NOT
  | WITH
  | AS
  | ELIF
  | IF
  | OR
  | YIELD
  | ASSERT
  | ELSE
  | IMPORT
  | PASS
  | BREAK
  | EXCEPT
  | IN
  | RAISE
  | PLUS
  | MINUS
  | STAR
  | DSTAR
  | SLASH
  | DSLASH
  | PERCENT
  | DBCHEVRON
  | DFCHEVRON
  | AMP
  | PIPE
  | CARET
  | TILDE
  | BCHEVRON
  | FCHEVRON
  | LTEQ
  | GTEQ
  | EQEQ
  | EXEQ
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | DOT
  | SEMICOLON
  | ATSYM
  | EQ
  | PLUSEQ
  | MINEQ
  | STAREQ
  | SLASHEQ
  | DSLASHEQ
  | PERCENTEQ
  | AMPEQ
  | PIPEEQ
  | CARETEQ
  | DFCHEVRONEQ
  | DBCHEVRONEQ
  | DSTAREQ
  | ELLIPSIS
  | STRING of (string)
  | NAME of (string)
  | NUMBER of (int)

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
