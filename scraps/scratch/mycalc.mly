
%{
type 

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS
%left TIME DIV

%nonassoc UMINUS
%start main

%type <int> main

%%
main:
    expr EOL    {$1}
;
expr:
    INT  {$1}
    | LPAREN expr RPAREN        {$2}
    | expr PLUS expr {$1 + $2}
    | expr MINUS expr {$1 - $2}
    | expr TIMES expr {$1 * $2}
    | expr DIV expr
    | MINUS expr %prec UMINUS      { - $2}
