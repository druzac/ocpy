
{
    open Parser
}

rule token = parse
[' ' '\t' '\n'] { token lexbuf}
| ['0'-'9']+ as lxm { NUMBER(int_of_string lxm) }
| '+'              {PLUS}
| '*'     {STAR}
| eof {EOF}
| _    {token lexbuf}
