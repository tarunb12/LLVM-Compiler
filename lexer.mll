{
    open Parser
    exception Eof
}

rule token = parse
  | [' ' '\n' '\t']                                             { token lexbuf }
  | ['0'-'9']+ as lxm                                           { INT (int_of_string lxm) }
  | ['0'-'9']+.['0'-'9']* as lxm                                { FLOAT (float_of_string lxm) }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm  { ID (lxm) }
  | '='                                                         { ASSIGN }
  | '=='                                                        { EQ }
  | '=='                                                        { NEQ }
  | '<'                                                         { LT }
  | '<='                                                        { LTE }
  | '>'                                                         { GT }
  | '>='                                                        { GTE }
  | '('                                                         { LPAR }
  | ')'                                                         { RPAR }
  | '{'                                                         { LBRAC }
  | '}'                                                         { RBRAC }
  | '.'                                                         { DOT }
  | ','                                                         { COMMA }
  | '+'                                                         { PLUS }
  | '-'                                                         { MINUS }
  | '*'                                                         { MUL }
  | '/'                                                         { DIV }
  | eof                                                         { raise Eof }