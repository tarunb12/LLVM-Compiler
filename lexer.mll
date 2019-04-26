{
  open Parser
  exception Eof
  
  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      }
}

let ws = [' ' '\n' '\t']
let digit = ['0'-'9']
let alph = ['a'-'z' 'A'-'Z']

rule token = parse
  | [' ' '\r' '\n' '\t']                        { token lexbuf }
  | digit+ as lxm                               { INT (int_of_string lxm) }
  | digit+ '.' digit* as lxm                    { FLOAT (float_of_string lxm) }
  | (alph | '_')(alph | digit | '_')* as lxm    { ID (lxm) }
  | '='                                         { ASSIGN }
  | "=="                                        { EQ }
  | "!="                                        { NEQ }
  | '<'                                         { LT }
  | "<="                                        { LTE }
  | '>'                                         { GT }
  | ">="                                        { GTE }
  | '('                                         { LPAR }
  | ')'                                         { RPAR }
  | '{'                                         { LBRAC }
  | '}'                                         { RBRAC }
  | '.'                                         { DOT }
  | ','                                         { COMMA }
  | '+'                                         { PLUS }
  | '-'                                         { MINUS }
  | '*'                                         { MUL }
  | '/'                                         { DIV }
  | eof                                         { raise Eof }
  | _ { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }
