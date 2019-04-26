{
  open Parser
    
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

let int = digit+ as lxm
let float = digit+ '.' digit* as lxm
let id = (alph | '_')(alph | digit | '_')* as lxm

rule token = parse
  | ws                  { token lexbuf }
  | "//"                { single_comment lexbuf }
  | '('                 { LPAR }
  | ')'                 { RPAR }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | '.'                 { DOT }
  | ','                 { COMMA }
  (* Math Operators *)
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { MUL }
  | '/'                 { DIV }
  | '='                 { ASSIGN }
  (* Bool Operators *)
  | "=="                { EQ }
  | "!="                { NEQ }
  | '<'                 { LT }
  | "<="                { LTE }
  | '>'                 { GT }
  | ">="                { GTE }
  | '!'                 { NOT }
  | "&&"                { AND }
  | "||"                { OR }
  (* Conditionals *)
  | "if"                { IF }
  | "else"              { ELSE }
  | "for"               { FOR }
  | "while"             { WHILE }
  (* Types *)
  | "int"               { TYPE_INT }
  | "float"             { TYPE_FLOAT }
  | "bool"              { TYPE_BOOL }
  | "unit"              { TYPE_UNIT }
  (* Atoms *)
  | int                 { INT (int_of_string lxm) }
  | float               { FLOAT (float_of_string lxm) }
  | id                  { ID (lxm) }
  | eof                 { EOF }
  | _                   { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }

and single_comment = parse
  | _         { single_comment lexbuf }