{
  open Parser
  open Lexing

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      }
}

(* Escapes:
  \x20 - space
  \x22 - double quote
  \x27 - single quote
 *)

let ws = ['\x20' '\t']
let digit = ['0'-'9']
let alph = ['a'-'z' 'A'-'Z']
let ascii = [' '-'!' '#'-'[' ']'-'~']
let escape_char = '\\' ['\\' '\x27' '\x22' 'n' 'r' 't']

let int = digit+ as lxm
let float = digit+ '.' digit* as lxm
let char = '\x27'(ascii|digit as lxm)'\x27'
let string = '\x22'(ascii|escape_char)'\x22'
let id = (alph | '_')(alph | digit | '_')* as lxm

rule token = parse
  | ws                  { token lexbuf }
  | '\n'                { next_line lexbuf; token lexbuf }
  | "//"                { single_comment lexbuf }
  | '('                 { LPAR }
  | ')'                 { RPAR }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | ','                 { COMMA }
  | ';'                 { SEMI }
  | '='                 { ASSIGN }

  (* Math Operators *)
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { MUL }
  | '/'                 { DIV }
  
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
  | "break"             { BREAK }
  | "continue"          { CONTINUE }
  | "return"            { RETURN }

  (* Types *)
  | "int"               { TYPE_INT }
  | "float"             { TYPE_FLOAT }
  | "bool"              { TYPE_BOOL }
  | "char"              { TYPE_CHAR }
  | "string"            { TYPE_STRING }
  | "unit"              { TYPE_UNIT }

  (* Built-in Functions *)
  | "print"             { PRINT }

  (* Atoms *)
  | int                 { INT (int_of_string lxm) }
  | float               { FLOAT (float_of_string lxm) }
  | char                { CHAR (lxm) }
  | id                  { ID (lxm) }

  | eof                 { EOF }
  | _                   { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }

and single_comment = parse
  | _         { single_comment lexbuf }