{
  open Parser ;;
  open Lexing ;;
  open Exceptions ;;

  let filename = Sys.argv.(1) ;;

  let unescape (str : string) : string = Scanf.sscanf ("\"" ^ str ^ "\"") "%S%!" (fun x -> x) ;;

  let next_line (lexbuf : lexbuf) : unit =
    let pos : position = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {
        pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
      } ;;
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
let escape = '\\' ['\\' '\x27' '\x22' 'n' 'r' 't']

let int = digit+ as lxm
let float = digit+ '.' digit* as lxm
let char = '\x27' (ascii|digit as lxm) '\x27'
let escape_char = '\x27' (escape as lxm) '\x27'
let string = '\x22' ((ascii|escape)* as lxm) '\x22'
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
  | '%'                 { MOD }
  
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
  | '^'                 { XOR }

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

  (* Atoms *)
  | int                 { INT (int_of_string lxm) }
  | float               { FLOAT (float_of_string lxm) }
  | char                { CHAR (lxm) }
  | escape_char         { CHAR (String.get (unescape lxm) 0) }
  | string              { STRING (unescape lxm) }
  | id                  { ID (lxm) }

  (* Anything else *)
  | eof                 { EOF }
  | _                   { raise (SyntaxError (lexbuf.lex_curr_p.pos_lnum + 1, lexeme lexbuf)) }

and single_comment = parse
  | '\n'                { token lexbuf }
  | _                   { single_comment lexbuf }