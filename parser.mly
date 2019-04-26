%{
    open List
    open Expr
%}

%token <string> ID
%token <float> FLOAT
%token <int> INT
%token PLUS MINUS MUL DIV
%token LPAR RPAR LBRAC RBRAC COMMA DOT
%token EOL
%left PLUS MINUS
%left MUL DIV
%type <expr> main
%start program
%%

program:
    | stmts                           { $1 }
    ;

stmts:
    | stmt_list                     { rev $1 }
    ;

stmt_list:
    | stmt                          { [$1] }
    | stmt_list stmt                { $2 :: $1 }
    ;

block:
    | LBRAC stmts RBRAC             { $2 }
    | LBRAC RBRAC                   {  }
    ;

(* v_expr:
    |   NUM                         { Num($1) }
    |   VAR                         { Var($1) }
    |   NUM VAR                     { Mul(Num($1), Var($2)) }
    |   VAR POW NUM                 { Pow(Var($1), $3) }
    |   NUM VAR POW NUM             { Mul(Num($1), Pow(Var($2), $4))}

expr:
    |   v_expr                      { $1 }
    |   LPAR expr RPAR              { $2 }
    |   expr TIMES expr             { Mul($1,$3) }
    |   expr PLUS expr              { Add($1, $3) }
    |   expr MINUS expr             { Sub($1, $3) }
    |   expr POW NUM                { Pow($1, $3) }
    |   PLUS expr                   { Pos($2) }
    |   MINUS expr                  { Neg($2) }
    ; *)