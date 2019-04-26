%{
    open List
    open Ast
%}

%token <string> ID
%token <float> FLOAT
%token <int> INT
%token ASSIGN LT LTE GT GTE EQ NEQ
%token LPAR RPAR LBRACE RBRACE COMMA DOT
%token PLUS MINUS MUL DIV AND OR NOT
%token IF ELSE FOR WHILE
%token TYPE_INT TYPE_FLOAT TYPE_BOOL
%token EOF
%left PLUS MINUS
%left MUL DIV
%type <expr> main
%start main
%%

main:
    | stmts EOF                         { Program($1) }
    ;

stmts:
    | stmt_list                         { rev $1 }
    ;

stmt_list:
    | stmt                              { [$1] }
    | stmt_list stmt                    { $2 :: $1 }
    ;

block:
    | LBRACE stmts RBRACE               { $2 }
    | LBRACE RBRACE                     {  }
    ;

stmt:
    | var_definition
    | func_definition
    | expr                              { Expr($1) }
    ;

var_declaration:
    | exprType ID                       { VarDec($1, $2) }
    ;

var_definition:
    | exprType ID ASSIGN expr           { VarDef($1, $2, $4) }
    ;

func_definition:
    | exprType ID LPAR func_declaration_args RPAR block
        { FuncDef($1, $2, $4, $6) }
    ;

func_declaration_args:
    | var_declaration                   { [$1] }
    | func_declaration_args COMMA var_declaration
        { rev ($3 :: $1) }
    ;

number:
    | INT                               { Int($1) }
    | FLOAT                             { Float($1) }
    ;

expr:
    | expr PLUS expr                    { BinOp(Add, $1, $3) }
    | expr MINUS expr                   { BinOp(Sub, $1, $3) }
    | expr MUL expr                     { BinOp(Mult, $1, $3) }
    | expr DIV expr                     { BinOp(Div, $1, $3) }
    | expr EQ expr                      { BinOp(Eq, $1, $3) }
    | expr NEQ expr                     { BinOp(NEq, $1, $3) }
    | expr LT expr                      { BinOp(Less, $1, $3) }
    | expr LTE expr                     { BinOp(LessEq, $1, $3) }
    | expr GT expr                      { BinOp(Greater, $1, $3) }
    | expr GTE expr                     { BinOp(GreaterEq, $1, $3) }
    | NOT expr                          { UnOp(Not, $1) }
    | expr AND expr                     { BinOp(And, $1, $3) }
    | expr OR expr                      { BinOp(Or, $1, $3) }
    | LPAR expr RPAR                    { $2 }
    ;

exprType:
    | TYPE_INT                          { INT }
    | TYPE_FLOAT                        { FLOAT }
    | TYPE_BOOL                         { BOOL }
    ;