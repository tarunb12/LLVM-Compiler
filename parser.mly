%{
    open List
    open Ast
%}

%token TYPE_INT TYPE_FLOAT TYPE_BOOL TYPE_CHAR TYPE_STRING TYPE_UNIT
%token ASSIGN LT LTE GT GTE EQ NEQ TRUE FALSE
%token LPAR RPAR LBRACE RBRACE COMMA SEMI
%token PLUS MINUS MUL DIV AND OR NOT
%token IF ELSE FOR WHILE BREAK CONTINUE RETURN
%token PRINT
%token EOF

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%left AND OR
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left MUL DIV
%right NOT

%type <Ast.program> program
%start program
%%

program:
    | stmts EOF                                 { Program($1) }
    ;

stmts:
    | stmt_list                                 { rev $1 }
    ;

stmt_list:
    | stmt                                      { [$1] }
    | stmt_list stmt                            { $2 :: $1 }
    ;

block:
    | LBRACE stmts RBRACE                       { $2 }
    | LBRACE RBRACE                             { [] }
    ;

stmt:
    | expr                                      { Expr($1) }
    | PRINT expr                                { Print($2) }
    | RETURN                                    { Return(Noexpr) }
    | RETURN expr                               { Return($2) }
    | LBRACE stmts RBRACE                       { Block($2) }
    | IF LPAR expr RPAR stmt ELSE stmt          { If($3, $5, $7) }
    | WHILE LPAR expr RPAR stmt                 { While($3, $5) }
    | var_definition                            { $1 }
    | func_definition                           { $1 }
    | IF LPAR expr RPAR stmt %prec NOELSE       { If($3, $5, Block([])) }
    | FOR LPAR opt_expr SEMI expr SEMI opt_expr RPAR stmt   { For($3, $5, $7, $9) }
    | BREAK                                     { Break }
    | CONTINUE                                  { Continue }
    ;

opt_expr:                                       { Noexpr }
    | expr                                      { $1 }
    ;

var_declaration:
    | exprType ID                               { VarDec($1, $2) }
    ;

var_definition:
    | exprType ID ASSIGN expr                   { VarDef($1, $2, $4) }
    ;

func_definition:
    | exprType ID LPAR func_declaration_args RPAR block
        { FuncDef($1, $2, $4, $6) }
    ;

func_declaration_args:
    | var_declaration                           { [$1] }
    | func_declaration_args COMMA var_declaration
        { rev ($3 :: $1) }
    ;

call_args: (* Empty list *)                     { [] }
    | expr_list                                 { rev $1 }
    ;

expr_list:
    | expr                                      { [$1] }
    | expr_list COMMA expr                      { $3 :: $1 }
    ;

expr:
    | atom                                      { $1 }
    | expr PLUS     expr                        { BinOp(Add, $1, $3) }
    | expr MINUS    expr                        { BinOp(Sub, $1, $3) }
    | expr MUL      expr                        { BinOp(Mult, $1, $3) }
    | expr DIV      expr                        { BinOp(Div, $1, $3) }
    | expr EQ       expr                        { BinOp(Eq, $1, $3) }
    | expr NEQ      expr                        { BinOp(NEq, $1, $3) }
    | expr LT       expr                        { BinOp(Less, $1, $3) }
    | expr LTE      expr                        { BinOp(LEq, $1, $3) }
    | expr GT       expr                        { BinOp(Greater, $1, $3) }
    | expr GTE      expr                        { BinOp(GEq, $1, $3) }
    | NOT expr                                  { UnOp(Not, $2) }
    | expr AND      expr                        { BinOp(And, $1, $3) }
    | expr OR       expr                        { BinOp(Or, $1, $3) }
    | ID LPAR call_args RPAR                    { Call($1, $3) }
    | LPAR  expr RPAR                           { $2 }
    ;

exprType:
    | TYPE_INT                                  { Int_t }
    | TYPE_FLOAT                                { Float_t }
    | TYPE_BOOL                                 { Bool_t }
    | TYPE_CHAR                                 { Char_t }
    | TYPE_STRING                               { String_t }
    | TYPE_UNIT                                 { Unit_t }
    ;

atom:
    | TRUE                                      { Bool(true) }
    | FALSE                                     { Bool(false) }
    | INT                                       { Int($1) }
    | FLOAT                                     { Float($1) }
    | CHAR                                      { Char($1) }
    | STRING                                    { String($1) }                    
    ;    