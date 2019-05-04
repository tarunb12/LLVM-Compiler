%{
    open List ;;
    open Ast ;;
%}

%token LPAR RPAR LBRACE RBRACE COMMA SEMI
%token PLUS MINUS MUL DIV ASSIGN NOT MOD XOR
%token INCREMENT DECREMENT
%token PLUSEQ MINUSEQ MULEQ DIVEQ
%token EQ NEQ LT LTE GT GTE AND OR
%token IF ELSE FOR WHILE BREAK CONTINUE RETURN
%token TYPE_INT TYPE_FLOAT TYPE_BOOL TYPE_CHAR TYPE_STRING TYPE_UNIT
%token EOF

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <bool> BOOL
%token <string> STRING
%token <string> ID

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left AND OR
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left MUL DIV MOD
%right NOT NEG
%left PLUSEQ MINUSEQ MULEQ DIVEQ
%left INCREMENT DECREMENT

%type <Ast.program> program
%start program
%%

program:
    | stmts EOF                                 { Program($1) }
    | EOF                                       { Program([]) }
    ;

stmts:
    | stmt_list                                 { rev $1 }
    ;

stmt_list:
    | stmt                                      { [$1] }
    | stmt_list stmt                            { $2 :: $1 }
    ;

block:
    | LBRACE RBRACE                             { [] }
    | LBRACE stmts RBRACE                       { $2 }
    ;

stmt:
    | expr                                      { Expr($1) }
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

var_definition:
    | exprType ID ASSIGN expr                   { VarDef($1, $2, $4) }
    | exprType ID                               { VarDef($1, $2, Noexpr) }
    ;

func_definition:
    | exprType ID LPAR func_declaration_args RPAR block
        { FuncDef($1, $2, $4, $6) }
    ;

func_declaration_args: (* Empty list *)         { [] }
    | var_definition                           { [$1] }
    | func_declaration_args COMMA var_definition
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
    | expr INCREMENT                            { Assign($1, BinOp(Add, $1, IntLit(1))) }
    | expr DECREMENT                            { Assign($1, BinOp(Sub, $1, IntLit(1))) }
    | expr PLUSEQ   expr                        { Assign($1, BinOp(Add, $1, $3)) }
    | expr MINUSEQ  expr                        { Assign($1, BinOp(Sub, $1, $3)) }
    | expr MULEQ    expr                        { Assign($1, BinOp(Mult, $1, $3)) }
    | expr DIVEQ    expr                        { Assign($1, BinOp(Div, $1, $3)) }
    | expr PLUS     expr                        { BinOp(Add, $1, $3) }
    | expr MINUS    expr                        { BinOp(Sub, $1, $3) }
    | expr MUL      expr                        { BinOp(Mult, $1, $3) }
    | expr DIV      expr                        { BinOp(Div, $1, $3) }
    | expr MOD      expr                        { BinOp(Mod, $1, $3) }
    | expr EQ       expr                        { BinOp(Eq, $1, $3) }
    | expr NEQ      expr                        { BinOp(NEq, $1, $3) }
    | expr LT       expr                        { BinOp(Less, $1, $3) }
    | expr LTE      expr                        { BinOp(LEq, $1, $3) }
    | expr GT       expr                        { BinOp(Greater, $1, $3) }
    | expr GTE      expr                        { BinOp(GEq, $1, $3) }
    | expr AND      expr                        { BinOp(And, $1, $3) }
    | expr OR       expr                        { BinOp(Or, $1, $3) }
    | expr XOR      expr                        { BinOp(Xor, $1, $3) }
    | expr ASSIGN   expr                        { Assign($1, $3) }
    | MINUS expr %prec NEG                      { UnOp(Neg, $2) }
    | NOT expr                                  { UnOp(Not, $2) }
    | LPAR  expr RPAR                           { $2 }
    | ID LPAR call_args RPAR                    { Call($1, $3) }
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
    | INT                                       { IntLit($1) }
    | FLOAT                                     { FloatLit($1) }
    | CHAR                                      { CharLit($1) }
    | BOOL                                      { BoolLit($1) }
    | STRING                                    { StringLit($1) }      
    | ID                                        { Id($1) }              
    ;    