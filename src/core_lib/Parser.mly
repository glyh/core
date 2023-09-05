%{
open Ast
%}

%token <int> NUM
%token <string> IDENT

%token SEMICOLON
%token IS
%token LETREC
%token LET
%token IN
%token CASE
%token OF
%token SLASH
%token DOT
%token PACKHEAD
%token COMMA
%token PACKEND
%token LPAREN
%token RPAREN
%token ARROW
%token LANG
%token RANG
%token ADD
%token SUB
%token MUL
%token DIV
%token EQ
%token NE
%token LE
%token LT
%token GE
%token GT
%token AND
%token OR

%token EOF

%

%left OR
%left AND
%left EQ NE LE LT GE GT
%left ADD SUB
%left MUL DIV
%prec prec_app

%type <program> program

%start program

%%

program: 
  | scs=list(sc) EOF { scs }

sc: 
  | name=IDENT args=list(IDENT) IS body=exp { 
      (name, args, body) 
    }

exp: 
  | hd=simple_exp rest=list(simple_exp) %prec prec_app
  {
    (List.fold_left (fun acc ele -> (App(acc, ele)))) hd rest
  }
  | lhs=exp op=binop rhs=exp { Bin(lhs, op, rhs) } 
  | LET ds=nonempty_list(defn) IN in=exp {
    Let(NotRec, ds, in)
  }
  | LETREC ds=defns IN in=exp {
    Let(Rec, ds, in)
  }
  | CASE e=exp OF alts=nonempty_list(alt) {
    Case(e, alts)
  }
  | SLASH ids=list(IDENT) DOT e=exp {
    LAM(ids, e)
  }
  | simple_exp

simple_exp:
  | id=IDENT { Var id }
  | n=NUM { Num n }
  | PACKHEAD i=NUM COMMA j=NUM PACKEND { Constr(i, j) }
  | LPAREN e=exp RPAREN { e }

defn:
  | name=IDENT IS body=exp { (name, body) }

alt: 
  | LANG n=NUM RANG ids=list(IDENT) ARROW e=exp {
    (n, ids, e)
  }

%inline bin_op: 
  | ADD { ADD }
  | SUB { SUB }
  | MUL { MUL }
  | DIV { DIV }
  | EQ { EQ }
  | NE { NE }
  | LE { LE }
  | LT { LT }
  | GE { GE }
  | GT { GT }
  | AND { AND }
  | OR { OR }
