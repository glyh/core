type ident = string

type bin_op = 
   | ADD | SUB | MUL | DIV
   | LT | LE | GT | GE | EQ | NE
   | AND | OR

type isrec = 
   | Rec
   | NotRec

type program = sc list
and  sc = ident * ident list * exp
and exp = 
   | App of exp * exp
   | Bin of exp * bin_op * exp
   | Let of isrec * defn list * exp
   | Case of exp * (int * (ident list) * exp) list
   | Lam of ident list * exp
   | Var of ident
   | Num of int
   | Constr of int * int
and defn = ident * exp
