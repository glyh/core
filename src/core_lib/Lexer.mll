{
open Lexing
open Parser

let advance_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  let pos' = { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  } in
  lexbuf.lex_curr_p <- pos'

let drop_str n s = 
  let len = String.length s in
    String.sub s n (len - n)
let drop_str_r n s = 
  let len = String.length s in
    String.sub s 0 (len - n)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let num = sign? digit+
let identifier = alpha (alpha | digit | '_')*

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Rules *)

rule token = parse
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  (* binary operators *)
  | ";" { SEMICOLON }
  | "=" { IS }
  | "letrec" { LETREC }
  | "let" { LET }
  | "in" { IN }
  | "case" { CASE }
  | "of" { OF }
  | "\\" { SLASH } 
  | "." { DOT }
  | "Pack{" { PACKHEAD }
  | "," { COMMA }
  | "}" { PACKEND }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "->" { ARROW }
  | "<" { LANG }
  | ">" { RANG }

  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }

  | "==" { EQ }
  | "~=" { NE }
  | "<=" { LE }
  | "<"  { LT }
  | ">=" { GE }
  | ">"  { GT }

  | "&" { AND }
  | "|" { OR }
  (* etc. *)
  | identifier { IDENT (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | newline { token lexbuf } (* just ignore *)
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
