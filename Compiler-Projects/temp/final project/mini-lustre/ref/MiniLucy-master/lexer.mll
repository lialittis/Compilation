(*
########
Copyright © 2017

This file is part of MiniLucy.
MiniLucy is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License v3 as published by
the Free Software Foundation.

Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

{

  open Lexing;;
  open Parser;;
  open Ast_type;;
  open Ast;;
  open Ast_lustre;;

  exception Lexical_error of string;;

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k) [
      "and", AND;
      "const", CONST;
      "current", CURRENT;
      "div", DIV;
      "else", ELSE;
      "end", END;
      "every", EVERY;
      "fby", FBY;
      "if", IF;
      "int", INT;
      "let", LET;
      "merge", MERGE;
      "mod", MOD;
      "node", NODE;
      "not", NOT;
      "or", OR;
      "pre", PRE;
      "real", REAL;
      "returns", RETURNS;
      "tel", TEL;
      "then", THEN;
      "type", TYPE;
      "var", VAR;
      "when", WHEN;
    ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENT s
  ;;

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum}
  ;;
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let float = digit+ '.' digit* exponent?
          | digit* '.'digit+ exponent?
    | digit+ exponent
let ident = alpha (alpha | '_' | digit)*

rule token = parse
  |'\n'
    {newline lexbuf; token lexbuf}
  |[' ' '\t' '\r']+
    {token lexbuf}
  |"--" [^ '\n']* ['\n']
    {newline lexbuf; token lexbuf}
  |"/*"
    {comment lexbuf; token lexbuf}
  |ident
    {id_or_keyword (lexeme lexbuf)}
  |digit+
    {CONST_INT (int_of_string (lexeme lexbuf))}
  |float
    {CONST_REAL (float_of_string (lexeme lexbuf))}
  |"-"
    {MINUS}
  |"+"
    {PLUS}
  |"*"
    {STAR}
  |"/"
    {SLASH}
  |">"
    {COMP Op_gt}
  |">="
    {COMP Op_ge}
  |"<"
    {COMP Op_lt}
  |"<="
    {COMP Op_le}
  |"<>"
    {NEQ}
  |"=>"
    {IMPL}
  |"->"
    {ARROW}
  |"("
    {LPAREN}
  |")"
    {RPAREN}
  |":"
    {COLON}
  |";"
    {SEMICOL}
  |"="
    {EQUAL}
  |","
    {COMMA}
  |_
    {raise (Lexical_error (lexeme lexbuf))}
  |eof
    {EOF}

and comment = parse
  |"*/" {()}
  |'\n' {newline lexbuf; comment lexbuf}
  |_ {comment lexbuf}
  |eof {raise (Lexical_error "unterminated comment")}
