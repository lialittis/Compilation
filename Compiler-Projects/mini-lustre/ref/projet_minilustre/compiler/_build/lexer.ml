# 1 "lexer.mll"
 

  open Lexing
  open Parser
  open Asttypes
  open Ast

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [ 
	"and", AND;
	"bool", BOOL;
	"const", CONST;
	"else", ELSE;
	"end", END;
	"false", CONST_BOOL(false); 
  "fby", FBY;
	"float", FLOAT;
	"if", IF;
	"int", INT;
	"let", LET;
  "merge", MERGE;
	"node", NODE;
	"not", NOT;
	"or", OR;
	"returns", RETURNS;
	"string", STRING;
	"tel", TEL;
	"then", THEN;
	"true", CONST_BOOL(true);
	"unit", UNIT; 
	"var", VAR;
  "when", WHEN;
      ];
    fun s -> 
      try Hashtbl.find h s with Not_found -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

# 48 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\227\255\228\255\229\255\231\255\232\255\233\255\234\255\
    \235\255\236\255\002\000\030\000\046\000\047\000\077\000\076\000\
    \092\000\102\000\118\000\003\000\255\255\242\255\253\255\182\000\
    \192\000\214\000\225\000\247\000\001\001\011\001\023\001\033\001\
    \043\001\053\001\063\001\085\001\095\001\105\001\230\255\245\255\
    \244\255\243\255\240\255\237\255\238\255\004\000\249\255\153\001\
    \162\000\255\255\252\255\253\255\254\255\124\000\252\255\253\255\
    \254\255\047\000\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\016\000\014\000\008\000\007\000\006\000\027\000\
    \004\000\003\000\009\000\001\000\255\255\255\255\255\255\255\255\
    \005\000\255\255\005\000\255\255\005\000\255\255\005\000\255\255\
    \005\000\255\255\005\000\255\255\005\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\004\000\
    \005\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\002\000\255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\047\000\000\000\047\000\
    \255\255\000\000\000\000\000\000\000\000\055\000\000\000\000\000\
    \000\000\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\020\000\000\000\019\000\019\000\046\000\000\000\
    \019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\000\000\003\000\019\000\000\000\000\000\049\000\000\000\
    \009\000\008\000\012\000\013\000\004\000\014\000\015\000\018\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\007\000\006\000\010\000\005\000\011\000\044\000\
    \043\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\042\000\041\000\040\000\058\000\000\000\
    \048\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\039\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\056\000\000\000\
    \000\000\000\000\024\000\038\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \022\000\023\000\000\000\000\000\021\000\000\000\057\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\023\000\000\000\000\000\050\000\017\000\000\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\033\000\000\000\033\000\000\000\000\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\000\000\000\000\000\000\000\000\051\000\000\000\
    \001\000\031\000\000\000\031\000\046\000\025\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \052\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\029\000\000\000\029\000\025\000\027\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\000\000\027\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\000\000\000\000\000\000\054\000\000\000\000\000\000\000\
    \037\000\000\000\037\000\000\000\035\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\255\255\035\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\019\000\000\000\045\000\255\255\
    \019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\019\000\255\255\255\255\045\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
    \010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\011\000\012\000\013\000\057\000\255\255\
    \045\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\014\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\053\000\255\255\
    \255\255\255\255\016\000\014\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \018\000\016\000\255\255\255\255\018\000\255\255\053\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\016\000\255\255\255\255\048\000\017\000\255\255\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\023\000\255\255\023\000\255\255\255\255\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\255\255\255\255\255\255\255\255\048\000\255\255\
    \000\000\025\000\255\255\025\000\045\000\024\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \048\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\027\000\255\255\027\000\024\000\026\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\255\255\026\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\255\255\255\255\255\255\053\000\255\255\255\255\255\255\
    \035\000\255\255\035\000\255\255\034\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\047\000\034\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\047\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\047\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\047\000";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 57 "lexer.mll"
      ( newline lexbuf; token lexbuf )
# 268 "lexer.ml"

  | 1 ->
# 59 "lexer.mll"
      ( token lexbuf )
# 273 "lexer.ml"

  | 2 ->
# 63 "lexer.mll"
      ( comment lexbuf; token lexbuf )
# 278 "lexer.ml"

  | 3 ->
# 65 "lexer.mll"
      ( id_or_keyword (lexeme lexbuf) )
# 283 "lexer.ml"

  | 4 ->
# 67 "lexer.mll"
      ( CONST_INT (int_of_string (lexeme lexbuf)) )
# 288 "lexer.ml"

  | 5 ->
# 69 "lexer.mll"
      ( CONST_FLOAT (float_of_string (lexeme lexbuf)) )
# 293 "lexer.ml"

  | 6 ->
# 71 "lexer.mll"
      ( MINUS )
# 298 "lexer.ml"

  | 7 ->
# 73 "lexer.mll"
      ( PLUS )
# 303 "lexer.ml"

  | 8 ->
# 75 "lexer.mll"
      ( STAR )
# 308 "lexer.ml"

  | 9 ->
# 77 "lexer.mll"
      ( SLASH )
# 313 "lexer.ml"

  | 10 ->
# 79 "lexer.mll"
      ( MINUS_DOT )
# 318 "lexer.ml"

  | 11 ->
# 81 "lexer.mll"
      ( PLUS_DOT )
# 323 "lexer.ml"

  | 12 ->
# 83 "lexer.mll"
      ( STAR_DOT )
# 328 "lexer.ml"

  | 13 ->
# 85 "lexer.mll"
      ( SLASH_DOT )
# 333 "lexer.ml"

  | 14 ->
# 87 "lexer.mll"
      ( COMP Bgt )
# 338 "lexer.ml"

  | 15 ->
# 89 "lexer.mll"
      ( COMP Bge )
# 343 "lexer.ml"

  | 16 ->
# 91 "lexer.mll"
      ( COMP Blt )
# 348 "lexer.ml"

  | 17 ->
# 93 "lexer.mll"
      ( COMP Ble )
# 353 "lexer.ml"

  | 18 ->
# 95 "lexer.mll"
      ( NEQ )
# 358 "lexer.ml"

  | 19 ->
# 97 "lexer.mll"
      ( LPAREN )
# 363 "lexer.ml"

  | 20 ->
# 99 "lexer.mll"
      ( RPAREN )
# 368 "lexer.ml"

  | 21 ->
# 101 "lexer.mll"
      ( COLON )
# 373 "lexer.ml"

  | 22 ->
# 103 "lexer.mll"
      ( SEMICOL )
# 378 "lexer.ml"

  | 23 ->
# 105 "lexer.mll"
      ( EQUAL )
# 383 "lexer.ml"

  | 24 ->
# 107 "lexer.mll"
      ( COMMA )
# 388 "lexer.ml"

  | 25 ->
# 109 "lexer.mll"
      ( ARROW )
# 393 "lexer.ml"

  | 26 ->
# 111 "lexer.mll"
      ( let buf = Buffer.create 512 in
	string buf lexbuf;
	CONST_STRING (Buffer.contents buf) )
# 400 "lexer.ml"

  | 27 ->
# 115 "lexer.mll"
      ( raise (Lexical_error (lexeme lexbuf)) )
# 405 "lexer.ml"

  | 28 ->
# 117 "lexer.mll"
      ( EOF )
# 410 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and string buf lexbuf =
   __ocaml_lex_string_rec buf lexbuf 45
and __ocaml_lex_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 120 "lexer.mll"
        ( () )
# 422 "lexer.ml"

  | 1 ->
# 122 "lexer.mll"
      ( Buffer.add_string buf "\\n";
	string buf lexbuf )
# 428 "lexer.ml"

  | 2 ->
# 125 "lexer.mll"
      ( Buffer.add_string buf "\\\\";
	string buf lexbuf )
# 434 "lexer.ml"

  | 3 ->
# 128 "lexer.mll"
      ( Buffer.add_string buf "\\\"";
	string buf lexbuf )
# 440 "lexer.ml"

  | 4 ->
# 131 "lexer.mll"
      ( Buffer.add_string buf (lexeme lexbuf);
	string buf lexbuf )
# 446 "lexer.ml"

  | 5 ->
# 134 "lexer.mll"
      ( raise (Lexical_error "illegal escape character") )
# 451 "lexer.ml"

  | 6 ->
# 136 "lexer.mll"
      ( raise (Lexical_error "unterminated string") )
# 456 "lexer.ml"

  | 7 ->
# 138 "lexer.mll"
      ( raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) )
# 461 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_rec buf lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 53
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 141 "lexer.mll"
         ( () )
# 473 "lexer.ml"

  | 1 ->
# 142 "lexer.mll"
         ( newline lexbuf; comment lexbuf )
# 478 "lexer.ml"

  | 2 ->
# 143 "lexer.mll"
         ( comment lexbuf )
# 483 "lexer.ml"

  | 3 ->
# 144 "lexer.mll"
         ( raise (Lexical_error "unterminated comment") )
# 488 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

