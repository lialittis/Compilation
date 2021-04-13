type token =
  | AND
  | BOOL
  | CONST
  | COLON
  | COMMA
  | COMP of (Asttypes.binop)
  | CONST_BOOL of (bool)
  | CONST_INT of (int)
  | CONST_FLOAT of (float)
  | CONST_STRING of (string)
  | ELSE
  | END
  | EOF
  | EQUAL
  | NEQ
  | FBY
  | FLOAT
  | IDENT of (string)
  | IF
  | INT
  | LET
  | LPAREN
  | MINUS
  | MINUS_DOT
  | NODE
  | NOT
  | OR
  | PLUS
  | PLUS_DOT
  | RETURNS
  | RPAREN
  | SEMICOL
  | SLASH
  | SLASH_DOT
  | STAR
  | STAR_DOT
  | STRING
  | TEL
  | THEN
  | UNIT
  | VAR

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

  open Asttypes
  open Ast

  let loc () = symbol_start_pos (), symbol_end_pos ()
  let mk_expr e = { pexpr_desc = e; pexpr_loc = loc () }
  let mk_patt p = { ppatt_desc = p; ppatt_loc = loc () }

# 56 "parser.ml"
let yytransl_const = [|
  257 (* AND *);
  258 (* BOOL *);
  259 (* CONST *);
  260 (* COLON *);
  261 (* COMMA *);
  267 (* ELSE *);
  268 (* END *);
    0 (* EOF *);
  269 (* EQUAL *);
  270 (* NEQ *);
  271 (* FBY *);
  272 (* FLOAT *);
  274 (* IF *);
  275 (* INT *);
  276 (* LET *);
  277 (* LPAREN *);
  278 (* MINUS *);
  279 (* MINUS_DOT *);
  280 (* NODE *);
  281 (* NOT *);
  282 (* OR *);
  283 (* PLUS *);
  284 (* PLUS_DOT *);
  285 (* RETURNS *);
  286 (* RPAREN *);
  287 (* SEMICOL *);
  288 (* SLASH *);
  289 (* SLASH_DOT *);
  290 (* STAR *);
  291 (* STAR_DOT *);
  292 (* STRING *);
  293 (* TEL *);
  294 (* THEN *);
  295 (* UNIT *);
  296 (* VAR *);
    0|]

let yytransl_block = [|
  262 (* COMP *);
  263 (* CONST_BOOL *);
  264 (* CONST_INT *);
  265 (* CONST_FLOAT *);
  266 (* CONST_STRING *);
  273 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\004\000\004\000\005\000\006\000\
\006\000\008\000\008\000\009\000\009\000\010\000\007\000\007\000\
\013\000\014\000\014\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\016\000\016\000\016\000\016\000\016\000\011\000\
\011\000\017\000\017\000\018\000\018\000\012\000\012\000\012\000\
\012\000\012\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\014\000\000\000\001\000\001\000\000\000\
\002\000\001\000\003\000\002\000\003\000\003\000\001\000\002\000\
\004\000\001\000\005\000\003\000\001\000\001\000\004\000\006\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\002\000\005\000\002\000\001\000\001\000\001\000\001\000\003\000\
\001\000\000\000\001\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\059\000\000\000\000\000\000\000\001\000\
\003\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\048\000\000\000\011\000\055\000\057\000\
\056\000\058\000\054\000\014\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\013\000\000\000\004\000\016\000\
\000\000\000\000\044\000\045\000\046\000\047\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000\000\000\000\000\
\000\000\043\000\000\000\000\000\000\000\041\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\051\000\000\000\000\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\000\033\000\
\030\000\031\000\000\000\023\000\000\000\000\000\052\000\000\000\
\042\000\000\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\012\000\030\000\035\000\042\000\013\000\
\036\000\014\000\015\000\028\000\043\000\044\000\087\000\062\000\
\088\000\089\000"

let yysindex = "\004\000\
\245\254\000\000\254\254\000\000\016\000\245\254\000\255\000\000\
\000\000\028\255\042\255\018\255\000\000\019\255\050\255\028\255\
\026\255\028\255\001\255\000\000\036\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\255\029\255\000\000\027\255\
\035\255\028\255\047\255\000\000\049\255\025\255\028\255\000\000\
\061\255\048\255\025\255\071\255\000\000\089\255\000\000\000\000\
\038\001\028\255\000\000\000\000\000\000\000\000\076\255\038\001\
\019\001\038\001\038\001\038\001\123\000\000\000\068\255\038\001\
\073\255\000\000\100\000\058\255\058\255\000\000\038\001\038\001\
\038\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
\000\000\038\001\038\001\038\001\038\001\000\000\147\000\074\255\
\000\000\038\001\038\001\000\000\246\000\042\001\042\001\042\001\
\200\000\058\255\058\255\223\000\058\255\058\255\000\000\000\000\
\000\000\000\000\038\001\000\000\177\000\082\255\000\000\038\001\
\000\000\200\000"

let yyrindex = "\000\000\
\115\000\000\000\000\000\000\000\000\000\115\000\000\000\000\000\
\000\000\086\255\002\255\000\000\000\000\087\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\098\255\000\000\000\000\000\000\000\000\000\000\100\255\000\000\
\000\000\000\000\088\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\038\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\094\255\
\000\000\000\000\000\000\108\255\136\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\096\255\000\000\
\000\000\000\000\000\000\000\000\081\000\020\000\042\000\064\000\
\051\255\164\255\192\255\078\000\220\255\248\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\072\255"

let yygindex = "\000\000\
\000\000\121\000\000\000\000\000\000\000\000\000\085\000\239\255\
\090\000\236\255\244\255\000\000\000\000\000\000\207\255\000\000\
\000\000\167\255"

let yytablesize = 589
let yytable = "\061\000\
\022\000\110\000\023\000\020\000\001\000\049\000\065\000\067\000\
\068\000\069\000\070\000\031\000\003\000\037\000\007\000\008\000\
\024\000\111\000\037\000\025\000\010\000\093\000\094\000\095\000\
\096\000\097\000\098\000\099\000\100\000\101\000\102\000\049\000\
\103\000\104\000\105\000\106\000\026\000\063\000\022\000\027\000\
\109\000\040\000\022\000\022\000\011\000\041\000\016\000\017\000\
\022\000\018\000\022\000\022\000\022\000\019\000\021\000\025\000\
\029\000\033\000\032\000\022\000\022\000\025\000\114\000\022\000\
\022\000\022\000\038\000\022\000\022\000\022\000\022\000\022\000\
\022\000\071\000\034\000\022\000\024\000\046\000\072\000\039\000\
\025\000\025\000\024\000\049\000\047\000\073\000\074\000\075\000\
\025\000\082\000\083\000\084\000\085\000\050\000\076\000\077\000\
\064\000\086\000\078\000\079\000\080\000\024\000\024\000\108\000\
\082\000\083\000\084\000\085\000\039\000\024\000\090\000\113\000\
\039\000\039\000\002\000\005\000\010\000\008\000\039\000\012\000\
\039\000\039\000\039\000\050\000\015\000\053\000\009\000\048\000\
\045\000\039\000\039\000\000\000\000\000\039\000\039\000\039\000\
\040\000\039\000\039\000\000\000\040\000\040\000\000\000\000\000\
\000\000\039\000\040\000\000\000\040\000\040\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\040\000\000\000\
\000\000\040\000\040\000\040\000\028\000\040\000\040\000\000\000\
\028\000\028\000\000\000\000\000\000\000\040\000\028\000\000\000\
\028\000\028\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\000\028\000\000\000\000\000\028\000\028\000\028\000\
\029\000\028\000\028\000\000\000\029\000\029\000\000\000\000\000\
\000\000\028\000\029\000\000\000\029\000\029\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\029\000\029\000\000\000\
\000\000\029\000\029\000\029\000\026\000\029\000\029\000\000\000\
\026\000\026\000\000\000\000\000\000\000\029\000\026\000\000\000\
\026\000\026\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\026\000\000\000\000\000\026\000\026\000\026\000\
\027\000\026\000\026\000\000\000\027\000\027\000\000\000\000\000\
\000\000\026\000\027\000\000\000\027\000\027\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\027\000\000\000\
\000\000\027\000\027\000\027\000\034\000\027\000\027\000\000\000\
\034\000\034\000\000\000\000\000\000\000\027\000\034\000\000\000\
\034\000\034\000\034\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\000\000\000\000\034\000\035\000\035\000\
\000\000\034\000\034\000\000\000\035\000\000\000\035\000\035\000\
\035\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\000\000\000\000\000\035\000\036\000\036\000\000\000\035\000\
\035\000\000\000\036\000\000\000\036\000\036\000\036\000\035\000\
\000\000\037\000\038\000\000\000\000\000\037\000\000\000\000\000\
\038\000\036\000\000\000\037\000\038\000\036\000\036\000\037\000\
\000\000\000\000\000\000\000\000\071\000\036\000\000\000\038\000\
\091\000\072\000\037\000\038\000\038\000\000\000\037\000\037\000\
\073\000\074\000\075\000\038\000\000\000\000\000\037\000\000\000\
\000\000\076\000\077\000\071\000\000\000\078\000\079\000\080\000\
\072\000\092\000\000\000\082\000\083\000\084\000\085\000\073\000\
\074\000\075\000\000\000\000\000\000\000\000\000\000\000\000\000\
\076\000\077\000\000\000\071\000\078\000\079\000\080\000\107\000\
\072\000\081\000\082\000\083\000\084\000\085\000\000\000\073\000\
\074\000\075\000\000\000\000\000\000\000\000\000\000\000\000\000\
\076\000\077\000\000\000\000\000\078\000\079\000\080\000\000\000\
\000\000\071\000\082\000\083\000\084\000\085\000\072\000\000\000\
\000\000\000\000\000\000\112\000\000\000\073\000\074\000\075\000\
\000\000\000\000\000\000\000\000\000\000\000\000\076\000\077\000\
\071\000\000\000\078\000\079\000\080\000\072\000\000\000\000\000\
\082\000\083\000\084\000\085\000\073\000\074\000\075\000\000\000\
\000\000\000\000\000\000\000\000\000\000\076\000\077\000\071\000\
\000\000\078\000\079\000\080\000\072\000\000\000\000\000\082\000\
\083\000\084\000\085\000\073\000\074\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\076\000\077\000\000\000\000\000\
\000\000\079\000\080\000\072\000\000\000\000\000\082\000\083\000\
\084\000\085\000\073\000\074\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\076\000\077\000\000\000\000\000\000\000\
\079\000\080\000\000\000\000\000\000\000\082\000\083\000\084\000\
\085\000\051\000\052\000\053\000\054\000\000\000\000\000\000\000\
\000\000\000\000\000\000\055\000\056\000\000\000\000\000\057\000\
\058\000\059\000\000\000\060\000\051\000\052\000\053\000\054\000\
\066\000\000\000\000\000\000\000\000\000\000\000\055\000\056\000\
\000\000\000\000\057\000\058\000\059\000\000\000\060\000\076\000\
\077\000\000\000\000\000\000\000\079\000\080\000\000\000\000\000\
\000\000\082\000\083\000\084\000\085\000"

let yycheck = "\049\000\
\018\000\091\000\002\001\016\000\001\000\004\001\056\000\057\000\
\058\000\059\000\060\000\029\000\024\001\034\000\017\001\000\000\
\016\001\107\000\039\000\019\001\021\001\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\079\000\080\000\030\001\
\082\000\083\000\084\000\085\000\036\001\050\000\001\001\039\001\
\090\000\017\001\005\001\006\001\017\001\021\001\005\001\030\001\
\011\001\031\001\013\001\014\001\015\001\004\001\029\001\005\001\
\021\001\031\001\030\001\022\001\023\001\011\001\112\000\026\001\
\027\001\028\001\020\001\030\001\031\001\032\001\033\001\034\001\
\035\001\001\001\040\001\038\001\005\001\017\001\006\001\031\001\
\030\001\031\001\011\001\013\001\037\001\013\001\014\001\015\001\
\038\001\032\001\033\001\034\001\035\001\005\001\022\001\023\001\
\021\001\030\001\026\001\027\001\028\001\030\001\031\001\030\001\
\032\001\033\001\034\001\035\001\001\001\038\001\038\001\030\001\
\005\001\006\001\000\000\030\001\030\001\020\001\011\001\020\001\
\013\001\014\001\015\001\030\001\037\001\030\001\006\000\043\000\
\039\000\022\001\023\001\255\255\255\255\026\001\027\001\028\001\
\001\001\030\001\031\001\255\255\005\001\006\001\255\255\255\255\
\255\255\038\001\011\001\255\255\013\001\014\001\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\255\255\
\255\255\026\001\027\001\028\001\001\001\030\001\031\001\255\255\
\005\001\006\001\255\255\255\255\255\255\038\001\011\001\255\255\
\013\001\014\001\015\001\255\255\255\255\255\255\255\255\255\255\
\255\255\022\001\023\001\255\255\255\255\026\001\027\001\028\001\
\001\001\030\001\031\001\255\255\005\001\006\001\255\255\255\255\
\255\255\038\001\011\001\255\255\013\001\014\001\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\255\255\
\255\255\026\001\027\001\028\001\001\001\030\001\031\001\255\255\
\005\001\006\001\255\255\255\255\255\255\038\001\011\001\255\255\
\013\001\014\001\015\001\255\255\255\255\255\255\255\255\255\255\
\255\255\022\001\023\001\255\255\255\255\026\001\027\001\028\001\
\001\001\030\001\031\001\255\255\005\001\006\001\255\255\255\255\
\255\255\038\001\011\001\255\255\013\001\014\001\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\255\255\
\255\255\026\001\027\001\028\001\001\001\030\001\031\001\255\255\
\005\001\006\001\255\255\255\255\255\255\038\001\011\001\255\255\
\013\001\014\001\015\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\001\001\255\255\255\255\026\001\005\001\006\001\
\255\255\030\001\031\001\255\255\011\001\255\255\013\001\014\001\
\015\001\038\001\255\255\255\255\255\255\255\255\255\255\255\255\
\001\001\255\255\255\255\026\001\005\001\006\001\255\255\030\001\
\031\001\255\255\011\001\255\255\013\001\014\001\015\001\038\001\
\255\255\001\001\005\001\255\255\255\255\005\001\255\255\255\255\
\011\001\026\001\255\255\011\001\015\001\030\001\031\001\015\001\
\255\255\255\255\255\255\255\255\001\001\038\001\255\255\026\001\
\005\001\006\001\026\001\030\001\031\001\255\255\030\001\031\001\
\013\001\014\001\015\001\038\001\255\255\255\255\038\001\255\255\
\255\255\022\001\023\001\001\001\255\255\026\001\027\001\028\001\
\006\001\030\001\255\255\032\001\033\001\034\001\035\001\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\022\001\023\001\255\255\001\001\026\001\027\001\028\001\005\001\
\006\001\031\001\032\001\033\001\034\001\035\001\255\255\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\022\001\023\001\255\255\255\255\026\001\027\001\028\001\255\255\
\255\255\001\001\032\001\033\001\034\001\035\001\006\001\255\255\
\255\255\255\255\255\255\011\001\255\255\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\001\001\255\255\026\001\027\001\028\001\006\001\255\255\255\255\
\032\001\033\001\034\001\035\001\013\001\014\001\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\001\001\
\255\255\026\001\027\001\028\001\006\001\255\255\255\255\032\001\
\033\001\034\001\035\001\013\001\014\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\255\255\255\255\
\255\255\027\001\028\001\006\001\255\255\255\255\032\001\033\001\
\034\001\035\001\013\001\014\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\022\001\023\001\255\255\255\255\255\255\
\027\001\028\001\255\255\255\255\255\255\032\001\033\001\034\001\
\035\001\007\001\008\001\009\001\010\001\255\255\255\255\255\255\
\255\255\255\255\255\255\017\001\018\001\255\255\255\255\021\001\
\022\001\023\001\255\255\025\001\007\001\008\001\009\001\010\001\
\030\001\255\255\255\255\255\255\255\255\255\255\017\001\018\001\
\255\255\255\255\021\001\022\001\023\001\255\255\025\001\022\001\
\023\001\255\255\255\255\255\255\027\001\028\001\255\255\255\255\
\255\255\032\001\033\001\034\001\035\001"

let yynames_const = "\
  AND\000\
  BOOL\000\
  CONST\000\
  COLON\000\
  COMMA\000\
  ELSE\000\
  END\000\
  EOF\000\
  EQUAL\000\
  NEQ\000\
  FBY\000\
  FLOAT\000\
  IF\000\
  INT\000\
  LET\000\
  LPAREN\000\
  MINUS\000\
  MINUS_DOT\000\
  NODE\000\
  NOT\000\
  OR\000\
  PLUS\000\
  PLUS_DOT\000\
  RETURNS\000\
  RPAREN\000\
  SEMICOL\000\
  SLASH\000\
  SLASH_DOT\000\
  STAR\000\
  STAR_DOT\000\
  STRING\000\
  TEL\000\
  THEN\000\
  UNIT\000\
  VAR\000\
  "

let yynames_block = "\
  COMP\000\
  CONST_BOOL\000\
  CONST_INT\000\
  CONST_FLOAT\000\
  CONST_STRING\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'node_decs) in
    Obj.repr(
# 74 "parser.mly"
                    ( _1 )
# 392 "parser.ml"
               : Ast.p_file))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
                    ( [] )
# 398 "parser.ml"
               : 'node_decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'node) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'node_decs) in
    Obj.repr(
# 79 "parser.mly"
                    ( _1 :: _2 )
# 406 "parser.ml"
               : 'node_decs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 12 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 10 : 'in_params) in
    let _8 = (Parsing.peek_val __caml_parser_env 6 : 'out_params) in
    let _11 = (Parsing.peek_val __caml_parser_env 3 : 'local_params) in
    let _13 = (Parsing.peek_val __caml_parser_env 1 : 'eq_list) in
    Obj.repr(
# 88 "parser.mly"
    ( { pn_name = _2;
	pn_input = _4;
	pn_output = _8;
	pn_local = _11;
	pn_equs = _13;
	pn_loc = loc(); } )
# 422 "parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
    ( [] )
# 428 "parser.ml"
               : 'in_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 100 "parser.mly"
    ( _1 )
# 435 "parser.ml"
               : 'in_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 106 "parser.mly"
    ( _1 )
# 442 "parser.ml"
               : 'out_params))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
    ( [] )
# 448 "parser.ml"
               : 'local_params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'param_list_semicol) in
    Obj.repr(
# 113 "parser.mly"
    ( _2 )
# 455 "parser.ml"
               : 'local_params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 118 "parser.mly"
    ( _1 )
# 462 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 120 "parser.mly"
    ( _1 @ _3 )
# 470 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    Obj.repr(
# 125 "parser.mly"
    ( _1 )
# 477 "parser.ml"
               : 'param_list_semicol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list_semicol) in
    Obj.repr(
# 127 "parser.mly"
    ( _1 @ _3 )
# 485 "parser.ml"
               : 'param_list_semicol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 133 "parser.mly"
      ( let typ = _3 in
        List.map (fun id -> (id, typ)) _1 )
# 494 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eq) in
    Obj.repr(
# 139 "parser.mly"
    ( [_1] )
# 501 "parser.ml"
               : 'eq_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'eq) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'eq_list) in
    Obj.repr(
# 141 "parser.mly"
    ( _1 :: _2 )
# 509 "parser.ml"
               : 'eq_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
    ( { peq_patt = _1; peq_expr = _3; } )
# 517 "parser.ml"
               : 'eq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 151 "parser.mly"
    ( mk_patt (PP_ident _1) )
# 524 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ident_comma_list) in
    Obj.repr(
# 153 "parser.mly"
    ( mk_patt (PP_tuple(_2::_4)) )
# 532 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
    ( _2 )
# 539 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 160 "parser.mly"
    ( _1 )
# 546 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 162 "parser.mly"
    ( mk_expr (PE_ident _1))
# 553 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_comma_list_empty) in
    Obj.repr(
# 164 "parser.mly"
    ( mk_expr (PE_app (_1, _3)))
# 561 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
    ( mk_expr (PE_if (_2, _4, _6)) )
# 570 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
    ( mk_expr (PE_fby (_1, _3)) )
# 578 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 170 "parser.mly"
    ( mk_expr (PE_binop (Badd, _1, _3)) )
# 586 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "parser.mly"
    ( mk_expr (PE_binop (Badd_f, _1, _3)) )
# 594 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 174 "parser.mly"
    ( mk_expr (PE_binop (Bsub, _1, _3)) )
# 602 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "parser.mly"
    ( mk_expr (PE_binop (Bsub_f, _1, _3)) )
# 610 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
    ( mk_expr (PE_binop (Bmul, _1, _3)) )
# 618 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 180 "parser.mly"
    ( mk_expr (PE_binop (Bmul_f, _1, _3)) )
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 182 "parser.mly"
    ( mk_expr (PE_binop (Bdiv, _1, _3)) )
# 634 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "parser.mly"
    ( mk_expr (PE_binop (Bdiv_f, _1, _3)) )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Asttypes.binop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
    ( mk_expr (PE_binop (_2, _1, _3)) )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
    ( mk_expr (PE_binop (Beq, _1, _3)) )
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
    ( mk_expr (PE_binop (Bneq, _1, _3)) )
# 667 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "parser.mly"
    ( mk_expr (PE_binop (Band, _1, _3)) )
# 675 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "parser.mly"
    ( mk_expr (PE_binop (Bor, _1, _3)) )
# 683 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 196 "parser.mly"
    ( mk_expr (PE_unop (Uminus, _2)) )
# 690 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 198 "parser.mly"
    ( mk_expr (PE_unop (Uminus_f, _2)) )
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 200 "parser.mly"
    ( mk_expr (PE_unop (Unot, _2)) )
# 704 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr_comma_list) in
    Obj.repr(
# 202 "parser.mly"
    ( mk_expr (PE_tuple (_2::_4)) )
# 712 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 207 "parser.mly"
    ( mk_expr (PE_const Cunit) )
# 718 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 209 "parser.mly"
    ( mk_expr (PE_const (Cbool _1)) )
# 725 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 211 "parser.mly"
    ( mk_expr (PE_const (Cint _1)) )
# 732 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 213 "parser.mly"
    ( mk_expr (PE_const (Cfloat _1)) )
# 739 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 215 "parser.mly"
    ( mk_expr (PE_const (Cstring _1)) )
# 746 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident_comma_list) in
    Obj.repr(
# 220 "parser.mly"
    ( _1 :: _3 )
# 754 "parser.ml"
               : 'ident_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 221 "parser.mly"
        ( [_1] )
# 761 "parser.ml"
               : 'ident_comma_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 225 "parser.mly"
    ( [] )
# 767 "parser.ml"
               : 'expr_comma_list_empty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_comma_list) in
    Obj.repr(
# 226 "parser.mly"
                  ( _1 )
# 774 "parser.ml"
               : 'expr_comma_list_empty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_comma_list) in
    Obj.repr(
# 231 "parser.mly"
    ( _1 :: _3 )
# 782 "parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 232 "parser.mly"
       ( [_1] )
# 789 "parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 236 "parser.mly"
         ( Tunit )
# 795 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 237 "parser.mly"
         ( Tbool )
# 801 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 238 "parser.mly"
         ( Tint )
# 807 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 239 "parser.mly"
         ( Tfloat )
# 813 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 240 "parser.mly"
         ( Tstring )
# 819 "parser.ml"
               : 'typ))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.p_file)
