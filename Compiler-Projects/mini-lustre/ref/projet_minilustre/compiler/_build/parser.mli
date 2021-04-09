type token =
  | AND
  | ARROW
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
  | MERGE
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
  | WHEN

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.p_file
