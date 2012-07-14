type token =
  | ID of (string)
  | NAT of (int)
  | FLOAT of (string)
  | EOF
  | BR_OPN
  | BR_CLS
  | ARROW
  | COLON
  | KW_CONST
  | KW_FLOAT
  | KW_INT
  | KW_VEC2
  | KW_VEC3
  | KW_VEC4

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> MlslAst.topdecl list
