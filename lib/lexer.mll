{
    open Lexing
    open Parser

    exception SyntaxError of string

    (* Remove leading '${' and trailing '}' *)
    let extract_dollar_content s = String.sub s 2 (String.length s - 3)

    (* Remove surrounding '"' *)
    let extract_string_literal_content s = String.sub s 1 (String.length s - 2)
}

let id = ['a'-'z' 'A'-'Z' '_' '0'-'9']+
let string = '"' ([^'"'] | "\\\"")* '"'
let dollar_id = '$' '{' id '}'
let newline = ('\r' | '\n' | "\r\n")
let white = [' ' '\t']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  {  new_line lexbuf; read lexbuf }
  | "val" { VAL }
  | "var" { VAR }
  | "echo" { ECHO }
  | '=' { EQ }
  | ';' { SEPARATOR }
  | string { STRING_LITERAL (extract_string_literal_content (Lexing.lexeme lexbuf)) }
  | dollar_id { DOLLAR_ID (extract_dollar_content (Lexing.lexeme lexbuf)) }
  | id { ID ( Lexing.lexeme lexbuf ) }
  | _ { raise (SyntaxError (Printf.sprintf "Unexpected char: %s" (Lexing.lexeme lexbuf))) }
  | eof      { EOF }