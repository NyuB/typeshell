{
    open Lexing
    open Parser

    exception SyntaxError of string

    (* Remove leading '${' and trailing '}' *)
    let extract_dollar_content s = String.sub s 2 (String.length s - 3)

    (* Remove surrounding '"' *)
    let extract_string_literal_content s = String.sub s 1 (String.length s - 2)
}

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let string = '"' ([^'"'] | "\\\"")* '"'
let dollar_id = '$' '{' id '}'
let newline = ('\r' | '\n' | "\r\n")
let white = [' ' '\t']*
let separator = ';'

rule read =
  parse
  | white    { read lexbuf }
  | newline  {  new_line lexbuf; read lexbuf }
  | "val" { VAL }
  | "var" { VAR }
  | "echo" { ECHO }
  | '=' { EQ }
  | separator { SEPARATOR }
  | '"' { read_string (Buffer.create 0) lexbuf }
  | dollar_id { DOLLAR_ID (extract_dollar_content (Lexing.lexeme lexbuf)) }
  | id { ID ( Lexing.lexeme lexbuf ) }
  | _ { raise (SyntaxError (Printf.sprintf "Unexpected char: %s" (Lexing.lexeme lexbuf))) }
  | eof      { EOF }
  and read_string buf =
  parse
  | '"'       { STRING_LITERAL (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }