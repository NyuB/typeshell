{
    open Lexing
    open Parser

    exception SyntaxError of string

    (* Remove leading '${' and trailing '}' *)
    let extract_dollar_content s = String.sub s 2 (String.length s - 3)

    (* Remove surrounding '"' *)
    let extract_string_literal_content s = String.sub s 1 (String.length s - 2)

    (* Remove n leading characters *)
    let remove_first_n n s = String.sub s n (String.length s - n)
}

let word_char = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] word_char*
let string = '"' ([^'"'] | "\\\"")* '"'
let dollar_id = '$' '{' id '}'
let newline = ('\r' | '\n' | "\r\n")
let white = [' ' '\t']*
let separator = ';'
let short_option = '-' ['a' - 'z' 'A' - 'Z']
let option_name = word_char (word_char | '-')*
let long_option = "--" option_name

rule read =
  parse
  | white    { read lexbuf }
  | newline  {  new_line lexbuf; read lexbuf }
  | "val" { VAL }
  | "var" { VAR }
  | '=' { EQ }
  | ':' { COLON }
  | separator { SEPARATOR }
  | '"' { read_string (Buffer.create 0) lexbuf }
  | dollar_id { DOLLAR_ID (extract_dollar_content (Lexing.lexeme lexbuf)) }
  | id { ID ( Lexing.lexeme lexbuf ) }
  | short_option | long_option { OPTION (Lexing.lexeme lexbuf) }
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