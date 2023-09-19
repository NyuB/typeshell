Lexer errors
  $ typeshell leading_digit.tsh
  Fatal error: exception Typeshell.Lexer.SyntaxError("Unexpected char: 0")
  [2]
String literals
  $ typeshell string_literals.tsh
  \t tab here <	> this should be tabulated
  \b kook
  \f form feed here <>
