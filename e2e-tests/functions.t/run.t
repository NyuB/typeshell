Argument expression modes
  $ TEST_ENV="Environment variable" typeshell expression.tsh
  String
  Environment variable
  Variable

Unknown function
  $ typeshell unknown.tsh
  Fatal error: Undeclared function 'some_fun'
  [2]

Invalid label/option
  $ typeshell multiple_invalid.tsh
  Fatal error: Invalid label 'foo'
  Fatal error: Invalid option '--nonsense'
  Fatal error: Too many arguments, expected 2 arguments but got 3
  Fatal error: Missing argument, expected 2 arguments but got 1
  [2]
