Argument expression modes
  $ TEST_ENV="Environment variable" typeshell expression.tsh
  String
  Environment variable
  Variable

Unknown function
  $ typeshell unknown.tsh
  Fatal error (line 2): Undeclared function 'some_fun'
  [2]

Invalid label/option
  $ typeshell multiple_invalid.tsh
  Fatal error (line 1): Invalid label 'foo'
  Fatal error (line 1): Invalid option '--nonsense'
  Fatal error (line 2): Too many arguments, expected 2 arguments but got 3
  Fatal error (line 3): Missing argument, expected 2 arguments but got 1
  [2]
