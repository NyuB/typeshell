Argument expression modes
  $ TEST_ENV="Environment variable" typeshell expression.tsh
  String
  Environment variable
  Variable

Unknown function
  $ typeshell unknown.tsh
  Fatal error: exception Typeshell.Compiler.Function_Calls.UndeclaredFunction("some_fun")
  [2]
