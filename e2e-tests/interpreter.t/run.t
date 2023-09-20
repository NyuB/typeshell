String literal declaration and echo
  $ typeshell declare_echo.tsh
  a = A

Env variable reading
  $ TSH=Ok typeshell assign_env.tsh
  Ok
  $ typeshell assign_env.tsh
  Fatal error: exception Typeshell.Lang.Interpreter.NullEnvironmentVariable("TSH")
  [2]
  $ TSH="" typeshell assign_env.tsh
  Fatal error: exception Typeshell.Lang.Interpreter.NullEnvironmentVariable("TSH")
  [2]

Mutable variable
  $ typeshell var.tsh
  Mutable ?
  Mutated !
  Mutable ?
