Reassigned
  $ typeshell reassigned.tsh
  Fatal error: exception Typeshell.Lang.ReassignedConstant("a")
  [2]

Undeclared
  $ typeshell undeclared_echo.tsh
  Fatal error: exception Typeshell.Lang.UndeclaredVariable("undeclared_variable")
  [2]
  $ typeshell undeclared_reassign.tsh
  Fatal error: exception Typeshell.Lang.UndeclaredVariable("undeclared_variable")
  [2]