Reassigned
  $ typeshell reassigned.tsh
  Fatal error: exception Typeshell.Compiler.Assignments.ReassignedConstant("a")
  [2]

Undeclared
  $ typeshell reassign_undeclared.tsh
  Fatal error: exception Typeshell.Compiler.UndeclaredVariable("undeclared_variable")
  [2]
  $ typeshell undeclared_assign.tsh
  Fatal error: exception Typeshell.Compiler.UndeclaredVariable("undeclared_variable")
  [2]
  $ typeshell undeclared_echo.tsh
  Fatal error: exception Typeshell.Compiler.UndeclaredVariable("undeclared_variable")
  [2]
  $ typeshell undeclared_reassign.tsh
  Fatal error: exception Typeshell.Compiler.UndeclaredVariable("undeclared_variable")
  [2]

Redeclared
  $ typeshell already_declared.tsh
  Fatal error: exception Typeshell.Compiler.Assignments.AlreadyDeclaredVariable("a")
  [2]
