Reassigned
  $ typeshell reassigned.tsh
  Fatal error: Reassigned constant 'a'
  [2]

Undeclared
  $ typeshell reassign_undeclared.tsh
  Fatal error: Undeclared variable 'undeclared_variable'
  [2]
  $ typeshell undeclared_assign.tsh
  Fatal error: Undeclared variable 'undeclared_variable'
  [2]
  $ typeshell undeclared_echo.tsh
  Fatal error: Undeclared variable 'undeclared_variable'
  [2]
  $ typeshell undeclared_reassign.tsh
  Fatal error: Undeclared variable 'undeclared_variable'
  [2]

Redeclared
  $ typeshell already_declared.tsh
  Fatal error: Already declared variable 'a'
  [2]

Multiple errors
  $ typeshell multiple_invalid.tsh
  Fatal error: Already declared variable 'a'
  Fatal error: Reassigned constant 'a'
  [2]
