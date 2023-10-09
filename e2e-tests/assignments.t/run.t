Reassigned
  $ typeshell reassigned.tsh
  Fatal error (line 2): Reassigned constant 'a'
  [2]

Undeclared
  $ typeshell reassign_undeclared.tsh
  Fatal error (line 1): Undeclared variable 'undeclared_variable'
  [2]
  $ typeshell undeclared_assign.tsh
  Fatal error (line 1): Undeclared variable 'undeclared_variable'
  [2]
  $ typeshell undeclared_echo.tsh
  Fatal error (line 1): Undeclared variable 'undeclared_variable'
  [2]
  $ typeshell undeclared_reassign.tsh
  Fatal error (line 2): Undeclared variable 'undeclared_variable'
  [2]

Redeclared
  $ typeshell already_declared.tsh
  Fatal error (line 2): Already declared variable 'a'
  [2]

Multiple errors
  $ typeshell multiple_invalid.tsh
  Fatal error (line 2): Already declared variable 'a'
  Fatal error (line 3): Reassigned constant 'a'
  [2]
