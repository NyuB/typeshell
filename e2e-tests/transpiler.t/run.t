Transpilation
  $ typeshell -t echo.sh echo.tsh
  $ cat echo.sh
  #!/bin/bash
  
  declare -r a='A'
  echo "${a}"
  test_var="${TEST_ENV:?"Null environment variable"}"
  echo "${test_var}"
  test_var="${a}"
  echo "${test_var}"
  $ chmod +x echo.sh

Nominal run
  $ TEST_ENV=B ./echo.sh
  A
  B
  A

Missing env fails
  $ ./echo.sh
  A
  ./echo.sh: line 5: TEST_ENV: Null environment variable
  [1]

Empty env fails
  $ TEST_ENV="" ./echo.sh
  A
  ./echo.sh: line 5: TEST_ENV: Null environment variable
  [1]
