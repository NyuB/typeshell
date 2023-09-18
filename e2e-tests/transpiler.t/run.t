Echo
  $ typeshell -t echo.sh echo.tsh
  $ cat echo.sh
  #!/bin/bash
  
  declare -r a='A'
  echo "${a}"
  test_env="${TEST_ENV:?"Null environment variable"}"
  echo "${test_env}"
  $ chmod +x echo.sh
  $ TEST_ENV=B ./echo.sh
  A
  B
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
