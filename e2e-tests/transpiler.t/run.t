Echo
  $ typeshell -t echo.sh echo.tsh
  $ cat echo.sh
  #!/bin/bash
  
  declare -r a='A'
  echo "${a}"
  test_env="${TEST_ENV}"
  echo "${test_env}"
  $ chmod +x echo.sh
  $ TEST_ENV=B ./echo.sh
  A
  B
