Transpilation
  $ typeshell -t echo.sh echo.tsh
  $ cat echo.sh
  #!/bin/bash
  set -e
  
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
  ./echo.sh: line 6: TEST_ENV: Null environment variable
  [1]

Empty env fails
  $ TEST_ENV="" ./echo.sh
  A
  ./echo.sh: line 6: TEST_ENV: Null environment variable
  [1]

Cleanup
  $ rm echo.sh

Standard library
  $ typeshell -t stdlib.sh stdlib.tsh
  $ cat stdlib.sh
  #!/bin/bash
  set -e
  
  echo 'echo OK'
  declare -r source='test.txt'
  declare -r target="${TEST_TARGET:?"Null environment variable"}"
  cp "${source}" "${target}"
  grep 'grep' "${target}"
  $ chmod +x stdlib.sh
  $ TEST_TARGET=target.txt ./stdlib.sh
  echo OK
  grep OK
  $ rm target.txt

Cleanup
  $ rm stdlib.sh

Exit on failure
  $ typeshell -t fail_fast.sh fail_fast.tsh
  $ cat fail_fast.sh
  #!/bin/bash
  set -e
  
  echo 'Before error'
  grep '--not-an-option' 'test.txt'
  echo 'After error'
  $ chmod +x fail_fast.sh
  $ ./fail_fast.sh 2>/dev/null
  Before error
  [2]

Cleanup
  $ rm fail_fast.sh
