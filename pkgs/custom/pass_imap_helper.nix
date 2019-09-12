{ bash, coreutils, pass, ... }: ''
  #!${bash}/bin/bash

  echo $(${pass}/bin/pass $1 | ${coreutils}/bin/head -n 1)
''
