{ bash, docker, ... }: ''
  #!${bash}/bin/bash

  if [ -z $1 ]; then
      echo "Dockerfile missing"
      exit 1
  fi

  ${docker}/bin/docker run --rm -i -v $(dirname $(realpath $1)):/root/ projectatomic/dockerfile-lint dockerfile_lint -p -f $(basename $1)
''
