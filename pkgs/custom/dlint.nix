{ bash, docker, ... }: ''
  #!${bash}/bin/bash

  if [ -z $1 ]; then
      echo "Dockerfile missing"
      exit 1
  fi

  ${docker}/bin/docker run --rm -i -v $(realpath $1):/Dockerfile redcoolbeans/dockerlint
''
