{ bash, coreutils, docker-machine, perl, zip, ... }: ''
  #!${bash}/bin/bash

  if [ -z "$1" ]; then
      echo "Usage: machine-export.sh MACHINE_NAME"
      echo ""
      echo "Exports the specified docker-machine to a MACHINE_NAME.zip file"
      echo "Note: This script requires you to have the same \$MACHINE_STORAGE_PATH/certs available on all host systems"
      exit 0
  fi

  machine_name=$1

  ${docker-machine}/bin/docker-machine status $machine_name 2>&1 > /dev/null
  if [ $? -ne 0 ]; then
      echo "No such machine found"
      exit 1
  fi

  set -e

  MACHINE_STORAGE_PATH="$(MACHINE_STORAGE_PATH:-"$HOME/.docker/machine")"
  machine_path="$MACHINE_STORAGE_PATH/machines/$machine_name"
  tmp_path="/tmp/machine-export-$(date +%s%3)"

  # copy to /tmp and strip out $MACHINE_STORAGE_PATH
  ${coreutils}/bin/mkdir -p $tmp_path
  ${coreutils}/bin/cp -r "$machine_path" "$tmp_path"
  ${perl}/bin/perl -pi -e "s|$MACHINE_STORAGE_PATH|__MACHINE__STORAGE_PATH__|g" $tmp_path/$machine_name/config.json

  # create zip
  ${coreutils}/bin/rm -f "$machine_name.zip"
  ${zip}/bin/zip -rj "$machine_name.zip" "$tmp_path/$machine_name" > /dev/null

  echo "Exported machine to $machine_name.zip"

  # cleanup
  ${coreutils}/bin/rm -rf $tmp_path
''
