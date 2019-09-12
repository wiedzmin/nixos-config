{ bash, nix, ... }: ''
  #!${bash}/bin/bash

  pause () {
      echo
      read -n 1 -s -r -p "Press any key to continue..."
      echo
  }

  RESULT_SYMLINK=''${1:-/etc/nixos/result}
  if [ ! -f "$RESULT_SYMLINK" ]; then
      echo "Cannot find symlink to recently built system, exiting"
      exit 1
  fi
  ISO_DIRNAME=$(readlink $RESULT_SYMLINK)/iso
  ISO_BASENAME=$(ls $ISO_DIRNAME)

  ISO_DEVICE=''${2:-/dev/sdb}
  if [ ! -b "$ISO_DEVICE" ]; then
      echo "USB drive NOT found, exiting"
      exit 1
  else
      echo "Going to burn $ISO_DIRNAME/$ISO_BASENAME to $ISO_DEVICE"
      pause
      sudo dd bs=4M if="$ISO_DIRNAME/$ISO_BASENAME" of="$ISO_DEVICE"
      # TODO: investigate unmounting/data flush issues
      echo "Burned successfully, unmounting pending"
      pause
      sudo umount "$ISO_DEVICE"
  fi
''
