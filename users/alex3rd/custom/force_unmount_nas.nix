{ bash, config, coreutils, dunst, gnugrep, lib, pkgs, yq-go, ... }:
with import ../secrets/const.nix { inherit lib config pkgs; };
let nasConfigPath = "$HOME/.config/synology/nas.yml";
in ''
  #!${bash}/bin/bash

  function unmount_volume() {
      VOLUME=$1
      NAS_MOUNT_PATH=$(${yq-go}/bin/yq r $${nasConfigPath} nas.mount.basedir)
      YET_MOUNTED=$(cat /etc/mtab | ${gnugrep}/bin/grep catscan | ${coreutils}/bin/cut -d ' '  -f 1 | ${gnugrep}/bin/grep $VOLUME)
      if [[ ! -z $YET_MOUNTED ]]; then
          fusermount -u $NAS_MOUNT_PATH/$VOLUME
          ${dunst}/bin/dunstify -t 3000 "Volume $VOLUME succesfully unmounted!"
      else
          ${dunst}/bin/dunstify -t 7000 "Volume '$VOLUME' already unmounted!"
      fi
  }

  CONFIGFILE=${nasConfigPath}
  if [[ ! -f $CONFIGFILE ]]; then
      ${dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
      exit 1
  fi
  NAS_HOSTNAME=$(${yq-go}/bin/yq r $CONFIGFILE nas.hostname)

  mounted_nas_volumes=$(cat /etc/mtab | ${gnugrep}/bin/grep $NAS_HOSTNAME | ${coreutils}/bin/cut -d ' '  -f 1)
  for i in "''${mounted_nas_volumes[@]}"
  do
      unmount_volume "$i"
  done
''
