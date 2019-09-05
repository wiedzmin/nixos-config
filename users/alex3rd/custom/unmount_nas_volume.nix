{ bash, config, coreutils, dunst, gnugrep, lib, pkgs, rofi, yq-go, ... }:
with import ../secrets/const.nix { inherit lib config pkgs; };
let
  nasConfigPath = "$HOME/.config/synology/nas.yml";
in
''
  #!${bash}/bin/bash

  function show_list() {
      contents=("$@")
      for i in "''${contents[@]}";
      do
          echo "$i"
      done
  }

  function ensure_config_exists() {
      if [[ ! -f $${nasConfigPath} ]]; then
          ${dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
          exit 1
      fi
  }

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

  main() {
      ensure_config_exists

      mounted_nas_volumes=$(cat /etc/mtab | ${gnugrep}/bin/grep catscan | ${coreutils}/bin/cut -d ' '  -f 1)
      selected_volume=$( (show_list "''${mounted_nas_volumes[@]}") | ${rofi}/bin/rofi -dmenu -p "Unmount: " )
      if [ -n "$selected_volume" ]; then
          unmount_volume "$selected_volume"
      fi
  }

  main

  exit 0
''
