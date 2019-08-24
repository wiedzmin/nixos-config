{ bash, config, gawk, gnused, lib, pkgs, rofi, xsel, yad, ... }:
with import ../../../pkgs/util.nix { inherit lib config pkgs; };
with import ../../../pkgs/const.nix { inherit lib config pkgs; };
with import ../secrets/const.nix { inherit lib config pkgs; };
let
  hostTraitsIpOutputPosition = 4;
in
''
  #!${bash}/bin/bash

  ${listOfSetsToShellHashtable
  (
    unfoldListOfSetsByAttr
      (jobExtraHosts ++ extraHosts)
      "hostNames"
  )
  "hostNames"
  "EXTRA_HOSTS"
  false}

  list_extra_hosts() {
      for i in "''${!EXTRA_HOSTS[@]}"
      do
          echo "$i"
      done
  }

  main() {
      SELECTED_HOST=$( (list_extra_hosts) | ${rofi}/bin/rofi -dmenu -p "Select" )
      if [ -n "$SELECTED_HOST" ]; then
          RESULT="$SELECTED_HOST ''${EXTRA_HOSTS[$SELECTED_HOST]}"
          RESULT_NEWLINES=$(echo $RESULT | ${gnused}/bin/sed 's/ /\n/g' | \
                                           ${gnused}/bin/sed 's/${sedPlaceholderChar}/ /g')
          IP=$(echo $RESULT | ${gawk}/bin/awk '{print ${"$" + builtins.toString hostTraitsIpOutputPosition}}' | \
                              ${gnused}/bin/sed 's/${sedPlaceholderChar}/ /g')
          echo "$RESULT_NEWLINES" > /tmp/extra_host
          echo "$IP" | ${gawk}/bin/awk '{print $2}'| tr -d '\n' | ${xsel}/bin/xsel -i --clipboard
          ${yad}/bin/yad --filename /tmp/extra_host --text-info
          rm /tmp/extra_host
      fi
  }

  main

  exit 0
''
