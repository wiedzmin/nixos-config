{ bash, config, feh, lib, pkgs, ... }:
with import ../const.nix { inherit lib config pkgs; };
''
  #!${bash}/bin/bash

  ${feh}/bin/feh --bg-fill ${wallpaperBaseDir}/${wallpaperCurrent}
''
