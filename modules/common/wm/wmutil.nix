{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };

rec {
  dmenu_runapps = writeShellScriptBinWithDeps "dmenu_runapps"
    (with pkgs; [ coreutils dmenu haskellPackages.yeganesh j4-dmenu-desktop ]) ''
      j4-dmenu-desktop --display-binary --dmenu="(cat ; (stest -flx $(echo $PATH | tr : ' ') | sort -u)) | \
        yeganesh -- -i -l 15 -fn '${config.attributes.wm.fonts.dmenu}'"
    '';
  dmenu_select_windows = writeShellScriptBinWithDeps "dmenu_select_windows" (with pkgs; [ coreutils dmenu wmctrl ]) ''
    wmctrl -a $(wmctrl -l | cut -d" " -f5- | dmenu -i -l 15 -fn '${config.attributes.wm.fonts.dmenu}')
  '';
}
