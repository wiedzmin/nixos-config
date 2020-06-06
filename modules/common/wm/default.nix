{ config, lib, pkgs, ... }:
with lib;

let cfg = config.wmCommon;
in {
  options = {
    wmCommon = {
      keys = mkOption {
        type = types.attrs;
        default = { };
        description = "Keybindings map.";
      };
      fonts.default = mkOption {
        type = types.str;
        default = "";
        description = "Current WM `internal` default font' definition.";
      };
      fonts.dmenu = mkOption {
        type = types.str;
        default = "";
        description = "Current WM `internal` dmenu font' definition.";
      };
      fonts.statusbar = mkOption {
        type = types.str;
        default = "";
        description = "Current WM `internal` statusbar font' definition.";
      };
    };
  };
}
