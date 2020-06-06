{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.wmCommon;
in {
  options = {
    wmCommon = {
      enable = mkOption {
        type = types.bool;
        default = config.wm.xmonad.enable; # TODO: add entries `or` later
        description = "Whether to enable common WM infrastructure.";
      };
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
      keybindingsCachePath = mkOption {
        type = types.str;
        default = homePrefix "keybindings.list";
        description = "Path to file with cached keybindings.";
        visible = false;
        internal = true;
        readOnly = true;
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        keybindings = writePythonScriptWithPythonPackages "keybindings" [ pkgs.python3Packages.redis pkgs.yad ]
          (builtins.readFile
            (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./keybindings.py; })));
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.activation.purgeKeybindingsCache = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "rm -f ${cfg.keybindingsCachePath}";
        };
        home.packages = with pkgs; [ keybindings ];
      };
      wmCommon.keys = { "M-k" = { cmd = "keybindings"; }; };
    })
  ];
}
