{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.workstation.input.core;
  user = config.attributes.mainUser.name;
in {
  options = {
    workstation.input.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable core workstation input customizations";
      };
      xmodmap.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use xmodmap";
      };
      xmodmap.rc = mkOption {
        type = types.lines;
        default = "";
        description = "Xmodmaprc contents";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      hardware = {
        trackpoint = {
          enable = true;
          sensitivity = 255;
          speed = 200;
          emulateWheel = true;
        };
      };
      services.xserver.libinput = { # TODO: move to specialized xserver module when exists
        enable = true;
        touchpad = {
          naturalScrolling = true;
          disableWhileTyping = true;
          tapping = false;
          tappingDragLock = false;
          accelSpeed = "0.6";
        };
      };
    })
    (mkIf cfg.xmodmap.enable {
      services.xserver.displayManager.sessionCommands = let xmodmaprc = pkgs.writeText "xmodmaprc" cfg.xmodmap.rc;
      in ''
        ${pkgs.xlibs.xmodmap}/bin/xmodmap ${xmodmaprc}
        ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "clear Lock"
      '';
    })
  ];
}
