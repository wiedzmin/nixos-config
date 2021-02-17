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
      services.xserver.xkbOptions = "compose:ralt"; # rctrl
      home-manager.users.${user} = {
        home.file = {
          ".XCompose".text = ''
            include "${pkgs.xorg.libX11}/share/X11/locale/en_US.UTF-8/Compose"

            <Multi_key> <m> <o> : "ө"
            <Multi_key> <m> <O> : "Ө"
            <Multi_key> <m> <u> : "ү"
            <Multi_key> <m> <U> : "Ү"
          '';
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
