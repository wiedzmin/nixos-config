{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.input.core;
  user = config.attributes.mainUser.name;
in
{
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
      xcompose.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use XCompose functionality";
      };
      xcompose.key = mkOption {
        type = types.enum [ "ralt" "rctrl" ]; # TODO: add more options
        default = "ralt";
        description = "XCompose activation key";
      };
      xcompose.mappings = mkOption {
        type = types.lines;
        default = '''';
        description = "Custom XCompose mappings";
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
      services.xserver.libinput = {
        # TODO: move to specialized xserver module when exists
        enable = true;
        touchpad = {
          naturalScrolling = true;
          disableWhileTyping = true;
          tapping = false;
          tappingDragLock = false;
          accelSpeed = "0.1";
        };
      };
      services.tp-auto-kbbl = {
        # TODO: consider moving to `video/backlight`
        enable = false; # because of input device names periodical skew, maybe because of dockstation usage
        # maybe could be re-enabled after multiple devices support addition
        device = optionalString (config.attributes.hardware.inputDevices.keyboard != [ ])
          (builtins.head config.attributes.hardware.inputDevices.keyboard);
        # TODO: use full list + mouse devices after multiple devices support addition
        arguments = [
          # TODO: consider adding options
          "--brightness 2"
          "--no-dim"
        ];
      };
    })
    (mkIf (cfg.enable && cfg.xmodmap.enable) {
      services.xserver.displayManager.sessionCommands =
        let xmodmaprc = pkgs.writeText "xmodmaprc" cfg.xmodmap.rc;
        in
        ''
          ${pkgs.xorg.xmodmap}/bin/xmodmap ${xmodmaprc}
          ${pkgs.xorg.xmodmap}/bin/xmodmap -e "clear Lock"
        '';
    })
    (mkIf (cfg.enable && cfg.xcompose.enable) {
      services.xserver.xkbOptions = "compose:${cfg.xcompose.key}";
      home-manager.users."${user}" = {
        home.file = {
          ".XCompose".text = ''
            include "${pkgs.xorg.libX11}/share/X11/locale/en_US.UTF-8/Compose"

            ${cfg.xcompose.mappings}
          '';
        };
      };
    })
  ];
}
