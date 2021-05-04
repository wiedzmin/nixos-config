{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.workstation.randr;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  kill-compton = pkgs.writeScriptBin "kill-compton" ''
    ${pkgs.procps}/bin/pkill -f compton
  '';
  headsOrientationModule = types.submodule {
    options = {
      primary = mkOption {
        type = types.enum [ "normal" "left" "right" "inverted" ];
        default = "normal";
        description = "Primary head orientation";
      };
      secondary = mkOption {
        type = types.enum [ "normal" "left" "right" "inverted" ];
        default = "normal";
        description = "Secondary head orientation";
      };
    };
  };
in {
  options = {
    workstation.randr = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XRandR automation";
      };
      defaults.rate = mkOption {
        type = types.str;
        default = "60.00";
        description = "XRandR default refresh rate";
      };
      defaults.gamma = mkOption {
        type = types.str;
        default = "1.0:0.909:0.833";
        description = "XRandR default gamma settings";
      };
      heads.orientation = mkOption {
        type = headsOrientationModule; # TODO: investigate how to keep defaults in module options
        description = "external head orientation";
      };
      hooks = mkOption {
        type = types.attrs;
        default = { };
        description = "Autorandr hooks";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM bindings";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        programs.autorandr = {
          enable = true;
          hooks = lib.optionalAttrs (hm.services.compton.enable) {
            predetect = { "kill-compton" = "${kill-compton}/bin/kill-compton"; };
          } // cfg.hooks;
        };
      };
      services.udev.extraRules = ''
        ACTION=="change", SUBSYSTEM=="drm", RUN+="${pkgs.autorandr}/bin/autorandr --batch --change --default default"
      '';

      nixpkgs.config.packageOverrides = _: rec {
        xrandrutil = mkPythonScriptWithDeps "xrandrutil" (with pkgs; [ autorandr nurpkgs.pystdlib python3Packages.ewmh ])
          (builtins.readFile ./scripts/xrandrutil.py);
        rescreen = mkShellScriptWithDeps "rescreen" (with pkgs; [ autorandr ]) ''
          rescreen-$(autorandr --detected)-i3
        '';
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "m" ];
          cmd = "${pkgs.autorandr}/bin/autorandr --load mobile";
          mode = "xserver";
        }
        {
          key = [ "a" ];
          cmd = "${pkgs.xrandrutil}/bin/xrandrutil --switch --dmenu-font '${config.wmCommon.fonts.dmenu}'";
          mode = "xserver";
        }
        {
          key = [ "r" ];
          cmd = "${pkgs.rescreen}/bin/rescreen";
          mode = "xserver";
        }
        {
          key = [ "c" ];
          cmd = "${pkgs.find-cursor}/bin/find-cursor";
          mode = "xserver";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ rescreen xrandrutil ]; };
    })
  ];
}
