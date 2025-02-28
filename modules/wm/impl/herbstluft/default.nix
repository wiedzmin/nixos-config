{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.wm.herbstluft;
  user = config.attributes.mainUser.name;
in
{
  options = {
    wm.herbstluft = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Herbstluft WM";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = "Set `Herbstluft` as default WM";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      fonts.packages = with pkgs; [ font-awesome ];

      nixpkgs.config.packageOverrides = _: {
        debug-herbstluft = mkWMDebugScript
          pkgs "debug-herbstluft"
          pkgs.herbstluftwm
          [ pkgs.dzen2 ]
          config.attributes.hardware.monitors.internalHead
          ''herbstluftwm -c "$XDG_CONFIG_HOME/herbstluftwm/autostart"'';
      };

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ debug-herbstluft ];
        xdg.configFile = {
          "herbstluftwm/autostart".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./autostart ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [{
        assertion = !config.wm.awesome.isDefault && !config.wm.i3.isDefault && !config.wm.qtile.isDefault;
        message = "herbstluft: exactly one WM could be the default.";
      }];

      shell.core.variables = [{ CURRENT_WM = "herbstluft"; global = true; emacs = true; }];

      services.xserver = {
        windowManager = {
          herbstluftwm = {
            enable = true;
            package = pkgs.herbstluftwm;
            # configFile = [[file:~/workspace/repos/github.com/NixOS/nixpkgs/nixos/modules/services/x11/window-managers/herbstluftwm.nix::configFile = mkOption {]];
          };
        };
        displayManager = { defaultSession = "none+herbstluft"; };
      };
    })
  ];
}
