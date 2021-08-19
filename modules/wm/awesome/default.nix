{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with import ../../wmutil.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.wm.awesome;
  user = config.attributes.mainUser.name;
in
{
  options = {
    wm.awesome = {
      config.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom config for AwesomeWM";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.config.enable {
      fonts.fonts = with pkgs; [ font-awesome ];

      shell.core.variables = [{ CURRENT_WM = "awesome"; global = true; emacs = true; }];

      nixpkgs.config.packageOverrides = _: rec {
        debug-awesome = mkShellScriptWithDeps "debug-awesome" (with pkgs; [ awesome xorg.xorgserver.out ]) ''
          Xephyr -ac -br -noreset -screen 1300x768 :1 &
          sleep 1
          DISPLAY=:1.0 awesome -c ~/.config/awesome/rc.lua
        '';
      };

      home-manager.users.${user} = {
        home.packages = with pkgs; [ debug-awesome ];
        xdg.configFile = {
          "awesome/rc.lua".source = ./rc.lua;
          "awesome/lib" = {
            source = ./lib;
            recursive = true;
          };
          "awesome/themes" = {
            source = ./themes;
            recursive = true;
          };
        };
      };
    })
  ];
}
