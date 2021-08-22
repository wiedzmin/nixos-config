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
        debug-awesome = mkWMDebugScript "debug-awesome" pkgs.awesome "awesome -c ~/.config/awesome/rc.lua";
      };

      home-manager.users.${user} = {
        home.packages = with pkgs; [ debug-awesome ];
        xdg.configFile = {
          "awesome/rc.lua".text = readSubstituted ../../subst.nix ./rc.lua;
          "awesome/lib/controls.lua".text = readSubstituted ../../subst.nix ./lib/controls.lua;
          "awesome/lib/menus.lua".text = readSubstituted ../../subst.nix ./lib/menus.lua;
          "awesome/lib/themes.lua".text = readSubstituted ../../subst.nix ./lib/themes.lua;
          "awesome/lib/utils.lua".text = readSubstituted ../../subst.nix ./lib/utils.lua;
          "awesome/lib/widgets.lua".text = readSubstituted ../../subst.nix ./lib/widgets.lua;
          "awesome/lib/windows.lua".text = readSubstituted ../../subst.nix ./lib/windows.lua;
          "awesome/lib/eminent" = { source = ./lib/eminent; recursive = true; };
          "awesome/lib/ezconfig" = { source = ./lib/ezconfig; recursive = true; };
          "awesome/lib/hints" = { source = ./lib/hints; recursive = true; };
          "awesome/lib/lain" = { source = ./lib/lain; recursive = true; };
          "awesome/lib/vicious" = { source = ./lib/vicious; recursive = true; };
          "awesome/themes" = { source = ./themes; recursive = true; };
        };
      };
    })
  ];
}
