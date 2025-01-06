{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.pim.core;
in
{
  options = {
    pim.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable core PIM setup";
      };
      emacs.automation.enable = mkOption {
        # TODO: check if EEV and Hyperbole are mutually exclusive in any way
        # TODO: think if we should have separate options for them
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs packages like EEV and Hyperbole";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.emacs.automation.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.eev ];
      ide.emacs.core.config = builtins.readFile ./elisp/eev.el;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = lib.optionals config.navigation.bookmarks.enable [
        (goLocalDebugKeybinding config {
          key = [ "e" ];
          cmd = [ "projects" "open" "--path" "${wsRoot roots "github"}/edrx/eev" ];
          mode = "dev";
        })
      ];
    })
  ];
}
