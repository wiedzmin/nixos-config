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
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs packages like Hyperbole/EEV/ifany";
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
      ide.emacs.core.extraPackages = epkgs: [ epkgs.hyperbole ];
      ide.emacs.core.config = builtins.readFile ./elisp/pim.el;
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
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "hyperbole-site" = {
          desc = "GNU Hyperbole site";
          remote = {
            url = "https://www.gnu.org/software/hyperbole/";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
        "hyperbole-man" = {
          desc = "GNU Hyperbole manual";
          remote = {
            url = "https://www.gnu.org/software/hyperbole/man/hyperbole.html";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
      };
    })
  ];
}
