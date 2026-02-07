{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.pim.core;
  user = config.attributes.mainUser.name;
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
      wsMapping.rules = mkOption {
        type = types.listOf types.attrs;
        default = [
          {
            class = "obsidian";
            title = "Archive Obsidian";
            desktop = "sandbox";
            activate = true;
          }
          {
            class = "obsidian";
            title = "Common Obsidian";
            desktop = "sandbox";
            activate = true;
          }
          {
            class = "obsidian";
            title = "Culture Obsidian";
            desktop = "var";
            activate = true;
          }
          {
            class = "obsidian";
            title = "Emacs Obsidian";
            desktop = "main";
            activate = true;
          }
          {
            class = "obsidian";
            title = "Housekeeping Obsidian";
            desktop = "main";
            activate = true;
          }
          {
            class = "obsidian";
            title = "NixOS Obsidian";
            desktop = "main";
            activate = true;
          }
          {
            class = "obsidian";
            title = "Problems Obsidian";
            desktop = "sandbox";
            activate = true;
          }
          {
            class = "obsidian";
            title = "Tech Obsidian";
            desktop = "main";
            activate = true;
          }
          {
            class = "obsidian";
            title = "WorkEnv Obsidian";
            desktop = "main";
            activate = true;
          }
        ];
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.emacs.automation.enable) {
      attributes.transientPackages = with pkgs; [ obsidian rofi-obsidian ];
      ide.emacs.core.extraPackages = epkgs: [ epkgs.melpaStablePackages.hyperbole ];
      ide.emacs.core.config = builtins.readFile ./elisp/pim.el;
      wmCommon.wsMapping.rules = cfg.wsMapping.rules;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = lib.optionals config.navigation.bookmarks.enable [
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ "e" ];
          cmd = "go#projects open --path ${wsRoot roots "github"}/edrx/eev";
          mode = "dev";
        })
        {
          key = [ "o" ];
          cmd = "${pkgs.rofi}/bin/rofi -modi obsidian:rofi-obsidian -show obsidian -combi-modi obsidian";
          mode = "run";
        }
      ];
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "hyperbole-site" = {
          desc = "GNU Hyperbole site";
          url = "https://www.gnu.org/software/hyperbole/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "hyperbole-man" = {
          desc = "GNU Hyperbole manual";
          url = "https://www.gnu.org/software/hyperbole/man/hyperbole.html";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        obsidian-vaults-Archive = {
          tags = [ "obsidian" "archive" ];
          path = homePrefix user "docs/obsidian/Archive";
          windowRules = [
            {
              class = "Emacs";
              title = "Archive obsidian";
              desktop = "sandbox";
              activate = true;
            }
          ];
        };
        obsidian-vaults-Common = {
          tags = [ "obsidian" "common" ];
          path = homePrefix user "docs/obsidian/Common";
          windowRules = [
            {
              class = "Emacs";
              title = "Common obsidian";
              desktop = "sandbox";
              activate = true;
            }
          ];
        };
        obsidian-vaults-Culture = {
          tags = [ "obsidian" "culture" ];
          path = homePrefix user "docs/obsidian/Culture";
          windowRules = [
            {
              class = "Emacs";
              title = "Culture obsidian";
              desktop = "var";
              activate = true;
            }
          ];
        };
        obsidian-vaults-Emacs = {
          tags = [ "obsidian" "emacs" ];
          path = homePrefix user "docs/obsidian/Emacs";
          windowRules = [
            {
              class = "Emacs";
              title = "Emacs obsidian";
              desktop = "main";
              activate = true;
            }
          ];
        };
        obsidian-vaults-Housekeeping = {
          tags = [ "obsidian" "housekeeping" ];
          path = homePrefix user "docs/obsidian/Housekeeping";
          windowRules = [
            {
              class = "Emacs";
              title = "Housekeeping obsidian";
              desktop = "main";
              activate = true;
            }
          ];
        };
        obsidian-vaults-NixOS = {
          tags = [ "obsidian" "nixos" ];
          path = homePrefix user "docs/obsidian/NixOS";
          windowRules = [
            {
              class = "Emacs";
              title = "NixOS obsidian";
              desktop = "main";
              activate = true;
            }
          ];
        };
        obsidian-vaults-Problems = {
          tags = [ "obsidian" "problems" ];
          path = homePrefix user "docs/obsidian/Problems";
          windowRules = [
            {
              class = "Emacs";
              title = "Problems obsidian";
              desktop = "sandbox";
              activate = true;
            }
          ];
        };
        obsidian-vaults-Tech = {
          tags = [ "obsidian" "tech" ];
          path = homePrefix user "docs/obsidian/Tech";
          windowRules = [
            {
              class = "Emacs";
              title = "Tech obsidian";
              desktop = "main";
              activate = true;
            }
          ];
        };
        obsidian-vaults-WorkEnv = {
          tags = [ "obsidian" ];
          path = homePrefix user "docs/obsidian/WorkEnv";
          windowRules = [
            {
              class = "Emacs";
              title = "WorkEnv obsidian";
              desktop = "main";
              activate = true;
            }
          ];
        };
      };
    })
  ];
}
