{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.pim.obsidian;
  user = config.attributes.mainUser.name;
in
{
  options = {
    pim.obsidian = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Obsidian setup";
      };
      vaults.root = mkOption {
        type = types.str;
        default = homePrefix user "docs/obsidian";
        description = "Obsidian vaults root directory";
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
            title = "Culture Obsidian";
            desktop = "var";
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
    (mkIf (cfg.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          obsidian
          rofi-obsidian
        ];
      };
      wmCommon.wsMapping.rules = cfg.wsMapping.rules;
      content.core.documents.path = cfg.vaults.root;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = lib.optionals config.navigation.bookmarks.enable [
        {
          key = [ "o" ];
          cmd = "${pkgs.rofi}/bin/rofi -modi obsidian:rofi-obsidian -show obsidian -combi-modi obsidian";
          mode = "run";
        }
      ];
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        obsidian-vaults-root = {
          tags = [ "obsidian" "root" ];
          path = homePrefix user "docs/obsidian";
        };
        obsidian-vaults-Archive = {
          tags = [ "obsidian" "archive" ];
          path = "${cfg.vaults.root}/Archive";
          windowRules = [
            {
              class = "Emacs";
              title = "Archive obsidian";
              desktop = "sandbox";
              activate = true;
            }
          ];
        };
        obsidian-vaults-Culture = {
          tags = [ "obsidian" "culture" ];
          path = "${cfg.vaults.root}/Culture";
          windowRules = [
            {
              class = "Emacs";
              title = "Culture obsidian";
              desktop = "var";
              activate = true;
            }
          ];
        };
        obsidian-vaults-Housekeeping = {
          tags = [ "obsidian" "housekeeping" ];
          path = "${cfg.vaults.root}/Housekeeping";
          windowRules = [
            {
              class = "Emacs";
              title = "Housekeeping obsidian";
              desktop = "main";
              activate = true;
            }
          ];
        };
        obsidian-vaults-Problems = {
          tags = [ "obsidian" "problems" ];
          path = "${cfg.vaults.root}/Problems";
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
          path = "${cfg.vaults.root}/Tech";
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
          path = "${cfg.vaults.root}/WorkEnv";
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
