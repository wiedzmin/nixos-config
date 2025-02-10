{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ext.networking.messengers;
  user = config.attributes.mainUser.name;
in
{
  options = {
    ext.networking.messengers = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various messengers";
      };
      telegram.autostart = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to start telegram automatically wit X session";
      };
      zoom.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Zoom";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs-related setup (primarily telega.el)";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        # TODO: review `tdl` docs at https://docs.iyear.me/tdl/
        home.packages = with pkgs; [ tdesktop ] ++ optionals (cfg.zoom.enable) [ zoom-us tdl ];
      };
      workstation.input.keyboard.xremap.config = {
        keymap = [
          {
            name = "Telegram";
            application = { only = "TelegramDesktop"; };
            remap = {
              "C-s" = [ "Esc" "Esc" "Esc" ];
              "M-Comma" = "Esc";
              "C-Shift-s" = "C-f";
              "C-t" = [ "Shift-Left" "C-x" "Left" "C-v" "Right" ];
              "C-x" = {
                remap = {
                  "C-c" = "C-q";
                };
              };
            };
          }
        ];
      };
      workstation.input.keyboard.xkeysnail.rc = ''
        define_keymap(re.compile("TelegramDesktop"), {
            K("C-x"): {
                K("C-c"): K("C-q"),
            },
            K("C-s"): [K("Esc"), K("Esc"), K("Esc")],
            K("M-comma"): K("Esc"),
            K("C-Shift-s"): K("C-f"),
            K("C-t"): [K("Shift-Left"), K("C-x"), K("Left"), K("C-v"), K("Right")],
        }, "Telegram")

        define_keymap(re.compile("Slack"), {
            K("C-y"): K("C-v"),
        }, "Slack")
      '';
      wmCommon.autostart.entries = optionals cfg.telegram.autostart [{ cmd = "${pkgs.tdesktop}/bin/telegram-desktop"; }];
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        messengers = {
          matches = [
            {
              trigger = ":shr";
              replace = "¯\\_(ツ)_/¯";
            }
            {
              trigger = ":sm";
              replace = "ツ";
            }
            {
              trigger = ":cr";
              replace = "©";
            }
            {
              trigger = ":stt";
              replace = "~~$|$~~";
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      # NOTE: epkgs.telega/stable | melpaPackages.telega/unstable
      ide.emacs.core.extraPackages = epkgs: [ epkgs.melpaPackages.telega ]; # review https://github.com/zevlg/telega.el as it goes
      ide.emacs.core.config = ''
        (use-package telega
          :commands (telega))
      '';
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "telega/manual" = {
          desc = "Telega.el manual";
          remote.url = "https://zevlg.github.io/telega.el/";
        };
        "telegram/service/status" = {
          desc = "Telegram service status";
          remote.url = "https://downdetector.com/status/telegram/";
        };
      };
    })
  ];
}
