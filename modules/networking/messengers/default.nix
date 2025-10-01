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
      whatsapp.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WhatsApp desktop client";
      };
      zoom.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Zoom";
      };
      webex.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WebEx";
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
        home.packages = with pkgs; [ tdesktop tdl ] ++
          optionals (cfg.whatsapp.enable) [ whatsie ] ++
          optionals (cfg.zoom.enable) [ zoom-us ] ++
          optionals (cfg.webex.enable) [ webex ];
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
      wmCommon.autostart.entries = optionals cfg.telegram.autostart [{ cmd = "${pkgs.telegram-desktop}/bin/Telegram"; }];
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
          url = "https://zevlg.github.io/telega.el/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "telegram/service/status" = {
          desc = "Telegram service status";
          url = "https://downdetector.com/status/telegram/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "telegram/api/reference/links" = {
          desc = "telegram API reference / links";
          url = "https://core.telegram.org/api/links";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "telegram/api/reference/usernames" = {
          desc = "telegram API reference / usernames";
          url = "https://core.telegram.org/api/invites#public-usernames";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
      };
    })
  ];
}
