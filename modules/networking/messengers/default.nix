{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.networking.messengers;
  user = config.attributes.mainUser.name;
in {
  options = {
    ext.networking.messengers = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various messengers";
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
        home.packages = with pkgs; [ tdesktop zoom-us tdlib ];
        xdg.configFile."espanso/user/telegram.yml".text = ''
          name: telegram
          parent: default
          filter_class: "TelegramDesktop"

          matches:
            - trigger: ":shr"
              replace: "¯\\_(ツ)_/¯"

            - trigger: ":sm"
              replace: "ツ"
        '';
      };
      workstation.input.xkeysnail.rc = ''
        define_keymap(re.compile("TelegramDesktop"), {
            K("C-x"): {
                K("C-c"): K("C-q"),
            },
            K("C-s"): K("Esc"),
            K("C-t"): [K("Shift-Left"), K("C-x"), K("Left"), K("C-v"), K("Right")],
        }, "Telegram")

        define_keymap(re.compile("Slack"), {
            K("C-y"): K("C-v"),
        }, "Slack")
      '';
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.telega ]; # review https://github.com/zevlg/telega.el as it goes
      ide.emacs.core.config = ''
        (use-package telega
          :commands (telega))
      '';
    })
  ];
}
