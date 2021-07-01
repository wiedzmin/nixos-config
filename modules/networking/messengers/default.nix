{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.networking.messengers;
  user = config.attributes.mainUser.name;
  stable = import inputs.stable ({
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  });
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
        home.packages = with pkgs; [ stable.tdesktop zoom-us tdlib ];
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
            K("C-s"): [K("Esc"), K("Esc"), K("Esc")],
            K("M-comma"): K("Esc"),
            K("C-Shift-s"): K("C-f"),
            K("C-t"): [K("Shift-Left"), K("C-x"), K("Left"), K("C-v"), K("Right")],
        }, "Telegram")

        define_keymap(re.compile("Slack"), {
            K("C-y"): K("C-v"),
        }, "Slack")
      '';
      wmCommon.autostart.entries = optionals (cfg.telegram.autostart) [ "${pkgs.tdesktop}/bin/telegram-desktop" ];
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
