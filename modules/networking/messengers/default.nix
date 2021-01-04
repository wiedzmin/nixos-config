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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      services.quassel.enable = true;
      home-manager.users."${user}" = { home.packages = with pkgs; [ skype tdesktop quasselClient ]; };
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
  ];
}
