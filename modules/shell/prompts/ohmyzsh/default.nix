{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.prompts.ohmyzsh;
  user = config.attributes.mainUser.name;
in {
  options = {
    shell.prompts.ohmyzsh = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable oh-my-zsh theming.";
      };
      theme = mkOption {
        type = types.str;
        default = "";
        description = "Oh-my-zsh prompt theme name.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.shell.zsh.enable;
          message = "shell/prompts/ohmyzsh: enable Zsh first.";
        }
        {
          assertion = (cfg.enable && !config.shell.prompts.starship.enable && !config.shell.prompts.liquid.enable);
          message = "shell/prompts/ohmyzsh: exactly one no theming should be used.";
        }
        {
          assertion = (cfg.enable && cfg.theme != "");
          message = "shell/prompts/ohmyzsh: prompt theming enabled but no theme name provided.";
        }
      ];

      home-manager.users."${user}" = {
        programs.zsh = {
          oh-my-zsh.theme = cfg.theme;
          plugins = lib.optionals (!config.shell.prompts.liquid.enable) [{
            name = "zsh-command-time";
            file = "command-time.plugin.zsh";
            src = inputs.zsh-command-time;
          }];
        };
      };
    })
  ];
}
