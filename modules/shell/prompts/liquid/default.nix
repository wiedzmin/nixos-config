{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.prompts.liquid;
  user = config.attributes.mainUser.name;
in {
  options = {
    shell.prompts.liquid = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable liquidprompt.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.shell.zsh.enable;
          message = "shell/prompts/liquid: enable Zsh first.";
        }
        {
          assertion = (cfg.enable && !config.shell.prompts.ohmyzsh.enable && !config.shell.prompts.starship.enable);
          message = "shell/prompts/liquid: exactly one no theming should be used.";
        }
      ];

      home-manager.users."${user}" = {
        home.file = { # TODO: extract options
          ".lp.ps1".text = builtins.readFile ./assets/.lp.ps1;
          ".liquidpromptrc".text = builtins.readFile ./assets/.liquidpromptrc;
        };
        programs.zsh = {
          plugins = [{
            name = "liquidprompt";
            file = "liquidprompt.plugin.zsh";
            src = inputs.liquidprompt;
          }];
        };
      };
    })
  ];
}
