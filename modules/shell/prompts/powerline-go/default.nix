{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.prompts.powerline-go;
  user = config.attributes.mainUser.name;
in
{
  options = {
    shell.prompts.powerline-go = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable `powerline-go`";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.shell.zsh.enable;
          message = "shell/prompts/powerline-go: enable Zsh first.";
        }
        {
          assertion = cfg.enable && !config.shell.prompts.ohmyzsh.enable && !config.shell.prompts.liquid.enable && !config.shell.prompts.starship.enable;
          message = "shell/prompts/powerline-go: exactly one or no theming should be used.";
        }
      ];

      home-manager.users."${user}" = {
        programs.powerline-go = {
          enable = true;
          modules = [
            "cwd"
            "docker"
            "docker_context"
            "exit"
            "git"
            "hg"
            "host"
            "load"
            "nix-shell"
            "perms"
            "root"
            "ssh"
            "user"
            # "duration"
          ];
          settings = {
            hostname-only-if-ssh = true;
            numeric-exit-codes = true;
            cwd-max-depth = 7;
            cwd-mode = "fancy";
            duration-min = 5000;
          };
        };
      };
    })
  ];
}
