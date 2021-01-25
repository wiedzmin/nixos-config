{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

# TODO: force fonts "beautify"-tion if module is enabled
let
  cfg = config.shell.prompts.starship;
  user = config.attributes.mainUser.name;
in {
  options = {
    shell.prompts.starship = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable starship";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.shell.zsh.enable;
          message = "shell/prompts/starship: enable Zsh first.";
        }
        {
          assertion = (cfg.enable && !config.shell.prompts.ohmyzsh.enable && !config.shell.prompts.liquid.enable);
          message = "shell/prompts/starship: exactly one no theming should be used.";
        }
        {
          assertion = (cfg.enable && !config.appearance.fonts.beautify);
          message = "shell/prompts/starship: beautified (Nerd) fonts required.";
        }
      ];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ starship ];
        # TODO: make evaluation in the very beginning of initExtra
        programs.zsh.initExtra = ''
          eval "$(starship init zsh)"
        '';
        xdg.configFile = {
          "starship.toml".text = toToml {
            add_newline = false;
            character = {
              success_symbol = "[➜](bold green) ";
              error_symbol = "[✗](bold red) ";
            };
            cmd_duration = {
              min_time = 500;
              format = "underwent [$duration](bold yellow)";
            };
            directory = {
              truncation_length = 8;
              truncation_symbol = "…/ ";
            };
            nix_shell.disabled = true;
            # time = {
            #   disabled = false;
            #   format = "🕙[\[ $time \]]($style) ";
            #   time_format = "%T";
            #   utc_time_offset = "local";
            #   time_range = "08:00:00-02:00:00";
            # };
            golang = { # TODO: make module-wise (as well as python and others among available)
              format = "[🏎💨 $version](bold cyan) ";
            };
          };
        };
      };
    })
  ];
}
