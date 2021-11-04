{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.prompts.starship;
  user = config.attributes.mainUser.name;
in
{
  options = {
    shell.prompts.starship = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable starship";
      };
      modulesConfiguration = mkOption {
        type = types.attrs;
        default = { };
        description = "Starship additional modules configuration";
      };
      configuration = mkOption {
        type = types.attrs;
        default = {
          add_newline = false;
          scan_timeout = 50;
          command_timeout = 2000;
          character = {
            success_symbol = "[➜](bold green) ";
            error_symbol = "[✗](bold red) ";
          };
          cmd_duration = {
            min_time = 100;
            format = "[$duration](bold yellow)";
            show_notifications = true;
          };
          directory = {
            truncation_length = 8;
          };
          line_break = {
            disabled = true;
          };
          nix_shell.disabled = true;
        } // optionalAttrs (cfg.modulesConfiguration != { }) cfg.modulesConfiguration;
        description = "Starship prompt configuration";
        visible = false;
        internal = true;
        readOnly = true;
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
          assertion = cfg.enable && !config.shell.prompts.ohmyzsh.enable && !config.shell.prompts.liquid.enable;
          message = "shell/prompts/starship: exactly one no theming should be used.";
        }
        {
          assertion = cfg.enable && config.appearance.fonts.beautify;
          message = "shell/prompts/starship: beautified (Nerd) fonts required.";
        }
      ];

      appearance.fonts.beautify = true;

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ starship ];
        # TODO: make evaluation in the very beginning of initExtra
        programs.zsh.initExtra = ''
          eval "$(starship init zsh)"
        '';
        xdg.configFile = {
          "starship.toml".text = toToml cfg.configuration;
        };
      };
    })
  ];
}
