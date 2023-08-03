{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.completion.expansions;
  user = config.attributes.mainUser.name;
in
{
  options = {
    completion.expansions = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable expansions";
      };
      expansions.entries = mkOption {
        type = types.listOf types.attrs;
        description = "Various expandable text snippets, mostly for development automation.";
        default = [ ];
      };
      # TODO: consider parameterizing expansions backend some time later
      espanso.toggleKey = mkOption {
        type = types.enum [
          "ALT"
          "CTRL"
          "LEFT_ALT"
          "LEFT_CTRL"
          "LEFT_META"
          "LEFT_SHIFT"
          "META"
          "OFF"
          "RIGHT_ALT"
          "RIGHT_CTRL"
          "RIGHT_META"
          "RIGHT_SHIFT"
          "SHIFT"
        ];
        default = "RIGHT_SHIFT";
      };
      espanso.searchShortcut = mkOption {
        type = types.enum [
          "ALT+SPACE"
        ];
        default = "ALT+SPACE";
      };
      espanso.backend = mkOption {
        type = types.enum [ "Auto" "Clipboard" "Inject" ];
        default = "Auto";
      };
      espanso.config = mkOption {
        type = types.lines;
        default = ''
          toggle_key: ${cfg.espanso.toggleKey}
          search_shortcut: ${cfg.espanso.searchShortcut}
          auto_restart: false
          backend: ${cfg.espanso.backend}
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = "Espanso main config";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      services.espanso.enable = true;
      home-manager.users."${user}" = {
        home.activation = {
          populateEspansoConfig = {
            after = [ ];
            before = [ "linkGeneration" ];
            data = ''mkdir -p /home/alex3rd/.config/espanso/config && echo "${cfg.espanso.config}" > /home/alex3rd/.config/espanso/config/default.yml'';
          };
          restartEspanso = {
            after = [ "linkGeneration" ];
            before = [ ];
            data = "${pkgs.systemd}/bin/systemctl --user restart espanso.service";
          };
        };
      };
    })
  ];
}
