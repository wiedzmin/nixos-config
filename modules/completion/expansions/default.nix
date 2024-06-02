{ config, lib, pkgs, ... }:
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
      # TODO: consider setting up emacs abbrev-mode as independent option for some subset of collected expansion
      expansions.entries = mkOption {
        type = types.listOf types.attrs;
        description = "Various expandable text snippets, mostly for development automation.";
        default = [ ];
      };
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
      espanso.config.default = mkOption {
        type = types.attrs;
        default = {
          toggle_key = cfg.espanso.toggleKey;
          search_shortcut = cfg.espanso.searchShortcut;
          auto_restart = false;
          backend = cfg.espanso.backend;
          x11_use_xclip_backend = true;
        };
        visible = false;
        readOnly = true;
        internal = true;
        description = "Espanso main config";
      };
      espanso.configs = mkOption {
        type = types.attrs;
        default = { };
        description = "Espanso per-app configs collection";
      };
      espanso.matches = mkOption {
        type = types.attrs;
        default = { };
        description = "Espanso per-app matches collection";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        services.espanso = {
          enable = true;
          configs = {
            default = cfg.espanso.config.default;
          } // cfg.espanso.configs;
          matches = cfg.espanso.matches;
        };
        home.activation = {
          ensureEspansoConfigPath = {
            after = [ ];
            before = [ "linkGeneration" ];
            data = ''mkdir -p /home/alex3rd/.config/espanso/config'';
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
