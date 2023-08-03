{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.completion.tabnine;
  user = config.attributes.mainUser.name;
  toml = pkgs.formats.toml { };
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    completion.tabnine = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable TabNine";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs TabNine setup";
      };
      useNixpkgsVersion = mkOption {
        type = types.bool;
        default = false; # NOTE: nixpkgs version is currently broken (invalid interpreter)
        description = "Use TabNine version from nixpkgs";
      };
      config = mkOption {
        type = types.attrs;
        default = { };
        visible = false;
        internal = true;
        description = "TabNine configuration";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; optionals (!cfg.useNixpkgsVersion) [
          unzip # NOTE: for in-band tabnine binaries installation
        ];
        xdg.configFile."TabNine/TabNine.toml".source = toml.generate "TabNine.toml" cfg.config;
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/match/completion.yml".source = yaml.generate "espanso-completion.yml" {
          matches = [
            {
              trigger = ":tabns";
              replace = "TabNine::sem";
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: optionals (config.ide.emacs.completion.backend == "company") [
        epkgs.company-tabnine
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/tabnine.el ];
    })
  ];
}
