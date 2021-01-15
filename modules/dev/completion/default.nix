{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.completion;
  user = config.attributes.mainUser.name;
in {
  options = {
    dev.completion = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable dev completion setup.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs completion setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ tabnine ]; # FIXME: install it to be consumable by company-tabnine
        xdg.configFile."TabNine/TabNine.toml".text =
          toToml { language.python = { command = "python-language-server"; }; };
      };
    })

    (mkIf cfg.emacs.enable {
      ide.emacs.core.extraPackages = epkgs:
        [
          epkgs.company
          epkgs.company-box
          epkgs.company-fuzzy
          epkgs.company-quickhelp
          epkgs.company-restclient
          epkgs.company-statistics
          epkgs.company-tabnine
          epkgs.yasnippet
        ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/completion.el;
    })
  ];
}
