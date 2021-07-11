{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.appearance.emacs;
  user = config.attributes.mainUser.name;
in {
  imports = [ ./doom-modeline.nix ./dracula-theme.nix ./telephone-line.nix ./zenburn-theme.nix ];

  options = {
    appearance.emacs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs appearance customization.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        xresources.properties = {
          "Emacs.fontBackend" = "xft,x";
          "Emacs.menuBar" = "0";
          "Emacs.toolBar" = "0";
          "Emacs.verticalScrollBars" = false;
        };
      };

      ide.emacs.core.extraPackages = epkgs: [ epkgs.diredfl epkgs.rainbow-mode epkgs.transwin epkgs.unicode-fonts ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./appearance.el;
    })
  ];
}
