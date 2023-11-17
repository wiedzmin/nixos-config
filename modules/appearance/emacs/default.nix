{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.appearance.emacs;
in
{
  imports = [ ./doom-modeline.nix ./doom-themes.nix ./dracula-theme.nix ./telephone-line.nix ./zenburn-theme.nix ];

  options = {
    appearance.emacs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs appearance customization";
      };
      currentLineHighlightFace = mkOption {
        description = "Face for highlighting current line in buffer";
        type = types.str;
        default = "";
      };
      windowDivider.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable windows internal divider";
      };
      windowDivider.width = mkOption {
        description = "Windows internal divider width";
        type = types.int;
        default = 4;
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.ide.emacs.navigation.enable;
        message = "appearance/emacs: ide/emacs/navigation must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.diredfl
        epkgs.default-text-scale
        epkgs.rainbow-mode
        epkgs.transwin
        epkgs.unicode-fonts
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/appearance.el ];
    })
  ];
}
