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
      iconSet = mkOption {
        type = types.enum [ "none" "all-the-icons" "nerd-icons" ];
        default = "none";
        description = "Prettifying iconset to use";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.ide.emacs.navigation.enable;
        message = "appearance/emacs: ide/emacs/navigation must be enabled.";
      }];

      fonts.packages = with pkgs; [ emacs-all-the-icons-fonts ];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.all-the-icons
        epkgs.nerd-icons
        epkgs.default-text-scale
        epkgs.diredfl
        epkgs.rainbow-mode
        epkgs.transwin
        epkgs.unicode-fonts
      ] ++ lib.optionals (cfg.iconSet == "all-the-icons") [
        epkgs.all-the-icons
      ] ++ lib.optionals (cfg.iconSet == "nerd-icons") [
        epkgs.nerd-icons
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/appearance.el ] +
        optionalString (cfg.iconSet == "all-the-icons") (builtins.readFile ./elisp/all-the-icons.el) +
        optionalString (cfg.iconSet == "nerd-icons") (builtins.readFile ./elisp/nerd-icons.el);
    })
  ];
}
