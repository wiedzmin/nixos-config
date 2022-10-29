{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.appearance.emacs;
  user = config.attributes.mainUser.name;
in
{
  imports = [ ./doom-modeline.nix ./doom-themes.nix ./dracula-theme.nix ./telephone-line.nix ./zenburn-theme.nix ];

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
      home-manager.users."${user}" = lib.optionalAttrs (!config.ide.emacs.core.useModernDrawingLibs) {
        xresources.properties = {
          "Emacs.fontBackend" = "xft,x";
          "Emacs.menuBar" = "0";
          "Emacs.toolBar" = "0";
          "Emacs.verticalScrollBars" = false;
        };
      };

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.diredfl
        epkgs.default-text-scale
        epkgs.lin
        epkgs.rainbow-mode
        epkgs.transwin
        epkgs.unicode-fonts
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ]
        ([ ./elisp/appearance.el ] ++ lib.optionals config.ide.emacs.core.useModernDrawingLibs [ ./elisp/appearance-clean.el ]);
    })
  ];
}
