{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ide.emacs.misc;
in
{
  options = {
    ide.emacs.misc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs misc extensions.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.ide.emacs.core.enable;
          message = "emacs/misc: core configuration must be enabled.";
        }
        {
          assertion = config.ide.emacs.edit.enable;
          message = "emacs/misc: emacs/edit must be enabled.";
        }
      ];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.flycheck
        epkgs.hl-todo
        epkgs.copy-as-format
        epkgs.format-all
        epkgs.ini-mode
        epkgs.markdown-mode
      ];
      ide.emacs.core.customKeymaps = {
        "custom-formatting-map" = "C-c f";
      };
      ide.emacs.core.config = builtins.readFile ./elisp/misc.el;
    })
  ];
}
