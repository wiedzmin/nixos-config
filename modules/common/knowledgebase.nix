{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.knowledgebase;
  rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
    ${pkgs.feh}/bin/feh --bg-fill ${cfg.wallpaper.root}/${cfg.wallpaper.current}
  '';
  emacsHelpSetup = ''
    (use-package apropos
      :bind
      (:map mode-specific-map
            :prefix-map custom-help-map
            :prefix "H"
            ("a" . apropos)
            ("d" . apropos-documentation)
            ("v" . apropos-variable)
            ("c" . apropos-command)
            ("l" . apropos-library)
            ("u" . apropos-user-option)
            ("i" . info-apropos)
            ("t" . tags-apropos)
            ("e" . apropos-value)))

    (use-package helpful
      :ensure t
      :defer t
      :bind
      (:prefix-map custom-help-map
                   :prefix "<f1>"
                   ("f" . helpful-function)
                   ("v" . helpful-variable)
                   ("C" . helpful-callable)
                   ("m" . helpful-macro)
                   ("c" . helpful-command)
                   ("k" . helpful-key)
                   ("RET" . helpful-at-point))
      (:map help-map
            ("f" . helpful-function)
            ("v" . helpful-variable)
            ("C" . helpful-callable)
            ("m" . helpful-macro)
            ("c" . helpful-command)
            ("k" . helpful-key)
            ("RET" . helpful-at-point)))

    (use-package which-key
      :ensure t
      :delight which-key-mode
      :config
      (which-key-mode))

    (use-package which-key-posframe
      :ensure t
      :after which-key
      :hook
      (after-init-hook . which-key-posframe-mode)
      :custom
      (which-key-posframe-poshandler 'posframe-poshandler-frame-center))
  '';
in {
  options = {
    knowledgebase = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable knowledge base facilities.";
      };
      man.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable man pages.";
      };
      info.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable TeXInfo pages.";
      };
      doc.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable documentation.";
      };
      dev.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable dev documentation.";
      };
      nixos.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable NixOS documentation.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs documentation setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      documentation = {
        enable = true;
        man.enable = cfg.man.enable;
        info.enable = cfg.info.enable;
        doc.enable = cfg.doc.enable;
        dev.enable = cfg.dev.enable;
        nixos = {
          enable = cfg.nixos.enable;
          includeAllModules = false; # FIXME build error
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [
          epkgs.helpful
          epkgs.which-key
          epkgs.which-key-posframe
        ];
      };
      ide.emacs.config = ''${emacsHelpSetup}'';
    })
  ];
}
