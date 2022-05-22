{ config, lib, ... }:
with lib;

let cfg = config.appearance.emacs.modeline.doom;
in
{
  options = {
    appearance.emacs.modeline.doom = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable doom-modeline.";
      };
      height = mkOption {
        type = types.int;
        default = 25;
        description = "Modeline height in pixels.";
      };
      displayMinorModes = mkOption {
        type = types.bool;
        default = false;
        description = "Whether display the minor modes in the mode-line";
      };
      notificationsCountMax = mkOption {
        type = types.int;
        default = 99;
        description = "The maximum number displayed for notifications";
      };
      vcBranchNameLengthMax = mkOption {
        type = types.int;
        default = 12;
        description = "The maximum displayed length of the branch name of version control";
      };
      bufferFileNameStyle = mkOption {
        type = types.enum [
          "auto"
          "truncate-upto-project"
          "truncate-from-project"
          "truncate-with-project"
          "truncate-except-project"
          "truncate-upto-root"
          "truncate-all"
          "truncate-nil"
          "relative-from-project"
          "relative-to-project"
          "file-name"
          "buffer-name"
        ];
        default = "auto";
        description = ''
          Determines the style used by `doom-modeline-buffer-file-name'.

          Given ~/Projects/FOSS/emacs/lisp/comint.el
            auto => emacs/lisp/comint.el (in a project) or comint.el
            truncate-upto-project => ~/P/F/emacs/lisp/comint.el
            truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
            truncate-with-project => emacs/l/comint.el
            truncate-except-project => ~/P/F/emacs/l/comint.el
            truncate-upto-root => ~/P/F/e/lisp/comint.el
            truncate-all => ~/P/F/e/l/comint.el
            truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
            relative-from-project => emacs/lisp/comint.el
            relative-to-project => lisp/comint.el
            file-name => comint.el
            buffer-name => comint.el<2> (uniquify buffer name)
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && config.wm.i3.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.doom-modeline ];
      ide.emacs.core.config = ''
        (use-package all-the-icons) ; TODO: consider lifting up to Nix level and make SPOT

        ;; TODO: implement Nix harness and respect relative packages setup (project.el/projectile/ffip)
        ;;       for 'doom-modeline-project-detection'. Also make choiced parameter for those packages.

        (use-package doom-modeline
          :hook
          (after-init-hook . doom-modeline-init)
          :custom
          (doom-modeline-height ${builtins.toString cfg.height})
          (doom-modeline-icon t)
          (doom-modeline-project-detection 'project)
          (doom-modeline-major-mode-icon t)
          (doom-modeline-major-mode-color-icon t)
          (doom-modeline-buffer-state-icon t)
          (doom-modeline-buffer-modification-icon t)
          (doom-modeline-buffer-name t)
          (doom-modeline-buffer-file-name-style '${cfg.bufferFileNameStyle})
          (doom-modeline-minor-modes ${if cfg.displayMinorModes then "t" else "nil"})
          (doom-modeline-buffer-encoding t)
          (doom-modeline-indent-info t)
          (doom-modeline-checker-simple-format nil)
          (doom-modeline-number-limit ${builtins.toString cfg.notificationsCountMax})
          (doom-modeline-vcs-max-length ${builtins.toString cfg.vcBranchNameLengthMax})
          (doom-modeline-lsp t)
          (doom-modeline-env-version t)
          :config
          (display-time-mode -1)
          (when (display-graphic-p) (setq doom-modeline-hud t)))
      '';
    })
  ];
}
