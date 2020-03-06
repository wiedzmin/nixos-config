let deps = import ../../../nix/sources.nix;
in { config, lib, pkgs, ... }:
with lib;

let cfg = config.ide.emacs;
in {
  options = {
    ide.emacs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs setup.";
      };
      config = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Content to be placed to init.el file.
        '';
      };
      dataDir = mkOption {
        type = types.str;
        default = "/home/${config.attributes.mainUser.name}/.emacs.d/data";
        visible = false;
        readOnly = true;
        internal = true;
        description = ''
          Path to store user data under.
        '';
      };
      orgDir = mkOption {
        type = types.str;
        default = "/home/${config.attributes.mainUser.name}/docs/org";
        description = ''
          Path to store org-mode docs under.
        '';
      };
      environment = mkOption {
        type = types.attrs;
        default = { };
        description = ''
          Environment variabled to be passed to Emacs.

          These variables should be set explicitly, using #'setenv, because
          emacs startup methods may vary between systems and packages like
          "exec-path-from-shell" will not always help.

          For example, in tiling WMs like xmonad/stumpwm, emacs is being started
          from simple dumb sh executable, with no env defined for interactive
          sessions.
        '';
      };
      initElContent = mkOption {
        type = types.lines;
        default = ''
          ;; -*- lexical-binding: t -*-
          (setq debug-on-error t)
          (setq debug-on-quit t)

          ${lib.optionalString (cfg.environment != { })
              (builtins.concatStringsSep "\n"
              (lib.mapAttrsToList (var: value: ''(setenv "${var}" "${value}")'') cfg.environment))}

          ${builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./base.el; }))}
          ${cfg.config}
          (setq debug-on-error nil)
          (setq debug-on-quit nil)
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = ''
          Aggregated content of init.el file.
        '';
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      fonts = { fonts = with pkgs; [ emacs-all-the-icons-fonts ]; };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          (makeDesktopItem {
            name = "org-protocol";
            exec = "${emacs}/bin/emacsclient %U";
            comment = "";
            desktopName = "Custom org-protocol handler";
            categories = "System";
            mimeType = stdenv.lib.concatStringsSep ";" [ "x-scheme-handler/org-protocol" ];
          })

          ispell
        ];
        programs.zsh.sessionVariables = { EDITOR = "${pkgs.emacs}/bin/emacsclient"; };
        programs.bash.sessionVariables = { EDITOR = "${pkgs.emacs}/bin/emacsclient"; };
        programs.emacs = {
          enable = true;
          package = (pkgs.emacs26.override {
            # build Lucid version
            withGTK2 = false;
            withGTK3 = false;
            inherit (pkgs) imagemagick;
          });
          # TODO: scan *.el and find packages not in list below
          extraPackages = epkgs: [
            epkgs.aggressive-indent
            epkgs.amx
            epkgs.anaphora
            epkgs.auto-compile
            epkgs.backup-each-save
            epkgs.beginend
            epkgs.blockdiag-mode
            epkgs.comment-dwim-2
            epkgs.company
            epkgs.company-fuzzy
            epkgs.company-lsp
            epkgs.company-quickhelp
            epkgs.company-statistics
            epkgs.copy-as-format
            epkgs.default-text-scale
            epkgs.deferred
            epkgs.delight
            epkgs.easy-kill
            epkgs.easy-kill-extras # add to .el
            epkgs.editorconfig
            epkgs.f
            epkgs.fancy-dabbrev
            epkgs.flycheck
            epkgs.format-all
            epkgs.gcmh
            epkgs.goto-char-preview
            epkgs.haskell-mode
            epkgs.hl-todo
            epkgs.ini-mode
            epkgs.iqa
            epkgs.keychain-environment
            epkgs.lsp-mode
            epkgs.lsp-ui
            epkgs.markdown-mode
            epkgs.multiple-cursors
            epkgs.mwim
            epkgs.names
            epkgs.no-littering
            epkgs.pinentry
            epkgs.posframe
            epkgs.quelpa
            epkgs.quelpa-use-package
            epkgs.recentf-ext
            epkgs.recursive-narrow
            epkgs.region-bindings-mode
            epkgs.restart-emacs
            epkgs.savekill
            epkgs.shift-number
            epkgs.smartparens
            epkgs.super-save
            epkgs.undo-propose
            epkgs.unicode-escape
            epkgs.wgrep
            epkgs.ws-butler
            epkgs.yasnippet
          ];
        };
        home.file = {
          ".emacs.d/init.el".text = cfg.initElContent;
          ".emacs.d/resources/yasnippet" = {
            source = deps.yasnippet-snippets;
            recursive = true;
          };
        };
        home.activation.ensureLspSessionDir = { # lsp-deferred fails otherwise
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${cfg.dataDir}/lsp";
        };
      };
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = { "M-w S-e" = ''spawn "${pkgs.procps}/bin/pkill -SIGUSR2 emacs"''; };
    })
  ];
}
