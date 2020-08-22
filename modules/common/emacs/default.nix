let deps = import ../../../nix/sources.nix;
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.ide.emacs;
  uid = builtins.toString config.users.extraUsers."${config.attributes.mainUser.name}".uid;
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
        default = homePrefix ".emacs.d/data";
        visible = false;
        readOnly = true;
        internal = true;
        description = ''
          Path to store user data under.
        '';
      };
      orgDir = mkOption {
        type = types.str;
        default = homePrefix "docs/org";
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
      package = mkOption {
        type = types.package;
        default = pkgs.emacsGit.override {
          withGTK2 = false;
          withGTK3 = false;
          imagemagick = pkgs.imagemagickBig;
        };
        description = ''
          Emacs derivation to use.
        '';
      };
      extraPackages = mkOption {
        default = self: [ ];
        type = selectorFunction;
        defaultText = "epkgs: []";
        example = literalExample "epkgs: [ epkgs.emms epkgs.magit ]";
        description = ''
          Extra packages available to Emacs. To get a list of
          available packages run:
          <command>nix-env -f '&lt;nixpkgs&gt;' -qaP -A emacsPackages</command>.
        '';
      };
      initElContent = mkOption {
        type = types.lines;
        default = ''
          ;; -*- lexical-binding: t -*-
          (setq debug-on-error t)
          (setq debug-on-quit t)

          (require 'notifications)

          ${lib.optionalString (cfg.environment != { }) (builtins.concatStringsSep "\n"
            (lib.mapAttrsToList (var: value: ''(setenv "${var}" "${value}")'') cfg.environment))}

          ${builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./emacs/base.el; }))}
          ${cfg.config}

          (notifications-notify :title "Emacs" :body "Started server")

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
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      fonts = { fonts = with pkgs; [ emacs-all-the-icons-fonts ]; };
      nixpkgs.config.packageOverrides = _: rec {
        org-capture = mkPythonScriptWithDeps "org-capture" (with pkgs; [ emacs pystdlib tmux xsel ]) (builtins.readFile
          (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/org-capture.py; })));
      };
      custom.dev.git.gitignore = ''
        *.elc
        .dir-locals.el
      '';
      environment.variables.EDITOR = "${pkgs.emacs}/bin/emacsclient";
      ide.emacs.extraPackages = epkgs:
        [
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
          epkgs.company-quickhelp
          epkgs.company-statistics
          epkgs.compdef
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
          epkgs.posframe
          epkgs.pos-tip
          epkgs.quelpa
          epkgs.quelpa-use-package
          epkgs.recentf-ext
          epkgs.recursive-narrow
          epkgs.region-bindings-mode
          epkgs.restart-emacs
          epkgs.savekill
          epkgs.shift-number
          epkgs.smartparens
          epkgs.string-inflection
          epkgs.super-save
          epkgs.undo-propose
          epkgs.unicode-escape
          epkgs.use-package
          epkgs.wgrep
          epkgs.ws-butler
          epkgs.yasnippet
        ] ++ lib.optionals (config.wm.i3.enable) [ epkgs.reverse-im ];
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.zsh.sessionVariables = {
          EDITOR = "${pkgs.emacs}/bin/emacsclient -c -s /run/user/${uid}/emacs/server";
        };
        programs.bash.sessionVariables = {
          EDITOR = "${pkgs.emacs}/bin/emacsclient -c -s /run/user/${uid}/emacs/server";
        };
        home.packages = (with pkgs; [ ispell org-capture editorconfig-checker ])
          ++ [ ((pkgs.emacsPackagesFor cfg.package).emacsWithPackages cfg.extraPackages) ];
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
      systemd.user.services."emacs" = let icon = "${pkgs.emacs}/share/icons/hicolor/scalable/apps/emacs.svg";
      in { # TODO: review/add socket activation
        description = "Emacs: the extensible, self-documenting text editor";
        documentation = [ "info:emacs" "man:emacs(1)" "https://gnu.org/software/emacs/" ];
        restartIfChanged = false;
        serviceConfig = {
          Type = "simple";
          ExecStart = ''${pkgs.runtimeShell} -l -c "exec emacs --fg-daemon"'';
          ExecStop = "${cfg.package}/bin/emacsclient --eval '(kill-emacs 0)'";
          ExecStopPost = "${pkgs.libnotify}/bin/notify-send --icon ${icon} 'Emacs' 'Stopped server'";
          Restart = "on-failure";
          StandardOutput = "journal";
          StandardError = "journal";
        };
        wantedBy = [ "default.target" ];
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "Shift" "e" ];
          cmd = "${pkgs.procps}/bin/pkill -SIGUSR2 emacs";
          mode = "window";
        }
        {
          key = [ "e" ];
          cmd = "${pkgs.emacs}/bin/emacsclient -c -s /run/user/${uid}/emacs/server";
          mode = "window";
        }
      ];
    })
  ];
}
