{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ide.emacs;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  uid = builtins.toString config.users.extraUsers.${user}.uid;
in {
  options = {
    ide.emacs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs setup.";
      };
      debug.enable = mkOption {
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
        default = let
          basepkg = ((pkgs.unstable.emacsGcc.override {
            withGTK2 = false;
            withGTK3 = false;
          }).overrideAttrs (old: rec {
            configureFlags = (remove "--with-cairo" (remove "--with-harfbuzz" old.configureFlags))
              ++ [ "--without-harfbuzz" "--without-cairo" ];
          }));
        in if cfg.debug.enable then (pkgs.enableDebugging basepkg) else basepkg;
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

          ${readSubstituted ../subst.nix ./emacs/base.el}
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
      dev.git.core.gitignore = ''
        *.elc
        .dir-locals.el
      '';
      custom.pim.timeTracking.rules = ''
        current window ($title =~ /^emacs - [^ ]+\.el .*$/) ==> tag coding:elisp,
        current window ($title =~ /^emacs - [^ ]+\.org .*$/) ==> tag edit:orgmode,
      '';
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
          epkgs.compdef
          epkgs.copy-as-format
          epkgs.default-text-scale
          epkgs.deferred
          epkgs.delight
          epkgs.easy-kill
          epkgs.easy-kill-extras # add to .el
          epkgs.f
          epkgs.fancy-dabbrev
          epkgs.flycheck
          epkgs.flycheck-projectile
          epkgs.format-all
          epkgs.gcmh
          epkgs.goto-char-preview
          epkgs.haskell-mode
          epkgs.hl-todo
          epkgs.ini-mode
          epkgs.iqa
          epkgs.keychain-environment
          epkgs.markdown-mode
          epkgs.multiple-cursors
          epkgs.mwim
          epkgs.names
          epkgs.no-littering
          epkgs.pos-tip
          epkgs.posframe
          epkgs.quelpa
          epkgs.quelpa-use-package
          epkgs.rainbow-delimiters
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
          epkgs.whole-line-or-region
          epkgs.ws-butler
        ] ++ lib.optionals (config.wm.i3.enable) [ epkgs.reverse-im ];
      home-manager.users.${user} = {
        programs.zsh.sessionVariables = {
          EDITOR = "${cfg.package}/bin/emacsclient -c -s /run/user/${uid}/emacs/server";
        };
        programs.bash.sessionVariables = {
          EDITOR = "${cfg.package}/bin/emacsclient -c -s /run/user/${uid}/emacs/server";
        };
        home.packages = (with pkgs; [ ispell ])
          ++ [ ((pkgs.unstable.emacsPackagesFor cfg.package).emacsWithPackages cfg.extraPackages) ];
        home.file = {
          ".emacs.d/init.el".text = cfg.initElContent;
          ".emacs.d/resources/yasnippet" = {
            source = inputs.yasnippet-snippets;
            recursive = true;
          };
        };
        home.activation.ensureLspSessionDir = { # lsp-deferred fails otherwise
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${cfg.dataDir}/lsp";
        };
      };
      systemd.user.services."emacs" = let icon = "${cfg.package}/share/icons/hicolor/scalable/apps/emacs.svg";
      in {
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
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "Shift" "e" ];
        cmd = "${pkgs.procps}/bin/pkill -SIGUSR2 emacs";
        mode = "services";
      }];
    })
  ];
}
