{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ide.emacs.core;
  user = config.attributes.mainUser.name;
  drop-corrupted = mkShellScriptWithDeps "drop-corrupted" (with pkgs; [ gitAndTools.git coreutils ]) ''
    ${pkgs.systemd}/bin/systemctl --user stop emacs.service
    sleep 2
    rm -f /home/${user}/.emacs.d/data/{save-kill.el,savehist.el}
    rm -f /home/${user}/.emacs.d/var/{save-kill.el,savehist.el}
    ${pkgs.systemd}/bin/systemctl --user restart emacs.service
  '';
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    ide.emacs.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs core setup.";
      };
      debug.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to build debuggable emacs package.";
      };
      native.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to build emacs with nativecomp features enabled";
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
        default =
          let
            flavor = (with pkgs.unstable; if cfg.native.enable then emacsGcc else emacs);
            configured = ((flavor.override {
              withGTK2 = false;
              withGTK3 = false;
            }).overrideAttrs (old: rec {
              withCsrc = true;
              configureFlags = (remove "--with-cairo" (remove "--with-harfbuzz" old.configureFlags))
                ++ [ "--without-harfbuzz" "--without-cairo" ];
            }));
          in
          if cfg.debug.enable then (pkgs.enableDebugging configured) else configured;
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
      customKeymaps = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "Custom config-wide keymaps definitions";
      };
      initElContent = mkOption {
        type = types.lines;
        default = ''
          ;; -*- lexical-binding: t -*-
          (setq debug-on-error t)
          (setq debug-on-quit t)

          (when (fboundp 'native-comp-available-p)
            (setq comp-async-query-on-exit t)
            (setq comp-async-jobs-number 4)
            (setq comp-async-report-warnings-errors nil)
            (setq comp-deferred-compilation t))

          (require 'notifications)

          ${lib.concatStringsSep "\n" (lib.forEach [ ./core.el ] (el: readSubstituted ../../subst.nix el))}

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
      '';
      pim.timetracking.rules = mkArbttTitleRule [ "^emacs - [^ ]+\\.el .*$" ] "coding:elisp";
      ide.emacs.core.extraPackages = epkgs:
        [
          epkgs.anaphora
          epkgs.auto-compile
          epkgs.compdef
          epkgs.deferred
          epkgs.delight
          epkgs.f
          epkgs.no-littering
          epkgs.use-package
        ] ++ lib.optionals (config.wm.i3.enable) [ epkgs.reverse-im ];
      ide.emacs.core.config = lib.optionalString (config.wm.i3.enable) ''
        (use-package reverse-im
          :ensure t
          :custom
          (reverse-im-input-methods '("russian-computer"))
          :config
          (reverse-im-mode t))
      '';
      shell.core.variables = [{
        EDITOR = "${cfg.package}/bin/emacsclient -c -s /run/user/${config.attributes.mainUser.ID}/emacs/server";
        VISUAL = "${cfg.package}/bin/emacsclient -c -s /run/user/${config.attributes.mainUser.ID}/emacs/server";
      }];
      home-manager.users.${user} = {
        home.packages = (with pkgs; [ drop-corrupted ispell nurpkgs.my_cookies ])
          ++ [ ((pkgs.unstable.emacsPackagesFor cfg.package).emacsWithPackages cfg.extraPackages) ];
        home.file = {
          ".emacs.d/early-init.el".text = ''
            ${lib.optionalString (cfg.environment != { }) (builtins.concatStringsSep "\n"
              (lib.mapAttrsToList (var: value: ''(setenv "${var}" "${value}")'') cfg.environment))}

            ${genEmacsCustomKeymaps cfg.customKeymaps}
          '';
          ".emacs.d/init.el".text = cfg.initElContent;
        };
      };
      systemd.user.services."emacs" =
        let icon = "${cfg.package}/share/icons/hicolor/scalable/apps/emacs.svg";
        in
        {
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
      wmCommon.keys = [
        {
          key = [ "Shift" "e" ];
          cmd = "${pkgs.procps}/bin/pkill -SIGUSR2 emacs";
          mode = "services";
        }
        {
          key = [ "e" ];
          cmd = ''[class="^Emacs$"] scratchpad show'';
          mode = "scratchpad";
          raw = true;
        }
      ];
    })
  ];
}
