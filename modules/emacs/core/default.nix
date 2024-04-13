{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.ide.emacs.core;
  user = config.attributes.mainUser.name;
  emacsWithPkgs = (pkgs.unstable.emacsPackagesFor cfg.package).emacsWithPackages cfg.extraPackages;
  emacsInitDir = homePrefix user cfg.initDir;
  drop-corrupted = pkgs.writeShellApplication {
    name = "drop-corrupted";
    text = ''
      ${pkgs.systemd}/bin/systemctl --user stop emacs.service
      sleep 2
      rm -f ${cfg.dataDir}/{save-kill.el,savehist.el}
      rm -f ${cfg.varDir}/{save-kill.el,savehist.el}
      ${pkgs.systemd}/bin/systemctl --user restart emacs.service
    '';
  };
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  genCustomPackages = customPackages:
    ''
      ${builtins.concatStringsSep "\n"
        (lib.mapAttrsToList (feature: impl:
          let
            def = pkgs.writeTextFile {
              name = "${feature}";
              text = ''
                ${impl}
                (provide '${feature})
              '';
              destination = "/${feature}.el";
            };
          in
            ''
              (add-to-list 'load-path "${def}")
            ''
        ) customPackages)}
    '';
  debugTracePatch = ''
    (let ((context (lambda () (format "\nload-file-name: %s" load-file-name))))
      (trace-function #'load nil context)
      (trace-function #'require nil context))
  '';
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    ide.emacs.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs core setup.";
      };
      # NOTE: there is only "/etc" to consider outside of $HOME, but Emacs is user-centric after all, hence the base dir
      initDir = mkOption {
        type = types.str;
        default = ".emacs.d";
        description = "Where to look for the Emacs init files, under $HOME";
      };
      serverSocket = mkOption {
        type = types.str;
        default = "/run/user/${config.attributes.mainUser.ID}/emacs/server";
        description = "Server socket path";
      };
      debug.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to build debuggable emacs package.";
      };
      debug.trace = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to trace configuration loading";
      };
      daemon.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to daemonize";
      };
      daemon.debug = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable pre-start debug output for systemd service";
      };
      daemon.debugCmd = mkOption {
        type = types.str;
        default = "";
        description = "Command used to produce debug output for systemd service";
      };
      daemon.ttyCmd = mkOption {
        type = types.str;
        default = "emacsclient -tc -s ${cfg.serverSocket}";
        description = "Command used for terminal emacs' flavor";
      };
      fromGit = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to build emacs Git version";
      };
      pgtk.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to build PGTK emacs flavor";
      };
      treesitter.enable = mkOption {
        # FIXME: consider relocating treesitter functionality to `emacs/edit` for example
        type = types.bool;
        default = true;
        description = "Whether to include tree-sitter support";
      };
      treesitter.grammars = mkOption {
        type = types.attrsOf (types.either (types.listOf types.str) types.str);
        default = { };
        example = {
          c = "https://github.com/tree-sitter/tree-sitter-c";
          python = "https://github.com/tree-sitter/tree-sitter-python";
          tsx = [ "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" ];
          typescript = [ "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" ];
        };
        description = ''
          Configuration for downloading and installing tree-sitter language grammars.
          Refer to `treesit-language-source-alist` emacs variable for details.
        '';
      };
      treesitter.modeRemappings = mkOption {
        type = types.attrsOf types.str;
        default = { };
        example = {
          c-mode = "c-ts-mode";
          clojure-mode = "clojure-ts-mode";
        };
        description = ''
          Alist mapping file-specified mode to actual mode.
          Refer to `major-mode-remap-alist` emacs variable for details.
        '';
      };
      treesitter.fontLockLevel = mkOption {
        type = types.int;
        default = 4;
        description = ''
          Features are grouped into decoration levels, right now there
          are 4 levels and the default level is 3. If you want to program
          in skittles, set treesit-font-lock-level to 4 ;-)
        '';
      };
      customPackages = mkOption {
        type = types.attrs;
        default = { };
        description = "Custom elisp definitions in form `feature name` --> `implementation`";
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
        default = "${emacsInitDir}/data";
        visible = false;
        readOnly = true;
        internal = true;
        description = ''
          Path to store user data under.
        '';
      };
      varDir = mkOption {
        type = types.str;
        default = "${emacsInitDir}/var";
        visible = false;
        readOnly = true;
        internal = true;
        description = ''
          Path to store various data under.
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
            flavor = with pkgs.unstable; if cfg.pgtk.enable then emacsPgtk else
            if cfg.fromGit then emacs-git else emacs;
            configured = (flavor.override {
              withGTK2 = false;
              withGTK3 = if cfg.pgtk.enable then true else false;
            }).overrideAttrs (_: {
              withCsrc = true;
            });
          in
          if cfg.debug.enable then (pkgs.enableDebugging configured) else configured;
        description = ''
          Emacs derivation to use.
        '';
      };
      extraPackages = mkOption {
        default = _: [ ];
        type = selectorFunction;
        defaultText = "epkgs: []";
        example = literalExpression "epkgs: [ epkgs.emms epkgs.magit ]";
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

          (when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
            (setq comp-async-query-on-exit t)
            (setq comp-async-jobs-number 4)
            (setq comp-async-report-warnings-errors nil)
            (setq native-comp-async-report-warnings-errors 'silent)
            (setq comp-deferred-compilation t))

          (require 'notifications)

          ${lib.optionalString (cfg.debug.trace) debugTracePatch}

          ${readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/core.el ]}

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
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Emacs-related bookmarks";
      };
      emacsEverywhere.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable system-wide popup Emacs windows for quick edits (package `emacs-everywhere`)";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = cfg.emacsEverywhere.enable && cfg.daemon.enable || !cfg.emacsEverywhere.enable && !cfg.daemon.enable;
        message = "emacs/core: daemon should be enabled for `emacsEverywhere` to work";
      }];

      dev.git.core.gitignore = ''
        *.elc
      '';
      pim.timetracking.rules = mkArbttTitleRule [ "^emacs - [^ ]+\\.el .*$" ] "coding:elisp";
      ide.emacs.core.extraPackages = epkgs:
        [
          epkgs.anaphora
          epkgs.deferred
          epkgs.delight
          epkgs.f
          epkgs.no-littering
          epkgs.reverse-im
        ] ++ lib.optionals cfg.treesitter.enable [ epkgs.treesit-auto ]
        ++ lib.optionals cfg.emacsEverywhere.enable [ epkgs.emacs-everywhere ];
      ide.emacs.core.config = lib.optionalString config.wm.i3.enable ''
        (use-package reverse-im
          :custom
          (reverse-im-input-methods '("russian-computer"))
          :config
          (reverse-im-mode t))
      '' + lib.optionalString cfg.treesitter.enable
        (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/treesit.el ])
      + lib.optionalString cfg.emacsEverywhere.enable ''
        (use-package emacs-everywhere)
      '';
      ide.emacs.core.customPackages = lib.optionalAttrs cfg.treesitter.enable {
        "treesit-util" = builtins.readFile ./elisp/custom/treesit-util.el;
      };
      ide.emacs.core.customKeymaps = {
        "custom-goto-map" = "M-s";
      };
      shell.core.variables = lib.optionals cfg.daemon.enable [{
        EDITOR = "${emacsWithPkgs}/bin/emacsclient -c -s ${cfg.serverSocket}";
        VISUAL = "${emacsWithPkgs}/bin/emacsclient -c -s ${cfg.serverSocket}";
      }];
      ide.emacs.completion.tempel.snippets = ''
        fundamental-mode

        (todo "TODO: ")
        (note "NOTE: ")
        (fixme "FIXME: ")
        (todop "TODO(${user}): ")
        (notep "NOTE(${user}): ")
        (fixmep "FIXME(${user}): ")
      '';
      home-manager.users."${user}" = {
        home.packages = (with pkgs; [ drop-corrupted ispell nurpkgs.my_cookies ])
          ++ [ emacsWithPkgs ]
          ++ optionals (cfg.emacsEverywhere.enable) (with pkgs; [
          xclip
          xdotool
          xorg.xprop
          xorg.xwininfo
        ]);
        home.file = {
          "${cfg.initDir}/early-init.el".text = ''
            ${lib.optionalString (cfg.daemon.enable) ''
              (setenv "EDITOR" "${emacsWithPkgs}/bin/emacsclient -c -s ${cfg.serverSocket}")
            ''}
            ;; FIXME: elaborate alternative command for the case of disabled daemon
            ${lib.optionalString (cfg.environment != { }) (builtins.concatStringsSep "\n"
              (lib.mapAttrsToList (var: value: ''(setenv "${var}" "${value}")'') cfg.environment))}

            ${genEmacsCustomKeymaps cfg.customKeymaps}
            ${genCustomPackages cfg.customPackages}
          '';
          "${cfg.initDir}/init.el".text = cfg.initElContent;
        };
        home.activation = {
          ensureEmacsConfigValid = {
            after = [ "linkGeneration" ];
            before = [ ];
            data = ''
              ${emacsWithPkgs}/bin/emacs --batch -l ${emacsInitDir}/early-init.el -l ${emacsInitDir}/init.el
            '';
          };
          ensureEmacsConfigPath = {
            after = [ ];
            before = [ "linkGeneration" ];
            data = ''
              mkdir -p ${emacsInitDir}
            '';
          };
        };
      };
      systemd.user.services."emacs" = optionalAttrs cfg.daemon.enable
        (
          let icon = "${emacsWithPkgs}/share/icons/hicolor/scalable/apps/emacs.svg";
          in
          {
            description = "Emacs: the extensible, self-documenting text editor";
            documentation = [ "info:emacs" "man:emacs(1)" "https://gnu.org/software/emacs/" ];
            restartIfChanged = false;
            serviceConfig = {
              Type = "simple";
              ExecStart = ''${pkgs.runtimeShell} -l -c "exec ${emacsWithPkgs}/bin/emacs --init-directory ${emacsInitDir} --fg-daemon"'';
              ExecStop = "${emacsWithPkgs}/bin/emacsclient --eval '(kill-emacs 0)'";
              ExecStopPost = "${pkgs.libnotify}/bin/notify-send --icon ${icon} 'Emacs' 'Stopped server'";
              Restart = "on-failure";
              StandardOutput = "journal";
              StandardError = "journal";
            } // optionalAttrs (cfg.debug.enable && cfg.daemon.debugCmd != "") {
              ExecStartPre = ''${pkgs.stdenv.shell} -l -c "${cfg.daemon.debugCmd} | tee /tmp/emacs-service-$(date +%Y-%m-%d-%H-%M-%S | tr -d '[:cntrl:]').debug"'';
            };
          }
        );
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = [
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
      ] ++ optionals (cfg.emacsEverywhere.enable) [
        {
          key = [ "e" ];
          cmd = "${emacsWithPkgs}/bin/emacsclient --eval \"(emacs-everywhere)\"";
          mode = "run";
        }
      ];
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        emacs-overlay = {
          desc = "nix emacs overlay";
          local.path = "${wsRoot roots "github"}/nix-community/emacs-overlay";
          remote = {
            url = "https://github.com/nix-community/emacs-overlay/";
            jump = true;
            searchSuffix = "search?q=";
          };
        };
        melpa = {
          desc = "MELPA";
          remote = {
            url = "https://melpa.org/#/";
            jump = true;
            searchSuffix = "?q=";
          };
        };
        elpa-gnu = {
          desc = "GNU ELPA";
          remote.url = "https://elpa.gnu.org/packages/";
        };
        elpa-nongnu = {
          desc = "NonGNU ELPA";
          remote.url = "https://elpa.nongnu.org/nongnu/";
        };
        use-package = { remote.url = "https://github.com/jwiegley/use-package"; };
        "emacs-news" = {
          desc = "Emacs news";
          remote.url = "https://sachachua.com/blog/category/emacs-news/";
          windowRules = [
            {
              class = mkWSMappingBrowsersRegexp config.attributes.browser;
              title = "sachachua emacs news";
              desktop = "main"; # [ref:desktop_main]
            }
          ];
        };
        "libhunt/elisp" = {
          tags = [ "search" "libraries" "emacs" "elisp" ];
          remote.url = "https://www.libhunt.com/l/emacs-lisp";
        };
        "gooem" = {
          desc = "emacs + ";
          remote = {
            url = "https://www.google.ru/";
            searchSuffix = "?q=emacs+";
          };
        };
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/match/emacs.yml".source = yaml.generate "espanso-emacs.yml" {
          filter_class = "Emacs";
          matches = [
            {
              trigger = ":bore";
              replace = "(setq backtrace-on-redisplay-error $|$)";
            }
            {
              trigger = ":el";
              replace = "\\.el";
            }
          ];
        };
      };
    })
  ];
}
