{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.dev;
  emacsDevSetup = ''
    (use-package webpaste
      :ensure t
      :bind
      (:prefix-map custom-webpaste-map
                   :prefix "M-p"
                   ("b" . webpaste-paste-buffer)
                   ("r" . webpaste-paste-region))
      :custom
      (webpaste-provider-priority '("ix.io" "gist.github.com")))

    (use-package jinja2-mode
      :ensure t
      :mode "\\.j2$")

    (use-package yaml-mode
      :mode "\\.yml\\'"
      :quelpa
      (yaml-mode :repo "yoshiki/yaml-mode" :fetcher github :version original))

    (use-package diff-hl
      :ensure t
      :hook
      (dired-mode-hook . diff-hl-dired-mode)
      (magit-post-refresh-hook . diff-hl-magit-post-refresh)
      (org-mode-hook . diff-hl-mode)
      (prog-mode . diff-hl-mode)
      :config
      (diff-hl-margin-mode 1)
      (diff-hl-amend-mode 1)
      (diff-hl-flydiff-mode 1)
      (global-diff-hl-mode 1))

    (use-package diff-mode
      :mode "diff")

    (use-package fic-mode
      :ensure t
      :hook
      (prog-mode . fic-mode))

    (use-package multi-compile :ensure t)

    (use-package company-restclient
      :ensure t
      :after company restclient
      :config
      (add-to-list 'company-backends 'company-restclient))

    (use-package company-lsp
      :ensure t
      :after lsp-ui
      :custom
      (company-lsp-async t)
      (company-lsp-cache-candidates 'auto)
      (company-lsp-enable-recompletion t)
      (company-lsp-enable-snippet t)
      :config
      (push 'company-lsp company-backends))

    (use-package company-tabnine
      :ensure t
      :config
      (add-to-list 'company-backends #'company-tabnine))

    (use-package lsp-mode
      :ensure t
      :hook (lsp-mode . company-mode)
      :bind
      (:map lsp-mode-map
            ("C-M-r" . lsp-rename))
      :custom
      (lsp-before-save-edits t)
      (lsp-eldoc-render-all nil)
      (lsp-highlight-symbol-at-point nil)
      (lsp-inhibit-message t)
      (lsp-message-project-root-warning t)
      (lsp-prefer-flymake nil)
      :config
      (use-package lsp-clients))

    (use-package lsp-ui
      :ensure t
      :after lsp-mode avy
      :hook
      (lsp-mode-hook . lsp-ui-mode)
      (lsp-after-open-hook . lsp-enable-imenu)
      :bind
      (:map lsp-ui-mode-map
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references))
      (:map mode-specific-map
            ("R" . lsp-restart-workspace))
      (:map custom-goto-map
            ("i" . lsp-ui-imenu))
      :custom
      (lsp-ui-doc-enable nil)
      (lsp-ui-flycheck-enable t)
      (lsp-ui-imenu-enable t)
      (lsp-ui-peek-enable t)
      (lsp-ui-sideline-enable t)
      (lsp-ui-sideline-ignore-duplicate t))

    (use-package lsp-ivy
      :ensure t
      :after lsp-mode avy
      :bind
      (:map mode-specific-map
            ("o b" . lsp-ivy-workspace-symbol)
            ("o g" . lsp-ivy-global-workspace-symbol))
      (:map lsp-mode-map
            ([remap xref-find-apropos] . lsp-ivy-global-workspace-symbol)))

    (use-package direnv
      :ensure t
      :config
      (direnv-mode))

    (use-package elmacro
      :ensure t)
  '';
  emacsCodeSearchSetup = ''
    (use-package codesearch
      :ensure t
      :custom
      (codesearch-global-csearchindex "${config.secrets.dev.workspaceRoot}/.csearchindex"))

    (use-package counsel-codesearch
      :ensure t
      :after codesearch
      :bind
      (:map mode-specific-map
            ("c" . counsel-codesearch)))

    (use-package projectile-codesearch
      :ensure t
      :after codesearch)
  '';
in {
  options = {
    custom.dev = {
      playground.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable non-production tools to play with.";
      };
      codesearch.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Codesearch dev infra.";
      };
      patching.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable patching helper tools.";
      };
      analysis.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable dev analysis tools.";
      };
      statistics.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable code stats tools.";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable various misc tools.'';
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable development infra for Emacs.'';
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable XMonad keybindings.'';
      };
      metadataCacheInstructions = mkOption {
        type = types.lines;
        default = '''';
        description = ''Set of commands needed to initialize develepment data cache.'';
      };
      pythonLib = mkOption {
        type = types.lines;
        default = ''
            from Xlib import X, display, error, Xatom, Xutil
            import Xlib.protocol.event


            def sendEvent(win, ctype, data, mask=None):
                data = (data+[0]*(5-len(data)))[:5]
                ev = Xlib.protocol.event.ClientMessage(window=win, client_type=ctype, data=(32,(data)))

                if not mask:
                    mask = (X.SubstructureRedirectMask|X.SubstructureNotifyMask)
                win.send_event(ev, event_mask=mask)


            def switch_desktop(index):
                display = Xlib.display.Display()
                sendEvent(display.screen().root, display.intern_atom("_NET_CURRENT_DESKTOP"),
                          [index, X.CurrentTime])
                display.flush()
        '';
        readOnly = true;
        internal = true;
        description = ''
          Common python routines to be included into custom scripts,
          but not yet deserving its own package/repo/etc.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.playground.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          # https://github.com/Matty9191/ssl-cert-check
          # https://github.com/alexmavr/swarm-nbt
          # https://github.com/moncho/dry
          # https://hub.docker.com/r/nicolaka/netshoot/
          # rstudio # qt plugins broken
          drone
          drone-cli
          jenkins
          python3Packages.deprecated
          python3Packages.unittest-data-provider
          terracognita
          terraform
          tflint
          vector
        ];
      };
    })
    (mkIf cfg.codesearch.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          codesearch
        ];
        programs = {
          zsh.sessionVariables = {
            CSEARCHINDEX = "${config.secrets.dev.workspaceRoot}/.csearchindex";
          };
          bash.sessionVariables = {
            CSEARCHINDEX = "${config.secrets.dev.workspaceRoot}/.csearchindex";
          };
        };
      };
      systemd.user.services."codesearch-reindex" = {
        description = "Codesearch index updating";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          Environment = [
            "CSEARCHINDEX=${config.secrets.dev.workspaceRoot}/.csearchindex"
          ];
          ExecStart = "${pkgs.codesearch}/bin/cindex ${config.secrets.dev.workspaceRoot}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."codesearch-reindex" = {
        description = "Codesearch index updating";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "5min";
          OnUnitActiveSec = "1h";
        };
      };
    })
    (mkIf (cfg.codesearch.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [
          epkgs.codesearch
          epkgs.counsel-codesearch
          epkgs.projectile-codesearch
        ];
      };
      ide.emacs.config = ''${emacsCodeSearchSetup}'';
    })
    (mkIf cfg.patching.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          patchutils
          wiggle
          ruplacer
        ];
      };
    })
    (mkIf cfg.analysis.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          diffoscope
          elfinfo
          flamegraph
          gdb
          gdbgui
          hopper
          patchelf
          patchutils
          radare2
          radare2-cutter
          sysdig
          valgrind
        ] ++ lib.optionals (config.attributes.staging.enable) [
          q-text-as-data
          textql
          visidata # TODO: make overlay
          datamash
          xsv
          xurls
          txr # TODO: get started, read docs
        ];
      };
    })
    (mkIf cfg.statistics.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          cloc
          gource
          sloccount
          tokei
        ] ++ lib.optionals (config.attributes.staging.enable) [
          scc
        ];
      };
    })
    (mkIf cfg.misc.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          icdiff
          ix
          loop
          lorri
          pv
          watchexec
          wstunnel
        ] ++ lib.optionals config.attributes.staging.enable [
          async
          mkcert
        ];
        programs.direnv = {
          enable = true;
          enableZshIntegration = true;
        };
      };
      systemd.services.redis.postStart = cfg.metadataCacheInstructions;
      systemd.user.services."lorri-fixed" = { # one from nixpkgs fails to socket-start for some reason
        description = "Start Lorri daemon";
        path = with pkgs; [ config.nix.package gnutar gzip ];
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          ExecStart = "${pkgs.lorri}/bin/lorri daemon";
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = "read-only";
          Restart = "on-failure";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
    })
    (mkIf cfg.emacs.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [
          epkgs.company-restclient
          epkgs.diff-hl
          epkgs.direnv
          epkgs.elmacro
          epkgs.fic-mode
          epkgs.jinja2-mode
          epkgs.multi-compile
          epkgs.webpaste
        ];
      };
      ide.emacs.config = ''${emacsDevSetup}'';
    })
    (mkIf (cfg.xmonad.enable && config.custom.virtualization.docker.enable) {
      wm.xmonad.keybindings = {
        "M-s d <Up>" = ''spawn "${pkgs.systemd}/bin/systemctl restart docker-devdns.service"'';
        "M-s d <Down>" = ''spawn "${pkgs.systemd}/bin/systemctl stop docker-devdns.service"'';
      };
    })
  ];
}
