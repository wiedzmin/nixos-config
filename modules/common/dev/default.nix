{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.dev;
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
        default = builtins.readFile ./pythonlib.py;
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
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./codesearch.el; }));
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
          extrace
          flamegraph
          gdb
          gdbgui
          hopper
          libwhich
          lsof
          ltrace
          nmon
          pagemon
          patchelf
          patchutils
          radare2
          radare2-cutter
          strace
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
          jwt-cli
        ];
      };
      environment.systemPackages = with pkgs; with config.boot.kernelPackages; [
        perf
        # hotspot # rarely used
      ];
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
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./dev.el; }));
    })
    (mkIf (cfg.xmonad.enable && config.custom.virtualization.docker.enable) {
      wm.xmonad.keybindings = {
        "M-s d <Up>" = ''spawn "${pkgs.systemd}/bin/systemctl restart docker-devdns.service"'';
        "M-s d <Down>" = ''spawn "${pkgs.systemd}/bin/systemctl stop docker-devdns.service"'';
      };
    })
  ];
}
