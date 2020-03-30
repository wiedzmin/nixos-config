{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.custom.dev;
in {
  options = {
    custom.dev = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom development infrastructure.";
      };
      workspaceRoots = mkOption {
        type = types.attrs;
        default = { };
        description = "Various workspace roots meta.";
      };
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
      analysis.staging.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable staging settings for analysis.";
      };
      statistics.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable code stats tools.";
      };
      statistics.staging.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable staging settings for statistics.";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various misc tools.";
      };
      misc.staging.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable staging misc settings.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
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
        home.packages = with pkgs; [ codesearch ];
        programs = {
          zsh.sessionVariables = { CSEARCHINDEX = "${cfg.workspaceRoots.global}/.csearchindex"; };
          bash.sessionVariables = { CSEARCHINDEX = "${cfg.workspaceRoots.global}/.csearchindex"; };
        };
      };
      systemd.user.services."codesearch-reindex" = {
        description = "Codesearch index updating";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          Environment = [ "CSEARCHINDEX=${cfg.workspaceRoots.global}/.csearchindex" ];
          ExecStart = "${pkgs.codesearch}/bin/cindex ${cfg.workspaceRoots.global}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."codesearch-reindex" = renderTimer "Codesearch index updating" "10min" "2h" "";
    })
    (mkIf (cfg.codesearch.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [ epkgs.codesearch epkgs.counsel-codesearch epkgs.projectile-codesearch ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./codesearch.el; }));
    })
    (mkIf cfg.patching.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ patchutils wiggle ruplacer ];
      };
    })
    (mkIf cfg.analysis.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs;
          [
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
          ] ++ lib.optionals (cfg.analysis.staging.enable) [
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
      environment.systemPackages = with pkgs;
        with config.boot.kernelPackages;
        [
          perf
          # hotspot # rarely used
        ];
    })
    (mkIf cfg.statistics.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs;
          [ cloc gource sloccount tokei ] ++ lib.optionals (cfg.statistics.staging.enable) [ scc ];
      };
    })
    (mkIf cfg.misc.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.file = {
          "workspace/.editorconfig".text = ''
            # top-most EditorConfig file
            root = true

            # Unix-style newlines with a newline ending every file
            [*]
            end_of_line = lf
            insert_final_newline = true
            indent_style = space
            charset = utf-8
            trim_trailing_whitespace = true

            # Matches multiple files with brace expansion notation
            # Set default charset
            [*.{js,py,go}]
            charset = utf-8

            # 4 space indentation
            [*.py]
            indent_style = space
            indent_size = 4

            # Tab indentation (no size specified)
            [Makefile]
            indent_style = tab

            # Indentation override for all JS under lib directory
            [lib/**.js]
            indent_style = space
            indent_size = 2

            # Matches the exact files either package.json or .travis.yml
            [{package.json,.travis.yml}]
            indent_style = space
            indent_size = 2

            [*.{json,yml}]
            indent_style = space
            indent_size = 2

            [*.md]
            trim_trailing_whitespace = false
          '';
        } // lib.optionalAttrs (config.custom.shell.enable) {
          "tmuxp/dev.yml".text = ''
            session_name: dev
            windows:
              - window_name: mc
                start_directory: ${config.custom.dev.workspaceRoots.global}/github.com/wiedzmin
                panes:
                  - mc
          '';
        };
        home.packages = with pkgs;
          [ icdiff ix loop lorri pv wstunnel ] ++ lib.optionals (cfg.misc.staging.enable) [ async mkcert ];
        programs.direnv = {
          enable = true;
          enableZshIntegration = true;
        };
      };
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
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
    })
    (mkIf cfg.emacs.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        xdg.configFile."TabNine/TabNine.toml".source = (pkgs.runCommand "TabNine.toml" {
          buildInputs = [ pkgs.remarshal ];
          preferLocalBuild = true;
        } ''
          remarshal -if json -of toml \
            < ${pkgs.writeText "TabNine.json" (builtins.toJSON { language.python = { command = "mspyls"; }; })} \
            > $out
        '');
        programs.emacs.extraPackages = epkgs: [
          epkgs.company-restclient
          epkgs.company-tabnine
          epkgs.diff-hl
          epkgs.direnv
          epkgs.elmacro
          epkgs.fic-mode
          epkgs.jinja2-mode
          epkgs.lsp-ivy
          epkgs.multi-compile
          epkgs.webpaste
          epkgs.yaml-mode
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
