{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.dev;
  globalRoot = config.custom.navigation.workspaceRoots.global;
  githubRoot = config.custom.navigation.workspaceRoots.github;

in {
  options = {
    custom.dev = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom development infrastructure.";
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
      statistics.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable code stats tools.";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        description = "Whether to enable shell bookmarks.";
        default = false;
      };
      bookmarks.entries = mkOption {
        type = types.attrs;
        default = {
          nixos = "/etc/nixos";
          nixpkgs-channels = "${homePrefix githubRoot}/NixOS/nixpkgs-channels";
          nixpkgs-proposed = "${homePrefix githubRoot}/wiedzmin/nixpkgs";
          home-manager = "${homePrefix githubRoot}/rycee/home-manager";
          postgres = "${homePrefix githubRoot}/postgres/postgres";
        };
        description = "Bookmarks data.";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various misc tools.";
      };
      remote.commands = mkOption {
        description = "Predefined commands list to execute remotely. Note that those must be present on ssh target.";
        type = types.listOf types.str;
        default = [ "ctop" "jnettop" ];
      };
      repoSearch.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable global repositories search.";
      };
      repoSearch.root = mkOption {
        type = types.str;
        default = homePrefix globalRoot;
        description = "Search root.";
      };
      repoSearch.depth = mkOption {
        type = types.int;
        default = 4;
        description = "Search depth.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
      staging.packages = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "List of staging packages.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
      pythonLib = mkOption {
        type = types.attrs;
        default = {
          "notify" = {
            packages = with pkgs; [ ]; # TODO: integrate later
            patch = builtins.readFile ./lib/notify.py;
          };
          "xlib" = {
            packages = with pkgs; [ ];
            patch = builtins.readFile ./lib/xlib.py;
          };
        };
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
    (mkIf (cfg.enable && cfg.playground.enable) {
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
    (mkIf (cfg.enable && cfg.codesearch.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ codesearch ];
        programs = {
          zsh.sessionVariables = { CSEARCHINDEX = "${homePrefix globalRoot}/.csearchindex"; };
          bash.sessionVariables = { CSEARCHINDEX = "${homePrefix globalRoot}/.csearchindex"; };
        };
      };
      systemd.user.services."codesearch-reindex" = {
        description = "Codesearch index updating";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          Environment = [ "CSEARCHINDEX=${homePrefix globalRoot}/.csearchindex" ];
          ExecStart = "${pkgs.codesearch}/bin/cindex ${homePrefix globalRoot}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."codesearch-reindex" = renderTimer "Codesearch index updating" "10min" "2h" "";
    })
    (mkIf (cfg.enable && cfg.codesearch.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [ epkgs.codesearch epkgs.helm-codesearch ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./codesearch.el; }));
    })
    (mkIf (cfg.enable && cfg.patching.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ patchutils wiggle ruplacer ];
      };
    })
    (mkIf (cfg.enable && cfg.analysis.enable) {
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
        ];
      };
    })
    (mkIf (cfg.enable && cfg.statistics.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ cloc gource sloccount tokei ];
      };
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) { custom.shell.bookmarks.entries = cfg.bookmarks.entries; })
    (mkIf (cfg.enable && cfg.repoSearch.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        reposearch = mkPythonScriptWithDeps "reposearch" [
          pkgs.fd
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.libtmux
          pkgs.python3Packages.notify2
          pkgs.xsel
          pkgs.emacs
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./reposearch.py; })));
      };
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ reposearch ]; };
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      custom.dev.git.gitignore = ''
        shell.nix
        .envrc
      '';
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
                start_directory: ${homePrefix githubRoot}/wiedzmin
                panes:
                  - mc
          '';
        };
        home.packages = with pkgs; [ icdiff ix loop pv wstunnel fastmod the-way devdocs-desktop ];
        programs.direnv = {
          enable = true;
          enableZshIntegration = true;
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        xdg.configFile."TabNine/TabNine.toml".source = (pkgs.runCommand "TabNine.toml" {
          buildInputs = [ pkgs.remarshal ];
          preferLocalBuild = true;
        } ''
          remarshal -if json -of toml \
            < ${
              pkgs.writeText "TabNine.json"
              (builtins.toJSON { language.python = { command = "python-language-server"; }; })
            } \
            > $out
        '');
        programs.emacs.extraPackages = epkgs: [
          epkgs.company-restclient
          epkgs.company-tabnine
          epkgs.diff-hl
          epkgs.direnv
          epkgs.elmacro
          epkgs.fic-mode
          epkgs.helm-lsp
          epkgs.jinja2-mode
          epkgs.multi-compile
          epkgs.webpaste
          epkgs.yaml-mode
        ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./dev.el; }));
    })
    (mkIf (cfg.enable && cfg.wm.enable && config.custom.virtualization.docker.enable) {
      wmCommon.keys = [
        {
          key = [ "d" ];
          cmd = "${pkgs.systemd}/bin/systemctl restart docker-devdns.service";
          mode = "service";
        }
        {
          key = [ "Shift" "d" ];
          cmd = "${pkgs.systemd}/bin/systemctl stop docker-devdns.service";
          mode = "service";
        }
      ] ++ lib.optionals (cfg.repoSearch.enable) [{
        key = [ "r" ];
        cmd = "${pkgs.reposearch}/bin/reposearch";
        mode = "run";
      }];
    })
    (mkIf (cfg.staging.packages != [ ]) {
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = cfg.staging.packages; };
    })
  ];
}
