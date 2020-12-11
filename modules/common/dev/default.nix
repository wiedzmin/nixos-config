{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.dev;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  imports = [ ./ansible.nix ];

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
      codesearch.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Codesearch dev infra.";
      };
      comby.excludes = mkOption {
        type = types.listOf types.str;
        default = [ ".envrc" ".gitattributes" ".pre-commit-config.yaml" "configuration.nix" "shell.nix" ];
        description = ''
          Patterns to ignore when running Comby refactoring.

          Comby plays badly with symlinks to /nix/store, so here they are.
        '';
      };
      patching.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable patching helper tools.";
      };
      statistics.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable code stats tools.";
      };
      # IDEA: make script for extracting from shell history based on substring
      bookmarks.entries = mkOption {
        type = types.attrs;
        default = {
          gourmet = { local.path = "${wsRoot "github"}/wiedzmin/gourmet"; };
          home-manager = {
            local.path = "${wsRoot "github"}/rycee/home-manager";
            remote = {
              url = "https://github.com/rycee/home-manager/";
              jump = true;
              desc = "home-manager upstream repo";
              searchSuffix = "search?q=";
            };
          };
          nixos = {
            local.path = "/etc/nixos";
            remote = {
              url = "https://github.com/wiedzmin/nixos-config/";
              jump = true;
              desc = "My NixOS configurations";
              searchSuffix = "search?q=";
            };
          };
          nixpkgs = {
            local.path = "${wsRoot "github"}/NixOS/nixpkgs";
            remote = {
              url = "https://github.com/NixOS/nixpkgs/";
              jump = true;
              desc = "Nixpkgs upstream repo";
              searchSuffix = "search?q=";
            };
          };
          emacs-overlay = {
            local.path = "${wsRoot "github"}/nix-community/emacs-overlay";
            remote = {
              url = "https://github.com/nix-community/emacs-overlay/";
              jump = true;
              desc = "nix emacs overlay";
              searchSuffix = "search?q=";
            };
          };
          # https://github.com/nix-community/emacs-overlay
          nur-packages = { local.path = "${wsRoot "github"}/wiedzmin/nur-packages"; };
          postgres = { local.path = "${wsRoot "github"}/postgres/postgres"; };
        };
        description = "Bookmarks data.";
      };
      timeTracking.extensions = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "List of file extensions to be considered dev-related";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various misc tools.";
      };
      direnv.granularity = mkOption {
        type = types.enum [ "project" "file" ];
        default = "project";
        description = "The approach to direnv environments switching";
      };
      direnv.whitelist = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "The collection of whitelisting prefixes for direnv to allow";
      };
      timeTracking.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable dev timetracking infra.";
      };
      timeTracking.identities = mkOption {
        type = types.attrs;
        default = { };
        example = {
          "id" = {
            jira = {
              creds = [ "login" "password" ];
              server = "<server url>";
              project = "foo";
            };
            workspaceRoot = "/path/to/root";
            meta = {
              bar = "quux";
            };
          };
        };
        description = "Timetracking identities collection.";
      };
      repoSearch.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable global repositories search.";
      };
      repoSearch.root = mkOption {
        type = types.str;
        default = wsRootAbs "global";
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
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && config.custom.navigation.bookmarks.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        open-project = mkPythonScriptWithDeps "open-project" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis ])
          (readSubstituted ../subst.nix ./scripts/open-project.py);
      };
      custom.navigation.bookmarks.entries = cfg.bookmarks.entries;
    })
    (mkIf (cfg.enable && cfg.codesearch.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ codesearch ];
        programs = {
          zsh.sessionVariables = { CSEARCHINDEX = "${wsRootAbs "global"}/.csearchindex"; };
          bash.sessionVariables = { CSEARCHINDEX = "${wsRootAbs "global"}/.csearchindex"; };
        };
      };
      systemd.user.services."codesearch-reindex" = {
        description = "Codesearch index updating";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          Environment = [ "CSEARCHINDEX=${wsRootAbs "global"}/.csearchindex" ];
          ExecStart = "${pkgs.codesearch}/bin/cindex ${wsRootAbs "global"}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."codesearch-reindex" = renderTimer "Codesearch index updating" "" "" "*-*-* 4:00:00";
    })
    (mkIf (cfg.enable && cfg.codesearch.enable && cfg.emacs.enable) {
      ide.emacs.extraPackages = epkgs: [ epkgs.codesearch epkgs.counsel-codesearch epkgs.projectile-codesearch ];
      ide.emacs.config = readSubstituted ../subst.nix ./emacs/codesearch.el;
    })
    (mkIf (cfg.enable && cfg.patching.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ patchutils wiggle diffoscope ];
      };
    })
    (mkIf (cfg.enable && cfg.statistics.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ cloc gource sloccount tokei logtop ];
      };
    })
    (mkIf (cfg.enable && cfg.repoSearch.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        reposearch = mkPythonScriptWithDeps "reposearch" (with pkgs; [ fd python3Packages.libtmux xsel emacs nurpkgs.pystdlib ])
          (readSubstituted ../subst.nix ./scripts/reposearch.py);
      };
    })
    (mkIf (cfg.enable && cfg.timeTracking.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        ttctl = mkPythonScriptWithDeps "ttctl" (with pkgs; [ python3Packages.jira python3Packages.pytz python3Packages.redis nurpkgs.pystdlib yad ])
          (readSubstituted ../subst.nix ./scripts/ttctl.py);
      };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set timetracking/identities ${lib.strings.escapeNixString (builtins.toJSON cfg.timeTracking.identities)}
      '';
      custom.pim.timeTracking.rules = with config.attributes.browser; ''
        -- TODO: parameterize web resources
        -- TODO: parameterize IDE (probably, not only emacs)
        current window ($program == [${concatStringListsQuoted ", " [default.windowClass fallback.windowClass]
                                      }] && $title =~ /habr/) ==> tag site:habr,
        current window ($program == [${concatStringListsQuoted ", " [default.windowClass fallback.windowClass]
                                      }] && $title =~ /pypi/) ==> tag site:pypi,
        current window ($program == [${concatStringListsQuoted ", " [default.windowClass fallback.windowClass]
                                      }] && $title =~ /stackoverflow/) ==> tag site:stackoverflow,
        ${concatStringsSep ",\n" (lib.mapAttrsToList (ext: tag: ''
          current window ($title =~ /^emacs - [^ ]+\.${ext} .*$/) ==> tag ${tag}'') cfg.timeTracking.extensions)},
      '';
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      wmCommon.wsMapping.rules = [{
        class = "Cutter";
        desktop = "tools";
      }];
      custom.dev.git.credentials.mapping = {
        "*github.com*" = {
          skip_username = 7;
          target = "dev/forges/github.com";
        };
        "*bitbucket.org*" = { target = "dev/forges/bitbucket.org"; };
      };
      custom.dev.git.gitignore = ''
        .direnv/
      '';
      home-manager.users.${user} = {
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
        };
        home.packages = with pkgs; [
          nurpkgs.comby
          devdocs-desktop
          icdiff
        ];
        programs.direnv = {
          enable = true;
          enableZshIntegration = true;
          enableNixDirenvIntegration = true;
        };
        xdg.configFile."direnv/direnv.toml".text = toToml {
          global = { disable_stdin = true; };
          whitelist = { prefix = cfg.direnv.whitelist; };
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.extraPackages = epkgs:
        [
          epkgs.company-restclient
          epkgs.company-tabnine
          epkgs.dap-mode
          epkgs.diff-hl
          epkgs.elmacro
          epkgs.fic-mode
          epkgs.jinja2-mode
          epkgs.lsp-ivy
          epkgs.lsp-treemacs
          epkgs.tuareg
          epkgs.webpaste
          epkgs.yaml-mode
        ] ++ lib.optionals (cfg.direnv.granularity == "project") [ epkgs.direnv ]
        ++ lib.optionals (cfg.direnv.granularity == "file") [ epkgs.nix-buffer ]; # when envrc.el will arrive to nixpkgs
      home-manager.users.${user} = {
        # TODO: fix tabnine build from nixpkgs (permission denied) and put to nur-packages
        xdg.configFile."TabNine/TabNine.toml".text =
          toToml { language.python = { command = "python-language-server"; }; };
      };
      ide.emacs.config = readSubstituted ../subst.nix ./emacs/dev.el;
    })
    (mkIf (cfg.enable && cfg.wm.enable && config.custom.virtualization.docker.enable) {
      wmCommon.keys = [
        {
          key = [ "Control" "d" ];
          cmd = "${pkgs.systemd}/bin/systemctl restart docker-devdns.service";
          mode = "dev";
        }
        {
          key = [ "Control" "Shift" "d" ];
          cmd = "${pkgs.systemd}/bin/systemctl stop docker-devdns.service";
          mode = "dev";
        }
      ] ++ lib.optionals (cfg.repoSearch.enable) [{
        key = [ "r" ];
        cmd = "${pkgs.reposearch}/bin/reposearch";
        mode = "dev";
      }] ++ lib.optionals (config.custom.navigation.bookmarks.enable) [{
        key = [ "p" ];
        cmd = "${pkgs.open-project}/bin/open-project";
        mode = "dev";
      }] ++ lib.optionals (cfg.timeTracking.enable) [{
        key = [ "t" ];
        cmd = "${pkgs.ttctl}/bin/ttctl";
        mode = "dev";
      }];
    })
    (mkIf (cfg.staging.packages != [ ]) {
      home-manager.users.${user} = { home.packages = cfg.staging.packages; };
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ ttctl open-project reposearch ];
      };
    })
  ];
}
