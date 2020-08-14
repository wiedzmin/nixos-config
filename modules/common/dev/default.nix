let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-16_04_20 = import deps.nixpkgs-pinned-16_04_20 { config.allowUnfree = true; };
  nixpkgs-pinned-09_07_20 = import deps.nixpkgs-pinned-09_07_20 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
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
          nixos = "/etc/nixos";
          nixpkgs-channels = "${wsRoot "github"}/NixOS/nixpkgs-channels";
          nixpkgs-proposed = "${wsRoot "github"}/wiedzmin/nixpkgs";
          home-manager = "${wsRoot "github"}/rycee/home-manager";
          postgres = "${wsRoot "github"}/postgres/postgres";
          # RSS_Feederbot = "${wsRootAbs "github"}/Dextroz/RSS_Feederbot";
          # SIM-UoW-Timetable-bot = "${wsRootAbs "github"}/xlanor/SIM-UoW-Timetable-bot";
          # VorpalRobot = "${wsRootAbs "github"}/Tronikart/VorpalRobot";
          # boobsbot = "${wsRootAbs "github"}/ouhettur/boobsbot";
          # bot-motivator = "${wsRootAbs "github"}/SabaunT/bot-motivator";
          # bot_against_humanity = "${wsRootAbs "gitlab"}/OctoNezd/bot_against_humanity";
          # cmsysbot-telegram = "${wsRootAbs "github"}/oddworldng/cmsysbot-telegram";
          # crypto-coins-info-bot-v2 = "${wsRootAbs "github"}/lytves/crypto-coins-info-bot-v2";
          # dclavorofvg-bot = "${wsRootAbs "github"}/marcotessarotto/dclavorofvg-bot";
          # friendship-quiz-bot = "${wsRootAbs "github"}/alistvt/friendship-quiz-bot";
          # gatekeeper-bot = "${wsRootAbs "github"}/Juhannuspukki/gatekeeper-bot";
          # homework-help-bot = "${wsRootAbs "github"}/leeweiminsg/homework-help-bot";
          # logbot = "${wsRootAbs "github"}/apiad/logbot";
          # mau_mau_bot = "${wsRootAbs "github"}/jh0ker/mau_mau_bot";
          # mvc_model_bot_developing = "${wsRootAbs "github"}/mmdaz/mvc_model_bot_developing";
          # nmjpeg-bot = "${wsRootAbs "github"}/zeroone2numeral2/nmjpeg-bot";
          # pricebot = "${wsRootAbs "github"}/lytves/pricebot";
          # privibot = "${wsRootAbs "github"}/pawamoy/privibot";
          # python-telegram-bot = "${wsRootAbs "github"}/python-telegram-bot/python-telegram-bot";
          # regex-bot = "${wsRootAbs "github"}/zeroone2numeral2/regex-bot";
          # remind_me_bot = "${wsRootAbs "github"}/dmakeienko/remind_me_bot";
          # someone-bot = "${wsRootAbs "github"}/zeroone2numeral2/someone-bot";
          # sudobot = "${wsRootAbs "github"}/bvanrijn/sudobot";
          # telegram-facebook-bot = "${wsRootAbs "github"}/MorenK1/telegram-facebook-bot";
          # telegram-movie-bot = "${wsRootAbs "github"}/rpolve/telegram-movie-bot";
          # tgbot = "${wsRootAbs "github"}/PaulSonOfLars/tgbot";
          # toptracksbot = "${wsRootAbs "github"}/pltnk/toptracksbot";
          # tt-bot = "${wsRootAbs "github"}/MgCoders/tt-bot";
          # twitter-lists-bot = "${wsRootAbs "github"}/lytves/twitter-lists-bot";
          # watermarker-bot = "${wsRootAbs "github"}/alistvt/watermarker-bot";
        };
        description = "Bookmarks data.";
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
        open-project = mkPythonScriptWithDeps "open-project" (with pkgs; [ pystdlib python3Packages.redis ])
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/open-project.py; })));
      };
      custom.navigation.bookmarks.entries = cfg.bookmarks.entries;
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ open-project ]; };
    })
    (mkIf (cfg.enable && cfg.codesearch.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
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
      ide.emacs.extraPackages = epkgs: [ epkgs.codesearch epkgs.helm-codesearch ];
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./emacs/codesearch.el; }));
    })
    (mkIf (cfg.enable && cfg.patching.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ patchutils wiggle nixpkgs-pinned-16_04_20.diffoscope ];
      };
    })
    (mkIf (cfg.enable && cfg.statistics.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ cloc gource sloccount tokei ];
      };
    })
    (mkIf (cfg.enable && cfg.repoSearch.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        reposearch = mkPythonScriptWithDeps "reposearch" (with pkgs; [ fd python3Packages.libtmux xsel emacs pystdlib ])
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/reposearch.py; })));
      };
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ reposearch ]; };
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      custom.dev.git.credentials.mapping = {
        "*github.com*" = {
          skip_username = 7;
          target = "${config.attributes.mainUser.name}/webservices/social/programming/github.com";
        };
        "*bitbucket.org*" = {
          target = "${config.attributes.mainUser.name}/webservices/social/programming/bitbucket.com";
        };
      };
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
                start_directory: ${wsRootAbs "github"}/wiedzmin
                panes:
                  - mc
          '';
        };
        home.packages = with pkgs; [ comby nixpkgs-pinned-09_07_20.devdocs-desktop icdiff ];
        programs.direnv = {
          enable = true;
          enableZshIntegration = true;
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
          epkgs.diff-hl
          epkgs.elmacro
          epkgs.fic-mode
          epkgs.helm-lsp
          epkgs.jinja2-mode
          epkgs.multi-compile
          epkgs.webpaste
          epkgs.yaml-mode
        ] ++ lib.optionals (cfg.direnv.granularity == "project") [ epkgs.direnv ]
        ++ lib.optionals (cfg.direnv.granularity == "file") [ epkgs.nix-buffer ]; # when envrc.el will arrive to nixpkgs
      home-manager.users."${config.attributes.mainUser.name}" = {
        xdg.configFile."TabNine/TabNine.toml".text =
          toToml { language.python = { command = "python-language-server"; }; };
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./emacs/dev.el; }));
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
      }] ++ lib.optionals (config.custom.navigation.bookmarks.enable) [{
        key = [ "Shift" "p" ];
        cmd = "${pkgs.open-project}/bin/open-project";
        mode = "run";
      }];
    })
    (mkIf (cfg.staging.packages != [ ]) {
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = cfg.staging.packages; };
    })
  ];
}
