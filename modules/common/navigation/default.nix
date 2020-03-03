{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.custom.navigation;
in {
  options = {
    custom.navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable navigation infra.
        '';
      };
      webjumps.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable webjumps.";
      };
      webjumps.entries = mkOption {
        type = types.attrs;
        default = { };
        description = "Webjumps entries.";
      };
      searchengines.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable searchengines.";
      };
      searchengines.entries = mkOption {
        type = types.attrs;
        default = { };
        description = "Searchengines entries.";
      };
      gmrun.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable gmrun.
        '';
      };
      gmrun.historySize = mkOption {
        type = types.int;
        default = 1024;
        description = ''
          History length.
        '';
      };
      gmrun.terminalApps = mkOption {
        type = types.listOf types.str;
        default = [ "info" "lynx" "man" "mc" "ssh" "vi" "vim" ];
        description = ''
          List of apps to always run in terminal.
        '';
      };
      mc.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Midnight Commander.
        '';
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable misc setup.
        '';
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable customized navigation for Emacs.
        '';
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        search_prompt = writePythonScriptWithPythonPackages "search_prompt" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.redis
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./search_prompt.py; })));
        search_selection = writePythonScriptWithPythonPackages "search_selection" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.redis
        ] (builtins.readFile (pkgs.substituteAll
          ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./search_selection.py; })));
        webjumps = writePythonScriptWithPythonPackages "webjumps" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.redis
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./webjumps.py; })));
        insert_snippet = writePythonScriptWithPythonPackages "insert_snippet" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.redis
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./insert_snippet.py; })));
      };

      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ j4-dmenu-desktop ]; };
    })
    (mkIf (cfg.enable && cfg.webjumps.enable) {
      assertions = [{
        assertion = cfg.webjumps.enable && cfg.webjumps.entries != { };
        message = "navigation: no webjumps to follow but they are enabled.";
      }];

      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set nav/webjumps ${lib.strings.escapeNixString (builtins.toJSON cfg.webjumps.entries)}
      '';
    })
    (mkIf (cfg.enable && cfg.searchengines.enable) {
      assertions = [{
        assertion = cfg.searchengines.enable && cfg.searchengines.entries != { };
        message = "navigation: no searchengines to follow but they are enabled.";
      }];

      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set nav/searchengines ${
          lib.strings.escapeNixString (builtins.toJSON cfg.searchengines.entries)
        }
      '';
    })
    (mkIf (cfg.enable && cfg.gmrun.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ gmrun ];
        home.file = {
          ".gmrunrc".text = (builtins.readFile
            (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./gmrunrc; })));
        };
      };
    })
    (mkIf (cfg.enable && cfg.mc.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ mc ];
        xdg.configFile."mc/mc.ext".text = ''
          regex/\.([pP][dD][fF])$
              Include=ebook
          regex/\.([dD][jJ][vV][uU])$
              Include=ebook

          regex/\.([jJ][pP][gG])$
              Include=image
          regex/\.([pP][nN][gG])$
              Include=image
          regex/\.([jJ][pP][eE][gG])$
              Include=image

          regex/\.([mM][pP]4)$
              Include=video
          regex/\.([fF][lL][vV])$
              Include=video

          include/ebook
              Open=(${config.attributes.defaultCommands.ebookReader} %f >/dev/null 2>&1 &)

          include/image
              Open=(${config.attributes.defaultCommands.imageViewer} %f >/dev/null 2>&1 &)

          include/video
              Open=(${config.attributes.defaultCommands.videoPlayer} %f >/dev/null 2>&1 &)
        '';
      };
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs = {
          z-lua = {
            enable = true;
            enableZshIntegration = true;
            options = [ "fzf" "enhanced" "once" ];
          };
          skim = {
            enable = true;
            historyWidgetOptions = [ "--exact" ];
            defaultOptions = [ "--height 40%" "--prompt ⟫" ];
            fileWidgetCommand = "${pkgs.fd}/bin/fd --type f";
            fileWidgetOptions = [ "--preview 'head {}'" ];
            changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type d";
            changeDirWidgetOptions = [ "--preview 'tree -C {} | head -200'" ];
            enableZshIntegration = true;
          };
        };
        home.packages = with pkgs; [ pueue ];
      };
      systemd.user.services."pueue-daemon" = {
        description = "Pueue daemon";
        path = [ pkgs.bash ];
        serviceConfig = {
          ExecStart = "${pkgs.pueue}/bin/pueued";
          ExecReload = "${pkgs.pueue}/bin/pueued";
          Restart = "no";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
        wantedBy = [ "multi-user.target" ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ ripgrep ];
        programs.emacs.extraPackages = epkgs: [
          epkgs.ace-link
          epkgs.ace-window
          epkgs.avy
          epkgs.avy-flycheck
          epkgs.avy-zap
          epkgs.counsel
          epkgs.counsel-projectile
          epkgs.dired-filetype-face
          epkgs.dired-git-info
          epkgs.dired-hide-dotfiles
          epkgs.dired-launch
          epkgs.dired-narrow
          epkgs.dired-quick-sort
          epkgs.imenu-anywhere
          epkgs.ivy
          epkgs.ivy-historian
          epkgs.ivy-rich
          epkgs.ivy-xref
          epkgs.ivy-yasnippet
          epkgs.link-hint
          epkgs.phi-search
          epkgs.polymode
          epkgs.projectile
          epkgs.rg
          epkgs.swiper
        ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./navigation.el; }));
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-/" = ''spawn "${pkgs.search_selection}/bin/search_selection" >> showWSOnProperScreen "web"'';
        "M-C-/" = ''spawn "${pkgs.search_prompt}/bin/search_prompt" >> showWSOnProperScreen "web"'';
        "M-j" = ''spawn "${pkgs.webjumps}/bin/webjumps" >> showWSOnProperScreen "web"'';
        "M-o" = ''spawn "${pkgs.insert_snippet}/bin/insert_snippet"'';
      };
    })
  ];
}
