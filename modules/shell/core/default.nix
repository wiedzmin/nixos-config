{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.shell.core;
  user = config.attributes.mainUser.name;
  serviceAttrsNames = [ "global" "emacs" ];
  envVars = filterAttrs (name: _: !builtins.elem name serviceAttrsNames);
in
{
  options = {
    shell.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable core shell setup";
      };
      variables = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
        description = "Metadata-augmented environment variables registry";
      };
      queueing.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell commands queueing, using `pueue` machinery";
      };
      dev.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell-related development infra";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell-related Emacs infra";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      console.useXkbConfig = true;

      environment.variables = foldl (a: b: a // (envVars b)) { }
        (builtins.filter (e: builtins.hasAttr "global" e && e.global) cfg.variables);
      environment.sessionVariables = foldl (a: b: a // (envVars b)) { }
        (builtins.filter (e: builtins.hasAttr "global" e && e.global) cfg.variables);
      ide.emacs.core.environment = foldl (a: b: a // (envVars b)) { }
        (builtins.filter (e: builtins.hasAttr "emacs" e && e.emacs) cfg.variables);

      home-manager.users."${user}" = {
        programs.readline = {
          enable = true;
          extraConfig = ''
            set echo-control-characters off
          '';
        };
        home.sessionVariables = foldl (a: b: a // (envVars b)) { } cfg.variables;
        programs.zsh.sessionVariables = {
          YSU_IGNORED_ALIASES = [ "g" "ll" ];
          YSU_MODE = "ALL";
        } // (foldl (a: b: a // (envVars b)) { } cfg.variables);
        programs.bash.sessionVariables = foldl (a: b: a // (envVars b)) { } cfg.variables;
        programs.fzf.enable = true;
        programs.command-not-found = {
          enable = true;
          dbPath = ./assets/programs.sqlite;
        };
        home.packages = with pkgs; [
          libnotify # FWIW
          perl # for plugins
          bashate # linter
          fancy-motd
          ets
        ];
      };
    })
    (mkIf (cfg.enable && cfg.dev.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ checkbashisms shellcheck ]; };
    })
    (mkIf (cfg.enable && cfg.queueing.enable) {
      home-manager.users."${user}" = {
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
      navigation.bookmarks.entries = {
        pueue-wiki = {
          desc = "Pueue github project wiki";
          remote.url = "https://github.com/Nukesor/pueue/wiki";
        };
        emacs-pueue-repo = {
          desc = "Pueue emacs frontend project repo";
          remote.url = "https://github.com/xFA25E/pueue";
        };
      };
    })
    (mkIf (cfg.enable && cfg.queueing.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        # TODO: some fzf-based tasks listing automation
        xdg.configFile."espanso/user/shell_core.yml".text = ''
          name: shell_core
          parent: default

          matches:
            - trigger: ":pus"
              replace: "pueue status"

            - trigger: ":pul"
              replace: "pueue log"

            - trigger: ":puc"
              replace: "pueue clean"

            - trigger: ":pur"
              replace: "pueue restart $|$"

            - trigger: ":pupr"
              replace: "pueue restart --in-place $|$"
        '';
      };
    })
    (mkIf (cfg.enable && cfg.dev.enable && cfg.emacs.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ nodePackages.bash-language-server ]; };
      ide.emacs.core.extraPackages = epkgs: [ epkgs.flycheck-checkbashisms epkgs.pueue ];
      ide.emacs.core.config = builtins.readFile ./emacs/shell.el;
    })
  ];
}
