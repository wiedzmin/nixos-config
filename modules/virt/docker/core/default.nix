{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ext.virtualization.docker.core;
  user = config.attributes.mainUser.name;
  inherit (config.wmCommon) prefix;
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    ext.virtualization.docker.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker core setup";
      };
      aux.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker auxiliary packages";
      };
      storageDriver = mkOption {
        type = types.str;
        default = "overlay2";
        description = "Docker storage driver";
      };
      emacs.enable = mkOption {
        # TODO: check if it works correctly
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs Docker setup.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Docker-related bookmarks";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      virtualisation.docker = {
        enable = true;
        enableOnBoot = false;
        inherit (cfg) storageDriver;
      };
      environment.variables.DOCKER_CONFIG = xdgConfig user "docker";
      users.users."${user}".extraGroups = [ "docker" ];

      networking.dhcpcd.denyInterfaces = [ "docker*" ];

      nixpkgs.config.packageOverrides = _: {
        dlint = pkgs.writeShellApplication {
          name = "dlint";
          runtimeInputs = with pkgs; [ docker ];
          text = builtins.readFile ./scripts/dlint.sh;
        };
        hadolintd = pkgs.writeShellApplication {
          name = "hadolintd";
          runtimeInputs = with pkgs; [ docker ];
          text = builtins.readFile ./scripts/hadolintd.sh;
        };
      };

      environment.systemPackages = with pkgs; [
        ctop
        dive
        dlint
        docker-compose
        hadolintd
        libcgroup
      ];

      home-manager.users."${user}" = {
        xdg.configFile."hadolint.yaml".text = builtins.toJSON {
          ignored = [ "DL3007" ];
          trustedRegistries = [ "docker.io" ];
        };
      };
      shell.prompts.starship.modulesConfiguration = { docker_context = { format = "via [🐋 $context](blue bold)"; }; };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/match/docker.yml".source = yaml.generate "espanso-docker.yml"
          {
            matches = [
              {
                trigger = ":dcr";
                replace = "docker-compose up --detach --build && docker-compose restart $|$";
              }
            ];
          } // optionalAttrs (config.shell.tmux.enable) {
          filter_title = "\".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*\"";
        };
      };
    })
    (mkIf (cfg.enable && cfg.aux.enable) {
      environment.systemPackages = with pkgs; [
        docker-slim
        dockfmt
        nsjail
        skopeo
        nodePackages.dockerfile-language-server-nodejs
      ];
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.dockerfile-mode ];
      ide.emacs.core.config = lib.optionalString (!config.ide.emacs.core.treesitter.enable) (builtins.readFile ./elisp/docker.el) +
        lib.optionalString (config.ide.emacs.core.treesitter.enable) (builtins.readFile ./elisp/docker-ts.el);
      ide.emacs.core.treesitter.grammars = {
        dockerfile = "https://github.com/camdencheek/tree-sitter-dockerfile";
      };
      ide.emacs.core.treesitter.modeRemappings = {
        dockerfile-mode = "dockerfile-ts-mode";
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.modeBindings = {
        "docker" = [ prefix "Shift" "d" ];
      };
    })
    (mkIf (cfg.enable && config.attributes.debug.exposeScripts) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          dlint
          hadolintd
        ];
      };
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "dockr" = {
          desc = "docker + ";
          remote = {
            url = "https://www.google.ru/";
            searchSuffix = "?q=docker+";
          };
        };
        "dockerhub" = {
          desc = "Docker Hub";
          remote = {
            url = "https://hub.docker.com/";
            searchSuffix = "search/?q=";
          };
        };
      };
    })
  ];
}
