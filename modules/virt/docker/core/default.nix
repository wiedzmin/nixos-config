{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# nsp>ctop|dive|lazydocker|libcgroup
# nsp>ctop npkg#ctop
# nsp>dive npkg#dive
# nsp>lazydocker npkg#lazydocker
# nsp>libcgroup npkg#libcgroup
# nsp>docker-slim|dockfmt|nsjail|skopeo
# nsp>docker-slim npkg#docker-slim
# nsp>dockfmt npkg#dockfmt
# nsp>nsjail npkg#nsjail
# nsp>skopeo npkg#skopeo

let
  cfg = config.ext.virtualization.docker.core;
  user = config.attributes.mainUser.name;
  inherit (config.wmCommon) prefix;
in
{
  options = {
    ext.virtualization.docker.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker core setup";
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

      home-manager.users."${user}" = {
        # TODO: consider adding also `docker-compose-language-service` for compose
        home.packages = with pkgs; [
          dlint
          hadolintd
          nodePackages.dockerfile-language-server-nodejs
        ];
        xdg.configFile."hadolint.yaml".text = builtins.toJSON {
          ignored = [ "DL3007" ];
          trustedRegistries = [ "docker.io" ];
        };
      };
      shell.prompts.starship.modulesConfiguration = { docker_context = { format = "via [🐋 $context](blue bold)"; }; };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        docker = {
          matches = [
            {
              trigger = ":dcr";
              replace = "docker-compose up --detach --build && docker-compose restart $|$"; # nsp>docker-compose npkg#docker-compose
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.dockerfile-mode ];
      ide.emacs.core.config = lib.optionalString (!config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst/non-ts.nix ] [ ./elisp/non-ts.el ]) +
        lib.optionalString (config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst/ts.nix ] [ ./elisp/ts.el ]);
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
          url = "https://www.google.ru/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          searchSuffix = "?q=docker+";
        };
        "dockerhub" = {
          desc = "Docker Hub";
          url = "https://hub.docker.com/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          searchSuffix = "search/?q=";
        };
      };
    })
  ];
}
