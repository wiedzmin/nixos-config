{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.virtualization.docker.core;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  configHome = hm.xdg.configHome;
in {
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
        description = "Whether to enable Docker auxillary packages";
      };
      defaultContainerShell = mkOption {
        type = types.str;
        default = "/bin/bash";
        description = "Default shell to execute within container with `exec -it`";
      };
      storageDriver = mkOption {
        type = types.str;
        default = "overlay2";
        description = "Docker storage driver";
      };
      emacs.enable = mkOption { # TODO: check if it works correctly
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs Docker setup.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      virtualisation.docker = {
        enable = true;
        storageDriver = cfg.storageDriver;
      };
      environment.variables.DOCKER_CONFIG = "${configHome}/docker";
      users.users.${user}.extraGroups = [ "docker" ];

      networking.dhcpcd.denyInterfaces = [ "docker*" ];

      nixpkgs.config.packageOverrides = _: rec {
        dlint =
          mkShellScriptWithDeps "dlint" (with pkgs; [ docker ]) (readSubstituted ../../../subst.nix ./scripts/dlint.sh);
        hadolintd = mkShellScriptWithDeps "hadolintd" (with pkgs; [ docker ])
          (readSubstituted ../../../subst.nix ./scripts/hadolintd.sh);
        docker_containers_traits = mkPythonScriptWithDeps "docker_containers_traits"
          (with pkgs; [ docker nurpkgs.pystdlib python3Packages.redis xsel yad ])
          (readSubstituted ../../../subst.nix ./scripts/docker_containers_traits.py);
        discover_containerized_services =
          mkPythonScriptWithDeps "discover_containerized_services" (with pkgs; [ docker nurpkgs.pystdlib ])
          (readSubstituted ../../../subst.nix ./scripts/discover_containerized_services.py);
        docker_shell = mkPythonScriptWithDeps "docker_shell"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.libtmux python3Packages.redis ])
          (readSubstituted ../../../subst.nix ./scripts/docker_shell.py);
      };

      environment.systemPackages = with pkgs; [
        ctop
        discover_containerized_services
        dive
        dlint
        docker_compose
        docker_containers_traits
        docker_shell
        hadolintd
        libcgroup
      ];

      home-manager.users.${user} = {
        xdg.configFile."hadolint.yaml".text = builtins.toJSON {
          ignored = [ "DL3007" ];
          trustedRegistries = [ "docker.io" ];
        };
      };

      shell.prompts.starship.modulesConfiguration = { docker_context = { format = "via [🐋 $context](blue bold)"; }; };
    })
    (mkIf (cfg.enable && cfg.aux.enable) {
      environment.systemPackages = with pkgs; [
        docker-slim
        nsjail
        skopeo
        nodePackages.dockerfile-language-server-nodejs
      ];
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.dockerfile-mode ];
      ide.emacs.core.config = readSubstituted ../../../subst.nix ./emacs/docker.el;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "t" ];
          cmd = "${pkgs.docker_containers_traits}/bin/docker_containers_traits";
          desktop = "shell";
          mode = "virt";
        }
        {
          key = [ "s" ];
          cmd = "${pkgs.docker_shell}/bin/docker_shell";
          desktop = "shell";
          mode = "virt";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [
          discover_containerized_services
          dlint
          docker_containers_traits
          docker_shell
          hadolintd
        ];
      };
    })
  ];
}
