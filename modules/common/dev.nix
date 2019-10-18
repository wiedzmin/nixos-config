{ config, lib, pkgs, ... }:
with lib;

let cfg = config.dev.tools;
in {
  options = {
    dev.tools = {
      playground.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable non-production tools to play with.";
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
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''Whether to enable various misc tools.'';
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
        ];
      };
    })
    (mkIf cfg.patching.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          patchutils
          wiggle
        ];
      };
    })
    (mkIf cfg.statistics.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          cloc
          gource
          sloccount
          tokei
        ];
      };
    })
    (mkIf cfg.misc.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          watchexec
          wstunnel
          pv
          loop
          ix
        ];
      };
    })
  ];
}
