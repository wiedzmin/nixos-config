{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.dev.ccpp;
  user = config.attributes.mainUser.name;
in {
  options = {
    custom.dev.ccpp = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable C/C++ dev infrastructure.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs C/C++ setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ ccls clang clang-tools rr-unstable gdb ];
        programs.zsh.sessionVariables = {
          # FIXME: coerce to global waorkspaces roots infra
          _RR_TRACE_DIR = "/home/${user}/workspace/rr";
        };
      };
      boot.kernel.sysctl = {
        "kernel.perf_event_paranoid" = 1; # for rr
      };
      environment.etc."security/limits.conf".text = ''
        * hard  nofile    32768
        * soft  nofile    32768
      '';
      custom.dev.timeTracking.extensions = {
        "c" = "coding:c";
        "cpp" = "coding:c++";
        "h" = "coding:c";
        "hpp" = "coding:c++";
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.extraPackages = epkgs: [
        epkgs.ccls
        epkgs.cmake-font-lock
        epkgs.cmake-mode
        epkgs.modern-cpp-font-lock
      ];
      ide.emacs.config = readSubstituted ../subst.nix ./emacs/ccpp.el;
    })
  ];
}
