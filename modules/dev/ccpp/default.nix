{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.dev.ccpp;
  user = config.attributes.mainUser.name;
in
{
  options = {
    dev.ccpp = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable C/C++ dev infrastructure.";
      };
      rootMarkers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use additional project root' marker files";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs C/C++ setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      shell.core.variables = [{ _RR_TRACE_DIR = "${homePrefix user config.navigation.bookmarks.workspaces.globalRoot}/rr"; }];
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ ccls clang clang-tools gdb ]; # rr-unstable (broken)
      };
      boot.kernel.sysctl = {
        "kernel.perf_event_paranoid" = 1; # for rr
      };
      environment.etc."security/limits.conf".text = ''
        * hard  nofile    32768
        * soft  nofile    32768
      '';
      dev.misc.timeTracking.extensions.dev = {
        "c" = "coding:c";
        "cpp" = "coding:cpp";
        "h" = "coding:c";
        "hpp" = "coding:cpp";
      };
      dev.projectenv.templates.entries = {
        "ccpp.generic" = configPrefix roots "modules/dev/ccpp/templates/generic";
        "ccpp.cmake" = configPrefix roots "modules/dev/ccpp/templates/cmake";
      };
    })
    (mkIf (cfg.enable && cfg.rootMarkers.enable) {
      dev.navigation.projects.rootMarkers = [ "CMakeLists.txt" "Makefile" ];
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.ccls
        epkgs.cmake-font-lock
        epkgs.cmake-mode
        epkgs.modern-cpp-font-lock
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/ccpp.el ];
    })
  ];
}
