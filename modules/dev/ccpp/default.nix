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
      markers.root.enable = mkOption {
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
        home.packages = with pkgs; [ gdb rr ccls neocmakelsp ];
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
    })
    (mkIf (cfg.enable && cfg.markers.root.enable) {
      dev.navigation.projects.markers.root = [ "CMakeLists.txt" "Makefile" ];
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.cmake-font-lock
        epkgs.cmake-mode
        epkgs.modern-cpp-font-lock
      ] ++ optionals (!config.ide.emacs.core.treesitter.enable) [
        epkgs.ccls
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/common.el ] +
        optionalString (!config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/non-ts.el ]) +
        optionalString (config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/ts.el ]);
      ide.emacs.completion.tempel.snippets = ''
        c-mode

        (fixme "// FIXME: ")
        (todo "// TODO: ")
        (bug "// BUG: ")
        (hack "// HACK: ")
        (note "// NOTE: ")
        (fixmep "// FIXME(${user}): ")
        (todop "// TODO(${user}): ")
        (bugp "// BUG(${user}): ")
        (hackp "// HACK(${user}): ")
        (notep "// NOTE(${user}): ")
      '';
      ide.emacs.core.treesitter.grammars = {
        c = "https://github.com/tree-sitter/tree-sitter-c";
        cmake = "https://github.com/uyha/tree-sitter-cmake";
        cpp = "https://github.com/tree-sitter/tree-sitter-cpp";
      };
      ide.emacs.core.treesitter.modeRemappings = {
        "c++-mode" = "c++-ts-mode";
        "c-mode" = "c-ts-mode";
        "c-or-c++-mode" = "c-or-c++-ts-mode";
      };
    })
  ];
}
