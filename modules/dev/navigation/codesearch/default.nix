{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.codesearch;
  user = config.attributes.mainUser.name;
in {
  options = {
    dev.codesearch = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Codesearch dev infra.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ codesearch ];
        programs = {
          zsh.sessionVariables = { CSEARCHINDEX = "${homePrefix config.custom.navigation.workspaceRootGlobal}/.csearchindex"; };
          bash.sessionVariables = { CSEARCHINDEX = "${homePrefix config.custom.navigation.workspaceRootGlobal}/.csearchindex"; };
        };
      };
      systemd.user.services."codesearch-reindex" = {
        description = "Codesearch index updating";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          Environment = [ "CSEARCHINDEX=${homePrefix config.custom.navigation.workspaceRootGlobal}/.csearchindex" ];
          ExecStart = "${pkgs.codesearch}/bin/cindex ${homePrefix config.custom.navigation.workspaceRootGlobal}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."codesearch-reindex" = renderTimer "Codesearch index updating" "" "" "*-*-* 4:00:00";
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.codesearch epkgs.counsel-codesearch epkgs.projectile-codesearch ];
      ide.emacs.core.config = readSubstituted ../../../subst.nix ./emacs/codesearch.el;
    })
  ];
}
