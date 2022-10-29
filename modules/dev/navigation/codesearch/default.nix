{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.codesearch;
  user = config.attributes.mainUser.name;
in
{
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
      shell.core.variables = [{
        CSEARCHINDEX = "${config.navigation.bookmarks.workspaces.globalRoot}/.csearchindex";
      }];
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ codesearch ];
      };
      systemd.user.services."codesearch-reindex" = {
        description = "Codesearch index updating";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          Environment =
            [ "CSEARCHINDEX=${config.navigation.bookmarks.workspaces.globalRoot}/.csearchindex" ];
          ExecStart = "${pkgs.codesearch}/bin/cindex ${config.navigation.bookmarks.workspaces.globalRoot}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."codesearch-reindex" = renderTimer "Codesearch index updating" "" "" "*-*-* 4:00:00" false "";
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.codesearch ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ../../subst.nix ] [ ./elisp/codesearch.el ];
    })
  ];
}
