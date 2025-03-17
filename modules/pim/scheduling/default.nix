{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.pim.scheduling;
  user = config.attributes.mainUser.name;
in
{
  options = {
    pim.scheduling = {
      enable = mkOption {
        type = types.bool;
        description = ''
          Whether to enable scheduled tasks, such as opening browser with links,
          starting applications or so.
        '';
        default = false;
      };
      entries = mkOption {
        type = types.attrs;
        example = {
          "read_mail" = {
            cal = "Mon,Tue *-*-01..04 12:00:00";
            cmd = "${config.attributes.browser.fallback} https://mail.google.com";
          };
        };
        default = { };
        description = ''
          Scheduled task entries.

          Timestamp for task issuing should be presented in systemd timers' OnCalendar entries format.
          Task definition is simple a shell command line to execute.
        '';
      };
      factoryCal.updateTimespec = mkOption {
        type = types.str;
        default = "";
        description = "Timestamp of `fcalendar-update` service activation (in systemd format).";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs scheduling-related setup";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ davfs2 gcalcli ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.config = builtins.readFile ./elisp/scheduling.el;
    })
  ];
}
