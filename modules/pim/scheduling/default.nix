{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.pim.scheduling;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
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
      nixpkgs.config.packageOverrides = _: {
        fcalendar = mkPythonScriptWithDeps pkgs "fcalendar"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.redis python3Packages.requests ])
          (builtins.readFile ./scripts/fcalendar.py);
      };
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ davfs2 gcalcli ];
        home.activation.ensureSchedulingTimers = {
          after = [ ];
          before = [ "checkLinkTargets" ];
          data = ''
            export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/${config.attributes.mainUser.ID}/bus
            ${lib.concatStringsSep "\n"
            (lib.mapAttrsToList (name: _: "${pkgs.systemd}/bin/systemctl --user restart ${name}.timer") cfg.entries)}
          '';
        };
      };
      systemd.user.services = (lib.mapAttrs
        (name: meta: {
          description = "${name}";
          serviceConfig =
            let forWork = builtins.hasAttr "forWork" meta && meta.forWork;
            in {
              Type = "oneshot";
              Environment = [ "DISPLAY=:0" ];
              ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
              ExecStart = optionalString forWork ''${pkgs.fcalendar}/bin/fcalendar check --cmd "'' + "${meta.cmd}"
                + optionalString forWork ''"'';
              StandardOutput = "journal";
              StandardError = "journal";
            };
        })
        cfg.entries) // {
        "fcalendar-update" = {
          description = "Update factory calendar";
          serviceConfig = {
            Type = "oneshot";
            Environment = [ "DISPLAY=:0" ];
            ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
            ExecStart = "${pkgs.fcalendar}/bin/fcalendar update";
            StandardOutput = "journal";
            StandardError = "journal";
          };
        };
      };
      systemd.user.timers = (lib.mapAttrs
        (name: meta: {
          description = "${name}";
          wantedBy = [ "timers.target" ];
          timerConfig = { OnCalendar = meta.cal; };
        })
        cfg.entries) // {
        "fcalendar-update" = {
          description = "Update factory calendar";
          wantedBy = [ "timers.target" ];
          timerConfig = { OnCalendar = cfg.factoryCal.updateTimespec; };
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.config = builtins.readFile ./elisp/scheduling.el;
    })
    (mkIf (cfg.enable && config.attributes.debug.exposeScripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ fcalendar ]; };
    })
  ];
}
