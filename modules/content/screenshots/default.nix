{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.content.screenshots;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  nixpkgs-last-unbroken = import inputs.nixpkgs-last-unbroken {
    config = config.nixpkgs.config // {
      allowUnfree = true;
      permittedInsecurePackages = config.ext.nix.core.permittedInsecurePackages;
    };
    localSystem = { system = "x86_64-linux"; };
  };
  inherit (config.wmCommon) prefix;
in
{
  options = {
    content.screenshots = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable screenshots management.";
      };
      baseDir = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Screenshots base directory";
      };
      ordering.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable screenshots ordering.";
      };
      ordering.timespec = mkOption {
        type = types.str;
        default = "";
        description = "Timestamp of service activation (in systemd format).";
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
      assertions = [{
        assertion = cfg.baseDir != null;
        message = "Must provide path to screenshots dir.";
      }];

      home-manager.users."${user}" = {
        services.flameshot = {
          enable = true;
          package = nixpkgs-last-unbroken.flameshot;
          settings = {
            General = {
              disabledTrayIcon = true;
              drawColor = "#ff0000";
              drawThickness = 2;
              filenamePattern = "screenshot-${config.attributes.dateFormats.flameshot}";
              savePath = cfg.baseDir;
            };
          };
        };
        xdg.configFile."toolbox/screenshots.json".text = builtins.toJSON {
          title = "screenshots";
          from = cfg.baseDir;
          to = "";
          unmatched = {
            subdir = "";
            skip = false;
          };
          rules = {
            "screenshot-(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})_[0-9]{2}:[0-9]{2}:[0-9]{2}\\.png" = "{{.year}}/{{.month}}/{{.day}}";
            "screenshot-(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})_[0-9]{2}-[0-9]{2}-[0-9]{2}\\.png" = "{{.year}}/{{.month}}/{{.day}}";
            "screenshot-(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})_[0-9]{2}-[0-9]{2}-[0-9]{2}_[0-9]{1,3}\\.png" = "{{.year}}/{{.month}}/{{.day}}";
            "(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})_[0-9]{2}:[0-9]{2}:[0-9]{2}_[0-9]+x[0-9]+_scrot\\.png" = "{{.year}}/{{.month}}/{{.day}}";
            "(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})-[0-9]{6}_[0-9]+x[0-9]+_scrot\\.png" = "{{.year}}/{{.month}}/{{.day}}";
            "(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})_[0-9]{2}-[0-9]{2}\\.png" = "{{.year}}/{{.month}}/{{.day}}";
            "screenshot-(?P<day>[0-9]{2})-(?P<month>[0-9]{2})-(?P<year>[0-9]{4})-[0-9]{2}:[0-9]{2}:[0-9]{2}\\.png" = "{{.year}}/{{.month}}/{{.day}}";
            "screenshot-[0-9]{2}:[0-9]{2}:[0-9]{2} (?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})\\.png" = "{{.year}}/{{.month}}/{{.day}}";
            "screenshot-[0-9]{2}:[0-9]{2}:[0-9]{2}_(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})\\.png" = "{{.year}}/{{.month}}/{{.day}}";
          };
        };
      };
      wmCommon.autostart.entries = [{ cmd = "flameshot"; }];
    })
    (mkIf (cfg.enable && cfg.ordering.enable) {
      systemd.user.services."order-screenshots" = {
        description = "Screenshots ordering";
        wantedBy = [ "graphical.target" ];
        partOf = [ "graphical.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${nurpkgs.toolbox}/bin/orderfiles --config ${xdgConfig user "/toolbox/screenshots.json"}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."order-screenshots" = renderTimer "Screenshots ordering" "" "" cfg.ordering.timespec false "";
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = [
        {
          key = [ "Print" ];
          cmd = "${pkgs.scrot}/bin/scrot --focused '${cfg.baseDir}/screenshot-%Y-%m-%d_%H-%M-%S.png'"; # NOTE: flameshot behaves strangely on kitty windows
          mode = "root";
        }
        {
          key = [ "Control" "Print" ];
          cmd = "${nixpkgs-last-unbroken.flameshot}/bin/flameshot full --path ${cfg.baseDir}";
          mode = "root";
        }
        {
          key = [ prefix "Print" ];
          cmd = "${nixpkgs-last-unbroken.flameshot}/bin/flameshot gui";
          mode = "root";
        }
      ];
    })
  ];
}
