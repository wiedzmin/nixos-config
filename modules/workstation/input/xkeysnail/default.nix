{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.workstation.input.xkeysnail;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    workstation.input.xkeysnail = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable xkeysnail";
      };
      configPath = mkOption {
        type = types.str;
        default = homePrefix ".config/xkeysnail/config.py";
        description = "Config file absolute path";
      };
      inputDevices = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = literalExample ''
          [
              "/dev/input/event3"
          ]
        '';
        description = ''
          Keyboard devices to control (if omitted,
          xkeysnail will choose proper keyboard
          devices)
        '';
      };
      rc = mkOption {
        type = types.lines;
        default = "";
        description = "xkeysnail customization";
      };
      setupText = mkOption {
        type = types.lines;
        default = ''
          # -*- coding: utf-8 -*-

          import re
          from xkeysnail.transform import *

          ${cfg.rc}
        '';
        visible = false;
        internal = true;
        readOnly = true;
        description = "xkeysnail final config.py contents";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = cfg.configPath != "";
        message = "XKeysnail: must provide path to config file";
      }];

      systemd.user.services."xkeysnail" = {
        description = "Xkeysnail";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          PIDFile = "/run/xkeysnail.pid";
          Restart = "always";
          RestartSec = 1;
          ExecStart = "/run/wrappers/bin/sudo ${nurpkgs.xkeysnail}/bin/xkeysnail ${
              optionalString (cfg.inputDevices != [ ])
              "--devices ${lib.concatStringsSep " " cfg.inputDevices}"
            } ${cfg.configPath}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      users.users.${user}.extraGroups = [ "input" ];
      home-manager.users.${user} = { xdg.configFile."xkeysnail/config.py".text = cfg.setupText; };
    })
  ];
}
