{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.input.keyboard;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  xremapSettingsFormat = pkgs.formats.yaml { };
  xremapConfigFile =
    if cfg.xremap.yamlConfig == "" then
      xremapSettingsFormat.generate "config.yml" cfg.xremap.config
    else
      pkgs.writeTextFile {
        name = "xremap-config.yml";
        text = cfg.xremap.yamlConfig;
      };
  mkExecStartXremap =
    configFile:
    let
      mkDeviceString = x: "--device '${x}'";
    in
    builtins.concatStringsSep " " (
      lib.flatten (
        lib.lists.singleton "${lib.getExe cfg.xremap.package}"
        ++ (if cfg.xremap.deviceNames != null then map mkDeviceString cfg.xremap.deviceNames else [ ])
        ++ lib.optional cfg.xremap.mouse "--mouse"
        ++ cfg.xremap.extraArgs
        ++ lib.lists.singleton configFile
      )
    );
in
{
  options = {
    workstation.input.keyboard = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable various auxiliary tools
          for controlling mouse input
        '';
      };
      remappingTool = mkOption {
        # TODO: monitor "keymapper" evolution, whether it could be an additional option in the future
        type = types.enum [ "xkeysnail" "xremap" ];
        default = "xkeysnail";
        description = "Keyboard remapping tool to use";
      };
      xkeysnail.inputDevices = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = literalExpression ''
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
      xkeysnail.rc = mkOption {
        type = types.lines;
        default = "";
        description = "xkeysnail customizations";
      };
      xkeysnail.rcCommon = mkOption {
        type = types.lines;
        default = ''
          # Emacs-like keybindings in non-Emacs applications
          define_keymap(lambda wm_class: wm_class not in ("Emacs", "URxvt", "Alacritty", "Nyxt"), {
              # Cursor
              K("C-b"): with_mark(K("left")),
              K("C-f"): with_mark(K("right")),
              K("C-p"): with_mark(K("up")),
              K("C-n"): with_mark(K("down")),
              K("C-h"): with_mark(K("backspace")),
              # Forward/Backward word
              K("M-b"): with_mark(K("C-left")),
              K("M-f"): with_mark(K("C-right")),
              # Beginning/End of line
              K("C-a"): with_mark(K("home")),
              K("C-e"): with_mark(K("end")),
              # Page up/down
              K("M-v"): with_mark(K("page_up")),
              K("C-v"): with_mark(K("page_down")),
              # Beginning/End of file
              K("M-Shift-comma"): with_mark(K("C-home")),
              K("M-Shift-dot"): with_mark(K("C-end")),
              # Newline
              K("C-m"): K("enter"),
              K("C-j"): K("enter"),
              K("C-o"): [K("enter"), K("left")],
              # Copy
              K("C-w"): [K("C-x"), set_mark(False)],
              K("M-w"): [K("C-c"), set_mark(False)],
              K("C-y"): [K("C-v"), set_mark(False)],
              # Delete
              K("C-d"): [K("delete"), set_mark(False)],
              K("M-d"): [K("C-delete"), set_mark(False)],
              # Kill line
              K("C-k"): [K("Shift-end"), K("C-x"), set_mark(False)],
              # Undo
              K("C-slash"): [K("C-z"), set_mark(False)],
              K("C-Shift-ro"): K("C-z"),
              # Mark
              K("C-space"): set_mark(True),
              #K("C-M-space"): with_or_set_mark(K("C-right")),
              # Search
              K("C-s"): K("F3"),
              K("C-r"): K("Shift-F3"),
              K("M-Shift-key_5"): K("C-h"),
              # Cancel
              K("C-g"): [K("esc"), set_mark(False)],
              # Escape
              K("C-q"): escape_next_key,
              # C-x YYY
              K("C-x"): {
                  # C-x h (select all)
                  K("h"): [K("C-home"), K("C-a"), set_mark(True)],
                  # C-x C-f (open)
                  K("C-f"): K("C-o"),
                  # C-x C-s (save)
                  # K("C-s"): K("C-s"),
                  # C-x k (kill tab)
                  K("k"): K("C-f4"),
                  # C-x C-c (exit)
                  K("C-c"): K("C-q"),
                  # cancel
                  K("C-g"): pass_through_key,
                  # C-x u (undo)
                  K("u"): [K("C-z"), set_mark(False)],
              }
          }, "Emacs-like keys")
        '';
        description = "common xkeysnail customizations";
      };
      xkeysnail.setupText = mkOption {
        type = types.lines;
        default = ''
          # -*- coding: utf-8 -*-

          import re
          from xkeysnail.transform import *

          ${cfg.xkeysnail.rc}
          ${cfg.xkeysnail.rcCommon}
        '';
        visible = false;
        internal = true;
        readOnly = true;
        description = "xkeysnail final config.py contents";
      };
      # FIXME: Recently, modes were added to modmap, which makes recent `xremap` versions work
      # weird/unpredictably with current config. It should be fixed in some way to be able to
      # bump version further.
      # See https://github.com/xremap/xremap/commit/6e7314b48d4f070fd345dcd1bf86a832fa2d5fe8 for details.
      xremap.package = mkOption {
        type = types.package;
        default = nurpkgs.xremap;
        description = "XRemap package to install";
      };
      xremap.config = mkOption {
        type = types.submodule { freeformType = xremapSettingsFormat.type; };
        default = { };
        description = ''
          Xremap configuration.

          See xremap repo for examples.

          Cannot be used together with .yamlConfig
        '';
      };
      xremap.yamlConfig = mkOption {
        type = types.str;
        default = "";
        description = ''
          The text of yaml config file for xremap. See xremap repo for examples. Cannot be used together with .config.
        '';
      };
      xremap.deviceNames = mkOption {
        type = with types; nullOr (listOf nonEmptyStr);
        default = null;
        description = "List of devices to remap.";
      };
      xremap.watch = mkEnableOption "running xremap watching new devices";
      xremap.mouse = mkEnableOption "watching mice by default";
      xremap.extraArgs = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = [ "--completions zsh" ];
        description = "Extra arguments for xremap";
      };
      xremap.debug = mkEnableOption "run xremap with RUST_LOG=debug in case upstream needs logs";
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      hardware.uinput.enable = true;
      services.udev.extraRules = ''
        KERNEL=="uinput", GROUP="input", TAG+="uaccess"
      '';
      users.users."${user}".extraGroups = [ "input" "uinput" ];
    })
    (mkIf (cfg.enable && cfg.remappingTool == "xkeysnail") {
      systemd.user.services."xkeysnail" = {
        description = "Xkeysnail";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          PIDFile = "/run/xkeysnail.pid";
          Restart = "always";
          RestartSec = 1;
          ExecStart = "/run/wrappers/bin/sudo ${pkgs.xkeysnail}/bin/xkeysnail ${
              optionalString (cfg.xkeysnail.inputDevices != [ ])
              "--devices ${lib.concatStringsSep " " cfg.xkeysnail.inputDevices}"
            } ${homePrefix user ".config/xkeysnail/config.py"}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      home-manager.users."${user}" = { xdg.configFile."xkeysnail/config.py".text = cfg.xkeysnail.setupText; };
    })
    (mkIf (cfg.enable && cfg.remappingTool == "xremap") {
      assertions = [
        {
          assertion = (cfg.xremap.yamlConfig == "" && cfg.xremap.config != { }) || (cfg.xremap.yamlConfig != "" && cfg.xremap.config == { });
          message = "input/keyboard/XRemap: config needs to be specified either in .yamlConfig or in .config";
        }
      ];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ nurpkgs.xremap ];
      };

      systemd.user.services.xremap = {
        description = "XRemap user service";
        path = [ cfg.xremap.package ];
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          KeyringMode = "private";
          SystemCallArchitectures = [ "native" ];
          RestrictRealtime = true;
          ProtectSystem = true;
          SystemCallFilter = map (x: "~@${x}") [
            "clock"
            "debug"
            "module"
            "reboot"
            "swap"
            "cpu-emulation"
            "obsolete"
            # NOTE: These two make the spawned processes drop cores
            # "privileged"
            # "resources"
          ];
          LockPersonality = true;
          UMask = "077";
          RestrictAddressFamilies = "AF_UNIX";
          Environment = [ "DISPLAY=:0" ] ++ optionals cfg.xremap.debug [ "RUST_LOG=debug" ];
          ExecStart = "${mkExecStartXremap xremapConfigFile}";
        };
      };
    })
  ];
}
