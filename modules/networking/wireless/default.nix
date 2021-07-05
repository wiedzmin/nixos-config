{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.networking.wireless;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    ext.networking.wireless = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable networking wireless support";
      };
      backend = mkOption {
        type = types.enum [ "iwd" "wpa_supplicant" ];
        default = "iwd";
        description = "Which system module to use";
      };
      macAddress = mkOption {
        type = types.str;
        default = "";
        description = ''
          MAC address for wifi interface to use.

          Highly usable for hardware independence (ex.: laptop cages swapping).
        '';
      };
      ifacesMap = mkOption {
        type = types.attrs;
        default = { };
        description = "WLAN interface-to-device mapping";
      };
      wireless.driver = mkOption {
        type = types.str;
        default = "nl80211";
        description = "Driver name for `wireless` system module";
      };
      bluetooth.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Bluetooth support";
      };
      bluetooth.devices = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
        description = "Bluetooth devices metadata";
      };
      tools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable network monitoring/debug tools";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM tools/keybindings";
      };
      wm.dmenu.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable dmenu frontend(s)";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        wifi-status = mkShellScriptWithDeps "wifi-status" (with pkgs; [ gawk wirelesstools ])
          (builtins.readFile ./scripts/wifi-status.sh);
        headset_battery = mkPythonScriptWithDeps "headset_battery"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.redis bluetooth_battery ])
          (builtins.readFile ./scripts/headset_battery.py);
      };

      boot.extraModprobeConfig = ''
        options iwlwifi 11n_disable=1 power_save=1 power_level=2
      '';

      networking.wlanInterfaces = optionalAttrs (cfg.ifacesMap != { }) cfg.ifacesMap;
      networking.networkmanager.wifi.macAddress = optionalString (cfg.macAddress != "") cfg.macAddress;
    })
    (mkIf (cfg.enable && cfg.backend == "iwd") {
      networking.wireless.iwd.enable = true;
      networking.networkmanager.wifi.backend = "iwd";
    })
    (mkIf (cfg.enable && cfg.backend == "wireless") {
      networking.wireless = {
        enable = true;
        driver = cfg.wireless.driver;
        userControlled.enable = true;
        interfaces = builtins.attrNames cfg.ifacesMap;
      };
    })
    (mkIf (cfg.enable && cfg.bluetooth.enable) {
      hardware = {
        bluetooth = {
          enable = true;
          powerOnBoot = true;
          package = pkgs.bluezFull;
        };
      };
      services.blueman.enable = true;
      wmCommon.wsMapping.rules = [{
        class = ".*blueman-manager.*";
        scratchpad = true;
        key = [ "b" ];
      }];
      home-manager.users."${user}" = { home.packages = with pkgs; [ bluetooth_battery ]; };
      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set networking/wireless/headsets ${
          lib.strings.escapeNixString (builtins.toJSON (lib.forEach cfg.bluetooth.devices (d: d.mac)))
        }
      '';
      wmCommon.keys = [{
        key = [ "h" ];
        mode = "network";
        cmd = "${pkgs.headset_battery}/bin/headset_battery";
      }];
    })
    (mkIf (cfg.enable && cfg.tools.enable) { programs.wavemon.enable = true; })
    (mkIf (cfg.enable && cfg.wm.enable) {
      programs.nm-applet.enable = if (config.wm.i3.enable || !attributes.wms.enabled) then true else false;

      wmCommon.autostart.entries = optionals (cfg.bluetooth.enable) [ "blueman-manager" ];

      home-manager.users."${user}" = {
        home.packages = with pkgs;
          optionals (cfg.backend == "wireless") [ wpa_supplicant_gui ]
          ++ optionals (cfg.backend == "networkmanager" && cfg.wm.dmenu.enable) [ nurpkgs.dmenu-ng ];
      };
      wmCommon.keys = optionals (cfg.backend == "networkmanager") [
        ({
          key = [ "Shift" "w" ];
          desktop = "shell";
          mode = "network";
        } // optionalAttrs (cfg.backend == "networkmanager") {
          cmd = "tmux new-window ${pkgs.networkmanager}/bin/nmtui";
        } // optionalAttrs (cfg.backend == "wireless") { cmd = "tmux new-window ${pkgs.wpa_supplicant}/bin/wpa_cli"; })
      ] ++ optionals (cfg.backend == "networkmanager" && cfg.wm.dmenu.enable) [{
        key = [ "w" ];
        cmd = ''${pkgs.runtimeShell} -l -c "exec ${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu"'';
        mode = "network";
      }] ++ optionals (cfg.bluetooth.enable) [{
        key = [ "b" ];
        mode = "network";
        cmd = "${pkgs.blueman}/bin/blueman-manager";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ wifi-status headset_battery ]; };
    })
  ];
}
