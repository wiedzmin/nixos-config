{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.networking.wireless;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    ext.networking.wireless = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable networking wireless support";
      };
      backend = mkOption {
        type = types.enum [ "networkmanager" "wireless" ];
        default = "networkmanager";
        description = "Which system module to use";
      };
      macAddress = mkOption {
        type = types.str; # TODO: search for specialized datatype
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
        default = true;
        description = "Whether to enable Bluetooth support";
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
          (readSubstituted ../../subst.nix ./scripts/wifi-status.sh);
      };

      boot.extraModprobeConfig = ''
        options iwlwifi 11n_disable=1 power_save=1 power_level=2
      '';

      networking.wlanInterfaces = optionalAttrs (cfg.ifacesMap != { }) cfg.ifacesMap;
    })
    (mkIf (cfg.enable && cfg.backend == "networkmanager") {
      users.users.${user}.extraGroups = [ "networkmanager" ];

      networking.networkmanager.wifi.macAddress = optionalString (cfg.macAddress != "") cfg.macAddress;
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
    })
    (mkIf (cfg.enable && cfg.tools.enable) {
      programs.wavemon.enable = true;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      programs.nm-applet.enable = if config.wm.i3.enable then true else false;

      home-manager.users."${user}" = {
        home.packages = with pkgs; optionals (cfg.backend == "wireless") [
          wpa_supplicant_gui
        ] ++ optionals (cfg.backend == "networkmanager" && cfg.wm.dmenu.enable) [
          nurpkgs.dmenu-ng
        ];
      };
      wmCommon.keys = optionals (cfg.backend == "networkmanager") [
        ({
          key = [ "Shift" "w" ];
          desktop = "shell";
          mode = "network";
        } // optionalAttrs (cfg.backend == "networkmanager") {
          cmd = "tmux new-window ${pkgs.networkmanager}/bin/nmtui";
        } // optionalAttrs (cfg.backend == "wireless") {
          cmd = "tmux new-window ${pkgs.wpa_supplicant}/bin/wpa_cli";
        })
        {
          key = [ "b" ];
          mode = "network";
          cmd = "${pkgs.blueman}/bin/blueman-manager";
        }
      ] ++ optionals (cfg.backend == "networkmanager" && cfg.wm.dmenu.enable) [{
        key = [ "w" ];
        cmd = ''${pkgs.runtimeShell} -l -c "exec ${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu"'';
        mode = "network";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ wifi-status ]; };
    })
  ];
}
