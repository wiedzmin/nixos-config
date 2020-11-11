{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.custom.video;
  prefix = config.wmCommon.prefix;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  kill-compton = pkgs.writeScriptBin "kill-compton" ''
    ${pkgs.procps}/bin/pkill -f compton
  '';
in {
  options = {
    custom.video = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable video customizations.";
      };
      rate = mkOption {
        type = types.str;
        default = "60.00";
        description = ''
          Refresh rate for XRandR heads.
        '';
      };
      gamma = mkOption {
        type = types.str;
        default = "1.0:0.909:0.833";
        description = "XRandR gamma settings.";
      };
      backlightDelta = mkOption {
        type = types.int;
        default = 10;
        description = "Backlight delta percents.";
      };
      rotateSecondaryHead = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to rotate secondary head left (monitor place vertically)";
      };
      rotation = mkOption {
        type = types.enum [ "normal" "left" "right" "inverted" ];
        default = "left";
        description = "Secondary head rotation way";
      };
      ddc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable talking to monitors using DDC.";
      };
      opengl.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable OpenGL";
      };
      autorandr.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable autorandr.";
      };
      autorandr.profilesPath = mkOption {
        type = types.str;
        default = assetsPrefix "xrandr";
        description = "Base path for autorandr profiles.";
      };
      autorandr.profiles = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Autorandr profiles list.";
      };
      autorandr.hooks = mkOption {
        type = types.attrs;
        default = { };
        description = "Autorandr hooks.";
      };
      screenlocker.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable automatic screen locking.";
      };
      screenlocker.respectPlayback = mkOption {
        type = types.bool;
        default = true;
        description = "Do not lock, while playing media.";
      };
      screenlocker.respectFullscreen = mkOption {
        type = types.bool;
        default = true;
        description = "Do not lock, when active window is fullscreen.";
      };
      screenlocker.notificationUrgency = mkOption {
        type = types.enum [ "normal" "critical" ];
        default = "critical";
        description = "Notification urgency level.";
      };
      screenlocker.notificationTimeout = mkOption {
        type = types.int;
        default = 7000;
        description = "Notification timeout.";
      };
      screenlocker.alertingTimerSec = mkOption {
        type = types.int;
        default = 150;
        description = "Seconds of idle time, before notification fires.";
      };
      screenlocker.lockingTimerSec = mkOption {
        type = types.int;
        default = 30;
        description = "Seconds of idle time between alert and locking.";
      };
      scripts.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom scripts.";
      };
      scripts.debug = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable debug tools.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM bindings.";
      };
      debug.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable debug tools.";
      };
      staging.packages = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "list of staging packages.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        xctl = mkPythonScriptWithDeps "xctl" (with pkgs; [ autorandr nurpkgs.pystdlib python3Packages.ewmh ])
          (readSubstituted ../subst.nix ./scripts/xctl.py);
        rescreen = mkShellScriptWithDeps "rescreen" (with pkgs; [ autorandr ]) ''
          rescreen-$(autorandr --detected)-i3
        '';
      };

      users.users.${user}.extraGroups = [ "video" ];
      programs.light.enable = true;
      hardware.brillo.enable = true;
      home-manager.users.${user} = {
        home.packages = with pkgs; lib.optionals (cfg.staging.packages != [ ]) cfg.staging.packages;
        home.file = {
          ".XCompose".text = ''
            include "${pkgs.xorg.libX11}/share/X11/locale/en_US.UTF-8/Compose"

            <Multi_key> <m> <o> : "ө"
            <Multi_key> <m> <O> : "Ө"
            <Multi_key> <m> <u> : "ү"
            <Multi_key> <m> <U> : "Ү"
          '';
        };
        services = {
          picom = {
            enable = true;
            backend = "glx";
            vSync = true;
            package = pkgs.picom;
            extraOptions = ''
              glx-no-rebind-pixmap = true;
              glx-no-stencil = true;
              xrender-sync-fence = true;
            '';
          };
          redshift = {
            enable = true;
            latitude = config.identity.secrets.redshiftLatitude;
            longitude = config.identity.secrets.redshiftLongitude;
            temperature.day = 5500;
            temperature.night = 3100;
            brightness.day = "1.0";
            brightness.night = "0.7";
            extraOptions = [ "-v" "-m randr" ];
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.opengl.enable) {
      hardware.opengl = {
        enable = true;
        extraPackages = with pkgs; [ intel-media-driver libvdpau-va-gl vaapiIntel vaapiVdpau ];
        driSupport32Bit = true;
        extraPackages32 = with pkgs.pkgsi686Linux; [ libvdpau-va-gl vaapiIntel vaapiVdpau ];
      };
      environment.sessionVariables.LIBVA_DRIVER_NAME = "iHD";
    })
    (mkIf (cfg.enable && cfg.ddc.enable) {
      security.wrappers = { ddcutil = { source = "${pkgs.ddcutil}/bin/ddcutil"; }; };
      boot = {
        kernelModules = [ "i2c-dev" "ddcci" "ddcci-backlight" ];
        extraModulePackages = with config.boot.kernelPackages; [ ddcci-driver ];
        extraModprobeConfig = ''
          options ddcci autoprobe_addrs=1
        '';
      };
      services.udev.extraRules = ''
        SUBSYSTEM=="i2c-dev", GROUP=${config.attributes.localGroup}, MODE="0660"
        ACTION=="add", KERNEL=="i2c-[0-9]*", GROUP="wheel", MODE="666"
      '';
    })
    (mkIf (cfg.enable && cfg.autorandr.enable) {
      home-manager.users.${user} = {
        programs.autorandr = {
          enable = true;
          hooks =
            lib.optionalAttrs (hm.services.compton.enable) {
              predetect = { "kill-compton" = "${kill-compton}/bin/kill-compton"; };
            } // cfg.autorandr.hooks;
        };
      };
      services.udev.extraRules = ''
        ACTION=="change", SUBSYSTEM=="drm", RUN+="${pkgs.autorandr}/bin/autorandr --batch --change --default default"
      '';
    })
    (mkIf (cfg.enable && cfg.screenlocker.enable) {
      systemd.user.services."screenlocker" = {
        description = "Lock the screen automatically after a timeout";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        path = [ pkgs.bash ];
        serviceConfig = {
          Type = "simple";
          ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
          ExecStart = ''
            ${pkgs.xidlehook}/bin/xidlehook \
                  ${optionalString cfg.screenlocker.respectPlayback "--not-when-audio"} \
                  ${optionalString cfg.screenlocker.respectFullscreen "--not-when-fullscreen"} \
                  --timer ${builtins.toString cfg.screenlocker.alertingTimerSec} "${pkgs.dunst}/bin/dunstify \
                          -t ${builtins.toString cfg.screenlocker.notificationTimeout} \
                          -u ${cfg.screenlocker.notificationUrgency} \
                          'Locking in ${builtins.toString cfg.screenlocker.lockingTimerSec} seconds'" "" \
                  --timer ${builtins.toString cfg.screenlocker.lockingTimerSec} \
                          "${config.custom.security.lockScreenCommand}" ""
          '';
        };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "XF86MonBrightnessDown" ];
          cmd = "${pkgs.light}/bin/light -U ${toString cfg.backlightDelta}";
          mode = "root";
        }
        {
          key = [ "XF86MonBrightnessUp" ];
          cmd = "${pkgs.light}/bin/light -A ${toString cfg.backlightDelta}";
          mode = "root";
        }
        {
          key = [ "Control" "XF86MonBrightnessDown" ];
          cmd = "${pkgs.light}/bin/light -S 20";
          mode = "root";
        }
        {
          key = [ "Control" "XF86MonBrightnessUp" ];
          cmd = "${pkgs.light}/bin/light -S 100";
          mode = "root";
        }
        {
          key = [ "m" ];
          cmd = "${pkgs.autorandr}/bin/autorandr --load mobile";
          mode = "xserver";
        }
        {
          key = [ "a" ];
          cmd = "${pkgs.xctl}/bin/xctl --switch";
          mode = "xserver";
        }
        {
          key = [ "r" ];
          cmd = "${pkgs.rescreen}/bin/rescreen";
          mode = "xserver";
        }
        {
          key = [ "c" ];
          cmd = "${pkgs.find-cursor}/bin/find-cursor";
          mode = "xserver";
        }
      ];
    })
    (mkIf (cfg.enable && cfg.debug.enable) {
      environment.systemPackages = with pkgs; [ xlibs.xev xlibs.xprop xorg.xkbcomp drm_info xtruss ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ rescreen xctl ]; };
    })
  ];
}
