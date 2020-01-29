{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.video;
  autorandr_profiles = pkgs.writeScriptBin "autorandr_profiles" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i python3 -p python3 python3Packages.dmenu-python
    import os
    import dmenu

    profiles = []


    for root, dirs, files in os.walk("/home/${config.attributes.mainUser.name}/.config/autorandr"):
        for dir in dirs:
            if not dir.endswith(".d"):
                profiles.append(dir)

    result = dmenu.show(profiles, prompt='profile', lines=5)
    if result:
        print(result)
        os.system("${pkgs.autorandr}/bin/autorandr --load {0}".format(result))
  '';
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
      autorandr.hooks = mkOption {
        type = types.attrs;
        default = {};
        description = "Autorandr hooks.";
      };
      autorandr.profiles = mkOption {
        type = types.attrs;
        description = "Autorandr profiles.";
        default = { # TODO: think if there should be a default value
          "mobile" = {
            fingerprint = {
              "${config.attributes.hardware.monitors.internalHead.name}" =
                config.attributes.hardware.monitors.internalHead.edid;
            };
            config = {
              "${config.attributes.hardware.monitors.internalHead.name}" = {
                enable = true;
                primary = true;
                position = "0x0";
                mode = config.attributes.hardware.monitors.internalHead.resolution;
                gamma = cfg.gamma;
                rate = cfg.rate;
              };
            };
          };
          "docked-home" = {
            fingerprint = {
              "HDMI-2" =
                "00ffffffffffff001e6dbc594f53010006170103803c2278ea3135a5554ea1260c5054a54b00714f81809500b300a9c0810081c09040023a801871382d40582c450056512100001e000000fd00384b1e530f000a202020202020000000fc003237454133330a202020202020000000ff0033303652414e4e324a3836330a00dd";
              "HDMI-3" =
                "00ffffffffffff000469b124010101011d18010380372378ea3d15a3544da027125054bfef00714f818081409500a940b300d1c00101283c80a070b023403020360022602100001a000000fd00324c1e5311000a202020202020000000fc0050413234380a20202020202020000000ff0045374c4d51533037373132380a0023";
              "${config.attributes.hardware.monitors.internalHead.name}" =
                config.attributes.hardware.monitors.internalHead.edid;
            };
            config = {
              "HDMI-2" = {
                enable = true;
                position = "0x0";
                mode = "1920x1080";
                gamma = cfg.gamma;
                rate = cfg.rate;
              };
              "HDMI-3" = {
                enable = true;
                position = "1366x1080";
                mode = "1920x1080";
                gamma = cfg.gamma;
                rate = cfg.rate;
                rotate = "left";
              };
              "${config.attributes.hardware.monitors.internalHead.name}" = {
                enable = true;
                primary = true;
                position = "0x1080";
                mode = config.attributes.hardware.monitors.internalHead.resolution;
                gamma = cfg.gamma;
                rate = cfg.rate;
              };
            };
          };
          "undocked-parents-dsub" = {
            fingerprint = {
              "VGA-1" =
                "00ffffffffffff004c2d0e0139314a4d100f01036c261e782aee95a3544c99260f5054bfef808180714f010101010101010101010101302a009851002a4030701300782d1100001e000000fd00384b1e510e000a202020202020000000fc0053796e634d61737465720a2020000000ff00485348593430323338330a202000d2";
              "${config.attributes.hardware.monitors.internalHead.name}" =
                config.attributes.hardware.monitors.internalHead.edid;
            };
            config = {
              "VGA-1" = {
                enable = true;
                position = "0x0";
                mode = "1280x1024";
                gamma = cfg.gamma;
                rate = cfg.rate;
              };
              "${config.attributes.hardware.monitors.internalHead.name}" = {
                enable = true;
                primary = true;
                position = "0x1024";
                mode = config.attributes.hardware.monitors.internalHead.resolution;
                gamma = cfg.gamma;
                rate = cfg.rate;
              };
            };
          };
        };
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
        type = types.str;
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
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad bindings.";
      };
      debug.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable debug tools.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      users.users."${config.attributes.mainUser.name}".extraGroups = [ "video" ];
      programs.light.enable = true;
      hardware.brillo.enable = true;
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; lib.optionals (config.attributes.staging.enable) [
          blugon
        ];
        home.file = {
          ".XCompose".text = ''
            include "${pkgs.xorg.libX11}/share/X11/locale/en_EN.UTF-8/Compose"

            <Multi_key> <m> <o> : "ө"
            <Multi_key> <m> <O> : "Ө"
            <Multi_key> <m> <u> : "ү"
            <Multi_key> <m> <U> : "Ү"
          '';
        };
        # TODO: consider rework/restructure
        services = {
          compton = {
            enable = true;
            backend = "glx";
            vSync = "opengl-swc";
            package = pkgs.compton-git;
            extraOptions = ''
              clear-shadow = true;
              glx-no-rebind-pixmap = true;
              glx-no-stencil = true;
              paint-on-overlay = true;
              xrender-sync-fence = true;
            '';
          };
          redshift = {
            enable = true;
            latitude = config.secrets.identity.redshiftLatitude;
            longitude = config.secrets.identity.redshiftLongitude;
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
      security.wrappers = {
        ddcutil = { source = "${pkgs.ddcutil}/bin/ddcutil"; };
      };
      boot = {
        kernelModules = [
          "i2c-dev"
          "ddcci"
          "ddcci-backlight"
        ];
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
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          autorandr_profiles
        ];
        programs.autorandr = {
          enable = true;
          hooks = lib.optionalAttrs
            (config.home-manager.users."${config.attributes.mainUser.name}".services.compton.enable) {
              predetect = { "kill-compton" = "${kill-compton}/bin/kill-compton"; };
          } // cfg.autorandr.hooks;
          profiles = cfg.autorandr.profiles;
        };
      };
      services.udev.extraRules = ''
        ACTION=="change", SUBSYSTEM=="drm", RUN+="${pkgs.autorandr}/bin/autorandr --batch --change --default default"
      '';
    })
    (mkIf (cfg.enable && cfg.screenlocker.enable) {
      systemd.user.services."xidlehook" = {
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
                          "${pkgs.i3lock-color}/bin/i3lock-color -c 232729 && ${pkgs.xorg.xset}/bin/xset dpms force off" ""
          '';
        };
      };
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "<XF86MonBrightnessUp>" = ''spawn "${pkgs.light}/bin/light -A ${toString cfg.backlightDelta}"'';
        "<XF86MonBrightnessDown>" = ''spawn "${pkgs.light}/bin/light -U ${toString cfg.backlightDelta}"'';
        "C-<XF86MonBrightnessUp>" = ''spawn "${pkgs.light}/bin/light -S 100"'';
        "C-<XF86MonBrightnessDown>" = ''spawn "${pkgs.light}/bin/light -S 20"'';
        "M-C-a" = ''spawn "${autorandr_profiles}/bin/autorandr_profiles"'';
        "M-M1-x" = ''spawn "${pkgs.autorandr}/bin/autorandr --load mobile"'';
        "M-a c" = ''spawn "${pkgs.systemd}/bin/systemctl --user restart compton.service"'';
       };
    })
    (mkIf (cfg.enable && cfg.debug.enable) {
      environment.systemPackages = with pkgs; [
        xlibs.xev
        xlibs.xprop
        xorg.xkbcomp
        drm_info
        xtruss
      ];
    })
  ];
}
