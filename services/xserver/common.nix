{ config, pkgs, ... }:
{
    services.xserver = {
        videoDrivers = [ "modesetting" "intel" ];
        desktopManager = {
            xterm.enable = false;
            gnome3.enable = true;
            default = "none";
        };
        displayManager = {
            lightdm.enable = true;
            gdm.enable = false;
            sessionCommands = ''
              ${pkgs.xlibs.xmodmap}/bin/xmodmap /etc/Xmodmaprc
              ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "clear Lock"

              export CURRENT_WM=${config.services.xserver.windowManager.default}
              export TZ="${config.time.timeZone}"

              ${pkgs.wmname}/bin/wmname LG3D

              source ~/common_settings

              ${pkgs.xorg.xrdb}/bin/xrdb -merge .Xresources
            '';
        };
        windowManager = {
            default = "xmonad";
            xmonad = {
                enable = true;
                enableContribAndExtras = true;
                extraPackages = p: [ p.taffybar p.dbus p.monad-logger p.lens p.split ];
            };
            #default = "stumpwm";
            #stumpwm.enable = true;
        };
        enable = true;
        xkbOptions = "caps:none";
        layout = "us,ru";
        libinput = {
            enable = true;
            naturalScrolling = true;
            disableWhileTyping = true;
            tapping = false;
            tappingDragLock = false;
            accelSpeed = "0.6";
        };
        xautolock = {
            enable = true;
            enableNotifier = true;
            time = 5;
            locker = "${pkgs.xkblayout-state}/bin/xkblayout-state set 0; ${pkgs.i3lock-color}/bin/i3lock-color -c 232729; ${pkgs.xorg.xset}/bin/xset dpms force off";
            nowlocker = "${pkgs.xkblayout-state}/bin/xkblayout-state set 0; ${pkgs.i3lock-color}/bin/i3lock-color -c 232729; ${pkgs.xorg.xset}/bin/xset dpms force off";
            notify = 20;
            notifier = "${pkgs.libnotify}/bin/notify-send -u critical \"Locking in 10 seconds\"";
            extraOptions = ["-detectsleep"];
        };
    };

    services.redshift = {
        enable = true;
        latitude = "55.751244"; # TODO: extract constant
        longitude = "37.618423"; # TODO: extract constant
        temperature.day = 5500;
        temperature.night = 3700;
    };

    services.arbtt.enable = true;

    services.autorandr = {
        enable = true;
        defaultTarget = "mobile";
    };

    services.compton = {
        enable = true;
        backend = "glx";
        vSync = "opengl-swc";
        opacityRules = [];
        package = pkgs.compton-git;
    };

    nixpkgs.config.dmenu.enableXft = true;

    environment.systemPackages = with pkgs; [
        arandr
        dmenu2
        edid-decode
        gmrun
        haskellPackages.xmobar
        haskellPackages.yeganesh
        i3lock-color
        taffybar
        xclip
        xdotool
        xlibs.xev
        xlibs.xmodmap
        xlibs.xprop
        xorg.xdpyinfo
        xorg.xhost
        xosd

        gnome3.gnome-tweak-tool
    ];
}
