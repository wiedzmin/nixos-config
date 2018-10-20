{ config, pkgs, ... }:
{
    services.xserver = {
        enable = true;
        videoDrivers = [ "modesetting" "intel" ];
        desktopManager = {
            xterm.enable = false;
            gnome3.enable = true;
            default = "none";
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
            notifier = "${pkgs.libnotify}/bin/notify-send -t 5000 -u critical \"Locking in 10 seconds\"";
            extraOptions = ["-detectsleep"];
        };
    };

    environment.etc."Xmodmap".source = ../../dotfiles/x11/xmodmaprc;

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
