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
              ${pkgs.autocutsel}/bin/autocutsel -fork -buttonup
              ${pkgs.autocutsel}/bin/autocutsel -fork -buttonup -selection PRIMARY

              ${pkgs.xlibs.xmodmap}/bin/xmodmap /etc/Xmodmaprc
              ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "clear Lock"

              export CURRENT_WM=${config.services.xserver.windowManager.default}
              export TZ="${config.time.timeZone}"

              ${pkgs.wmname}/bin/wmname LG3D

              source ~/common_settings

              ${pkgs.xorg.xrdb}/bin/xrdb -merge .Xresources
              arbtt-capture & # installed nondeclaratively, is broken otherwise
            '';
        };
        windowManager = {
            default = "xmonad";
            xmonad = {
                enable = true;
                enableContribAndExtras = true;
            };
        };
        enable = true;
        xkbOptions = "caps:none";
        layout = "us,ru";
        libinput = {
            enable = true;
            naturalScrolling = true;
            disableWhileTyping = true;
            tappingDragLock = false;
            accelSpeed = "0.6";
        };
    };

    services.autorandr = {
        enable = true;
        defaultTarget = "mobile";
    };

    nixpkgs.config.dmenu.enableXft = true;

    environment.systemPackages = with pkgs; [
        arandr
        dmenu2
        gmrun
        haskellPackages.xmobar
        haskellPackages.yeganesh
        i3lock-color
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
