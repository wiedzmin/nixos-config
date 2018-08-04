{ config, pkgs, ... }:
{
    services.xserver = {
        videoDrivers = [ "modesetting" "intel" ];
        desktopManager = {
            gnome3.enable = true;
            default = "gnome3";
        };
        displayManager = {
            gdm = {
                enable = true;
                autoLogin.enable = true;
                autoLogin.user = "alex3rd"; # TODO: try to make a SPOT with users definitions
            };
            sessionCommands = ''
              ${pkgs.xlibs.xmodmap}/bin/xmodmap /etc/Xmodmaprc
            '';
        };
        windowManager.xmonad.enable = true;
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

    nixpkgs.config.dmenu.enableXft = true;

    environment.systemPackages = with pkgs; [
        arandr
        xlibs.xev
        xlibs.xmodmap
        xlibs.xprop
        xorg.xdpyinfo
        xorg.xhost

        gnome3.gnome-tweak-tool
    ];
}
