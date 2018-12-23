{ config, pkgs, ... }:
{
    imports = [
        ../private/traits/sys.nix
        ../../../toolbox/wm/xmonad.nix
    ];
    services = {
        xserver = {
            enable = true;
            videoDrivers = [ "modesetting" "intel" ];
            desktopManager = {
                xterm.enable = false;
                gnome3.enable = true;
                default = "none";
            };
            displayManager = {
                lightdm.enable = true;
                gdm.enable = false;
                # TODO: think of migrating to home-manager "xsession" module (beware that it is more versatile)
                sessionCommands = ''
                    ${pkgs.xlibs.xmodmap}/bin/xmodmap /etc/Xmodmaprc
                    ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "clear Lock"

                    export CURRENT_WM=${config.services.xserver.windowManager.default}
                    export TZ="${config.time.timeZone}"

                    ${pkgs.wmname}/bin/wmname LG3D

                    ${pkgs.xorg.xrdb}/bin/xrdb -merge .Xresources

                    ${pkgs.xidlehook}/bin/xidlehook --not-when-audio --not-when-fullscreen\
                          --timer normal 150 '${pkgs.libnotify}/bin/notify-send -t 7000 -u critical "Locking in 30 seconds"' ''' \
                          --timer primary 30 '${pkgs.lockscreen}/bin/lockscreen' ''' &
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
        };
        redshift = {
            enable = true;
            latitude = "${config.sys.redshift.latitude}";
            longitude = "${config.sys.redshift.longitude}";
            temperature.day = 5500;
            temperature.night = 3700;
        };
        arbtt.enable = true;
        autorandr = {
            enable = true;
            defaultTarget = "mobile";
        };
        compton = {
            enable = true;
            backend = "glx";
            vSync = "opengl-swc";
            opacityRules = [];
            package = pkgs.compton-git;
            extraOptions = ''
                inactive-dim 0.3
                focus-exclude '_NET_WM_NAME@:s = "rofi"'
            '';
        };
    };
    programs.light.enable = true;

    environment.systemPackages = with pkgs; [
        ckbcomp
        gmrun
        xlibs.xev
        xlibs.xprop
        xorg.xhost
        xorg.xmessage
    ];
    fonts = {
        fontconfig = {
            enable = true;
            useEmbeddedBitmaps = true;
        };
        enableFontDir = true;
        enableGhostscriptFonts = true;
        fonts = with pkgs; [
            # input-fonts # NOTE: no automated download, should prefetch manually
            anonymousPro
            corefonts
            dejavu_fonts
            dosemu_fonts
            emacs-all-the-icons-fonts
            emojione
            fantasque-sans-mono
            fira
            fira-code
            fira-mono
            font-awesome-ttf
            font-droid
            freefont_ttf
            go-font
            gohufont
            google-fonts
            hack-font
            inconsolata
            iosevka
            liberation_ttf
            mononoki
            mplus-outline-fonts
            noto-fonts
            noto-fonts-emoji
            powerline-fonts
            profont
            proggyfonts
            roboto
            roboto-mono
            roboto-slab
            source-code-pro
            terminus_font
            terminus_font_ttf
            ttf_bitstream_vera
            ubuntu_font_family
            unifont
            vistafonts
        ];
    };
}
