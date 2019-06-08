{ config, pkgs, ... }:
with import ../../const.nix {inherit config pkgs;};
{
    imports = [
        ./wm/xmonad.nix
    ];
    services = {
        xserver = {
            enable = true;
            videoDrivers = [ "modesetting" ];
            desktopManager = {
                xterm.enable = false;
                gnome3.enable = false;
                default = "none";
            };
            displayManager = {
                lightdm = {
                    enable = true;
                    background = "black";
                    greeters.mini = {
                        enable = true;
                        user = userName;
                    };
                };
                gdm.enable = false;
                sessionCommands = ''
                    ${pkgs.xlibs.xmodmap}/bin/xmodmap /etc/Xmodmaprc
                    ${pkgs.xlibs.xmodmap}/bin/xmodmap -e "clear Lock"

                    export CURRENT_WM=${config.services.xserver.windowManager.default}
                    export TZ="${config.time.timeZone}"
                    export _JAVA_AWT_WM_NONREPARENTING=1

                    ${pkgs.wmname}/bin/wmname LG3D

                    ${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources

                    ${pkgs.xpointerbarrier}/bin/xpointerbarrier 25 0 0 0 &

                    ${pkgs.xidlehook}/bin/xidlehook --not-when-audio --not-when-fullscreen\
                          --timer normal 150 '${pkgs.dunst}/bin/dunstify -t 7000 -u critical "Locking in 30 seconds"' ''' \
                          --timer primary 30 '${pkgs.i3lock-color}/bin/i3lock-color -c 232729 && ${pkgs.xorg.xset}/bin/xset dpms force off' ''' &
                '';
            };
            xkbOptions = "caps:none";
            layout = "us,ru";
            libinput.enable = false;
            multitouch = {
                enable = true;
                invertScroll = true;
                ignorePalm = true;
                tapButtons = false;
                additionalOptions = ''
                    # Option        "ScrollCoastDuration" "500"
                    # Option        "ScrollCoastEnableSpeed" ".3"
                    Option        "ButtonIntegrated" "true"
                    Option        "ButtonMoveEmulate" "false"
                    Option        "ClickTime" "25"
                    Option        "EdgeBottomSize" "5"
                    Option        "FingerHigh" "5"
                    Option        "FingerLow" "1"
                    Option        "Hold1Move1StationaryMaxMove" "1000"
                    Option        "IgnoreThumb" "true"
                    Option        "ScrollCoastDuration" "600"
                    Option        "ScrollCoastEnableSpeed" "0.05"
                    Option        "ScrollDistance" "100"
                    Option        "ScrollSensitivity" "0"
                    Option        "Sensitivity" "0.3"
                    Option        "SwipeDistance" "700"
                    Option        "SwipeDownButton" "0"
                    Option        "SwipeLeftButton" "8"
                    Option        "SwipeRightButton" "9"
                    Option        "SwipeUpButton" "0"
                    Option        "TapButton4" "0"
                    Option        "ThumbRatio" "70"
                    Option        "ThumbSize" "25"
                '';
            };
        };
        arbtt.enable = true;
        autorandr = {
            enable = true;
            defaultTarget = "mobile";
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
            anonymousPro
            corefonts
            dejavu_fonts
            dosemu_fonts
            emacs-all-the-icons-fonts
            fantasque-sans-mono
            fira-code
            fira-mono
            font-awesome-ttf
            go-font
            hack-font
            inconsolata
            iosevka
            mononoki
            powerline-fonts
            profont
            roboto-mono
            source-code-pro
            terminus_font
            terminus_font_ttf
            unifont
        ];
    };
}
