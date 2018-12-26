{config, pkgs, lib, ...}:

let
    urxvtUrlLauncher = "${pkgs.xdg_utils}/bin/xdg-open";
    urxvtPerlExtensions = [
        "clipboard"
        "default"
        "font-size"
        "fullscreen"
        "keyboard-select"
        "matcher"
        "selection"
        "url-select"
    ];
in
{
    # TODO: search for existing solution
    system.activationScripts.updateXresources = ''
        ${pkgs.xorg.xrdb}/bin/xrdb -merge ${config.users.extraUsers.alex3rd.home}/.Xresources
    '';

    home-manager.users.alex3rd = {
        xresources.properties = {
            "Xmessage*Buttons" = "Quit";
            "Xmessage*defaultButton" = "Quit";
            "Xmessage*faceName" = "${config.sys.fonts.main.name}";
            "Xmessage*faceSize" = "${config.sys.fonts.size.Xmessage}";
            "Xmessage*faceWeight" = "${config.sys.fonts.main.weight}";
            "Xmessage*international" = true;

            "dzen2.font" = "${config.sys.fonts.main.name}:weight=${config.sys.fonts.main.weight}:size=${config.sys.fonts.size.Dzen}";

            "Emacs*XlwMenu.font" = "${config.sys.fonts.code.name}:weight=${config.sys.fonts.code.weight}:size=${config.sys.fonts.size.Emacs}";
            "Emacs.Font" = "${config.sys.fonts.code.name}:weight=${config.sys.fonts.code.weight}:size=${config.sys.fonts.size.Emacs}";
            "Emacs.FontBackend" = "xft,x";
            "Emacs.dialog*.font" = "${config.sys.fonts.code.name}:weight=${config.sys.fonts.code.weight}:size=${config.sys.fonts.size.Emacs}";
            "Emacs.menuBar" = "0";
            "Emacs.toolBar" = "0";
            "Emacs.verticalScrollBars" = false;

            "urgentOnBell" = true;
            "visualBell" = true;
            "URxvt*background" = "#1A1A1A";
            "URxvt*color0" = "#3f3f3f";
            "URxvt*color10" = "#7f9f7f";
            "URxvt*color11" = "#f0dfaf";
            "URxvt*color12" = "#94bff3";
            "URxvt*color13" = "#ec93d3";
            "URxvt*color14" = "#93e0e3";
            "URxvt*color15" = "#ffffff";
            "URxvt*color1" = "#705050";
            "URxvt*color2" = "#60b48a";
            "URxvt*color3" = "#dfaf8f";
            "URxvt*color4" = "#506070";
            "URxvt*color5" = "#dc8cc3";
            "URxvt*color6" = "#8cd0d3";
            "URxvt*color7" = "#DCDCCC";
            "URxvt*color8" = "#709080";
            "URxvt*color9" = "#cc9393";
            "URxvt*colorUL" = "#c5f779";
            "URxvt*foreground" = "#f6f0e8";
            "URxvt*scrollTtyKeypress" = true;
            "URxvt*scrollTtyOutput" = false;
            "URxvt*scrollWithBuffer" = true;
            "URxvt*secondaryScreen" = "1";
            "URxvt*secondaryScroll" = "0";
            "URxvt*underlineColor" = "#c5f779";
            "URxvt.borderless" = true;
            "URxvt.cursorBlink" = false;
            "URxvt.depth" = "32";
            "URxvt.externalBorder" = "3";
            "URxvt.fadeColor" = "#666666";
            "URxvt.fading" = "40";
            "URxvt.font" = "xft:${config.sys.fonts.main.name}:weight=${config.sys.fonts.main.weight}:size=${config.sys.fonts.size.URxvt}";
            "URxvt.geometry" = "80x27";
            "URxvt.internalBorder" = "3";
            "URxvt.keysym.C-Delete" = "perl:matcher:last";
            "URxvt.keysym.C-equal" = "perl:font-size:incglobal";
            "URxvt.keysym.C-minus" = "perl:font-size:decglobal";
            "URxvt.keysym.C-y" = "perl:clipboard:paste";
            "URxvt.keysym.F11" = "perl:fullscreen:switch";
            "URxvt.keysym.M-/" = "perl:keyboard-select:search";
            "URxvt.keysym.M-Delete" = "perl:matcher:list";
            "URxvt.keysym.M-Esc" = "perl:keyboard-select:activate";
            "URxvt.keysym.M-Escape" = "perl:keyboard-select:activate";
            "URxvt.keysym.M-Minus" = "perl:font-size:decrease";
            "URxvt.keysym.M-Plus" = "perl:font-size:increase";
            "URxvt.keysym.M-j" = "command:\033]721;8\007";
            "URxvt.keysym.M-k" = "command:\033]720;8\007";
            "URxvt.keysym.M-u" = "perl:url-select:select_next";
            "URxvt.keysym.M-w" = "perl:clipboard:copy";
            "URxvt.keysym.M-z" = "perl:fullscreen:switch";
            "URxvt.letterSpace" = "-1";
            "URxvt.lineSpace" = "0";
            "URxvt.loginShell" = true;
            "URxvt.matcher.button" = "1";
            "URxvt.perl-ext-common" = "${lib.concatStringsSep "," urxvtPerlExtensions}";
            "URxvt.scrollBar" = false;
            "URxvt.shading" = "100";
            "URxvt.tintColor" = "#354040";
            "URxvt.transparent" = false;
            "URxvt.underlineURLs" = true;
            "URxvt.url-select.launcher" = "${urxvtUrlLauncher}";
            "URxvt.url-select.underline" = true;
            "Urxvt*perl-lib" = "${pkgs.rxvt_unicode-with-plugins}/lib/urxvt/perl/";

            "Xft.antialias" = true;
            "Xft.autohint" = false;
            "Xft.dpi" = "120.0";
            "Xft.hinting" = true;
            "Xft.hintstyle" = "hintslight";
            "Xft.lcdfilter" = "lcddefault";
            "Xft.rgba" = "none";
        };
    };
}
