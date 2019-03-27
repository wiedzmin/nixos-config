{ config, pkgs, ... }:

{
    services.kmscon = {
        enable = true;
        hwRender = true;
        extraConfig = ''
            font-name=${config.sys.fonts.term.name}
            font-size=${config.sys.fonts.size.Alacritty}
        '';
        extraOptions = "--term xterm-256color";
        autologinUser = "${config.sys.kmscon.autologinUser}";
    };

    services.urxvtd.enable = true;

    environment.systemPackages = with pkgs; [
        alacritty
        eternal-terminal
        rxvt_unicode-with-plugins
        tmux
    ];
}
