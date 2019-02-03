{ config, pkgs, ... }:

{
    services.kmscon = {
        enable = true;
        hwRender = true;
        extraConfig = ''
            font-name=Iosevka Bold # TODO: templatize
            font-size=14
        '';
        extraOptions = "--term xterm-256color";
        autologinUser = "alex3rd";
    };

    services.urxvtd.enable = true;

    environment.systemPackages = with pkgs; [
        alacritty
        eternal-terminal
        rxvt_unicode-with-plugins
        tmux
    ];
}
