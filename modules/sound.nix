{ config, pkgs, ... }:
{
    sound.enable = true;

    hardware.pulseaudio = {
        enable = true;
        support32Bit = true;
    };

    environment.systemPackages = with pkgs; [
        pasystray
        pavucontrol
    ];
}
