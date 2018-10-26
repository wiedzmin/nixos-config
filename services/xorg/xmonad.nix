{ config, pkgs, ... }:
{
    services.xserver.windowManager = {
        default = "xmonad";
        xmonad = {
            enable = true;
            enableContribAndExtras = true;
            extraPackages = p: [ p.taffybar p.dbus p.monad-logger p.lens p.split ];
        };
    };
}
