{config, pkgs, lib, ...}:

{
    home-manager.users.alex3rd = {
        home.file = {
            ".config/autorandr/predetect" = {
                executable = true;
                text = ''
                    #!${pkgs.bash}/bin/bash
                    ${pkgs.procps}/bin/pkill -f compton
                '';
            };
            ".config/autorandr/postswitch" = {
                executable = true;
                text = ''
                    #!${pkgs.bash}/bin/bash
                    ${pkgs.feh}/bin/feh --bg-fill ${config.sys.wallpaper.baseDir}/${config.sys.wallpaper.current}
                '';
            };
            ".config/autorandr/docked-home/config".source = ../../../../hardware/autorandr/laptoptop/docked-home/config;
            ".config/autorandr/docked-home/setup".source = ../../../../hardware/autorandr/laptoptop/docked-home/setup;
            ".config/autorandr/docked-office/config".source = ../../../../hardware/autorandr/laptoptop/docked-office/config;
            ".config/autorandr/docked-office/setup".source = ../../../../hardware/autorandr/laptoptop/docked-office/setup;
            ".config/autorandr/mobile/config".source = ../../../../hardware/autorandr/laptoptop/mobile/config;
            ".config/autorandr/mobile/setup".source = ../../../../hardware/autorandr/laptoptop/mobile/setup;
            ".config/autorandr/undocked-parents-dsub/config".source = ../../../../hardware/autorandr/laptoptop/undocked-parents-dsub/config;
            ".config/autorandr/undocked-parents-dsub/setup".source = ../../../../hardware/autorandr/laptoptop/undocked-parents-dsub/setup;
        };
    };
}
