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
                    ${pkgs.feh}/bin/feh --bg-fill ${config.sys.wallpapers_dir}/${config.sys.current_wallpaper}
                '';
            };
            # TODO: actualize list below
            ".config/autorandr/mobile/config".source = ../../../../hardware/autorandr/x230/mobile/config;
            ".config/autorandr/mobile/setup".source = ../../../../hardware/autorandr/x230/mobile/setup;
            ".config/autorandr/docked-home/config".source = ../../../../hardware/autorandr/x230/docked-home/config;
            ".config/autorandr/docked-home/setup".source = ../../../../hardware/autorandr/x230/docked-home/setup;
            ".config/autorandr/docked-office-double/config".source = ../../../../hardware/autorandr/x230/docked-office-double/config;
            ".config/autorandr/docked-office-double/setup".source = ../../../../hardware/autorandr/x230/docked-office-double/setup;
        };
    };
}
