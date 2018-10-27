{ config, pkgs, ... }:
{
    services = {
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
        };
    };
}
