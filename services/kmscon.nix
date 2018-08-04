{ config, ... }:

{
    services.kmscon = {
        enable = true;
        hwRender = true;
        extraConfig = ''
            font-name=Fira Mono
        '';
    };
}
