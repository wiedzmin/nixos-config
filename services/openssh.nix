{ config, ... }:

{
    services.openssh = {
        enable = true;
        forwardX11 = true;
    };
}
