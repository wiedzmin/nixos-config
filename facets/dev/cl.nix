{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        sbcl
        lispPackages.quicklisp
    ];
}
