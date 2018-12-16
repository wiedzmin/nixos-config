{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        lispPackages.quicklisp
        sbcl
    ];
}
