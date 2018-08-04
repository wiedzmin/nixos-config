{ config, pkgs, ...}:

{
    nix.extraOptions = ''
        gc-keep-outputs = true
        gc-keep-derivations = true
    '';
    nix.trustedBinaryCaches = [
        "http://hydra.nixos.org"
        "http://cache.nixos.org"
    ];
}
