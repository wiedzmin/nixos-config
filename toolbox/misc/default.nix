{pkgs, ...}:
{
    imports =
    [
        ./compress.nix
        ./media.nix
        ./org.nix
        ./staging.nix
    ];

    system.activationScripts.saveCurrentHMVersion = ''
        cd /etc/nixos/pkgs/home-manager
        hm_revision=$(${pkgs.git}/bin/git rev-parse --short HEAD)
        echo "$hm_revision" > /etc/current-home-manager
    '';
}
