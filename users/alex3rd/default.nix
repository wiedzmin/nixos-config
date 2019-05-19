{config, pkgs, lib, ...}:
with import <home-manager/modules/lib/dag.nix> { inherit lib; }; # TODO: make more declarative
{
    imports = [
        <home-manager/nixos>
        ./config
        ./packages.nix
        ./services
        ../scripts.nix
    ];

    users.extraUsers.alex3rd = {
        isNormalUser = true;
        uid = 1000;
        description = "Alex Ermolov";
        shell = pkgs.zsh;
        extraGroups = [
            "audio"
            "docker"
            "input"
            "lp"
            "networkmanager"
            "scanner"
            "vboxusers"
            "video"
            "wheel"
        ];
    };

    system.activationScripts.saveCurrentHMVersion = ''
        cd /etc/nixos/pkgs/home-manager
        hm_revision=$(${pkgs.git}/bin/git rev-parse --short HEAD)
        echo "$hm_revision" > /etc/current-home-manager
    '';

    nix.trustedUsers = [ "alex3rd" ];

    networking.extraHosts = (builtins.concatStringsSep "\n"
                                      (map (host: host.ip + "   " + (builtins.concatStringsSep " " host.hostNames))
                                      (config.job.extraHosts ++ config.misc.extraHosts)));

    home-manager.users.alex3rd = {
        nixpkgs.config.allowUnfree = true;
        home.packages = with pkgs; [
            # base
            file
            glibcLocales
        ];
    };
}
