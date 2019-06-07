{config, pkgs, lib, ...}:
with import <home-manager/modules/lib/dag.nix> { inherit lib; };
with import ./const.nix {inherit config pkgs;};
{
    imports = [
        <home-manager/nixos>
        ./config
        ./modules
        ./packages.nix
        ./services
        ../scripts.nix
    ];

    users.extraUsers."${userName}" = {
        isNormalUser = true;
        uid = 1000;
        description = "Alex Ermolov";
        shell = pkgs.zsh;
        extraGroups = [
            "audio"
            "input"
            "lp"
            "networkmanager"
            "scanner"
            "video"
            "wheel"
        ];
    };

    systemd.user.services.lowbatt.enable = true;

    system.activationScripts.saveCurrentHMVersion = ''
        cd /etc/nixos/pkgs/home-manager
        hm_revision=$(${pkgs.git}/bin/git rev-parse --short HEAD)
        echo "$hm_revision" > /etc/current-home-manager
    '';

    nix.trustedUsers = [ userName ];

    networking.extraHosts = (builtins.concatStringsSep "\n"
                                      (map (host: host.ip + "   " + (builtins.concatStringsSep " " host.hostNames))
                                      (config.job.extraHosts ++ config.misc.extraHosts)));

    home-manager.users."${userName}" = {
        nixpkgs.config.allowUnfree = true;
        home.packages = with pkgs; [
            # base
            file
            glibcLocales
        ];
    };
}
