{config, pkgs, lib, ...}:

{
    imports = [
        <home-manager/nixos>
        ./dotfiles/shell/common.nix
        ./dotfiles/shell/term.nix
        ./dotfiles/shell/tmux.nix
        ./dotfiles/shell/zsh.nix
        ./dotfiles/x11/browser.nix
        ./dotfiles/x11/misc.nix
        ./dotfiles/x11/xresources.nix
        ./services/xkeysnail.nix
        ./private/traits/network.nix
    ];

    users.extraUsers.kotya = {
        isNormalUser = true;
        uid = 1000;
        description = "Ekaterina Ermolova";
        shell = pkgs.zsh;
        extraGroups = [
            "audio"
            "input"
            "lp"
            "networkmanager"
            "vboxusers"
            "video"
            "wheel"
        ];
    };

    nix.trustedUsers = [ "kotya" ];

    networking.extraHosts = (builtins.concatStringsSep "\n"
                                      (map (host: host.ip + "   " + (builtins.concatStringsSep " " host.hostNames))
                                      config.misc.extra_hosts));

    home-manager.users.kotya = {
        home.packages = with pkgs; [
            # base
            file
            glibcLocales

            anydesk
        ];
    };
}
