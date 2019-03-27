{config, pkgs, lib, ...}:

{
    imports = [
        <home-manager/nixos>
        ../../toolbox/desktop/org_protocol.nix
        ../../toolbox/dev/common/clients.nix
        ../../toolbox/dev/common/misc.nix
        ../../toolbox/dev/common/monitoring/misc.nix
        ../../toolbox/dev/common/monitoring/network.nix
        ../../toolbox/dev/common/monitoring/resources.nix
        ../../toolbox/dev/common/vcs.nix
        ../../toolbox/misc/compress.nix
        ../../toolbox/misc/finance.nix
        ../../toolbox/misc/media.nix
        ../../toolbox/misc/org.nix
        ../../toolbox/network/clients.nix
        ../../toolbox/network/misc.nix
        ../../toolbox/network/system.nix
        ../../toolbox/nix.nix
        ../../toolbox/scripts/inventory.nix
        ../../toolbox/scripts/services.nix
        ../../toolbox/scripts/virt.nix
        ../../toolbox/security.nix
        ../../toolbox/shell/convert.nix
        ../../toolbox/shell/misc.nix
        ../../toolbox/shell/search.nix
        ../../toolbox/shell/term.nix
        ../../toolbox/shell/view.nix
        ../../toolbox/text/convert.nix
        ../../toolbox/text/misc.nix
        ../../toolbox/text/scanner.nix
        ../../toolbox/text/tex.nix
        ../../toolbox/text/view.nix
        ../../toolbox/virt/docker.nix
        ../../toolbox/virt/misc.nix
        ../../toolbox/virt/vm.nix
        ./dotfiles/dev/editor.nix
        ./dotfiles/dev/git.nix
        ./dotfiles/shell/common.nix
        ./dotfiles/shell/term.nix
        ./dotfiles/shell/tmux.nix
        ./dotfiles/shell/zsh.nix
        ./dotfiles/x11/browser.nix
        ./dotfiles/x11/misc.nix
        ./dotfiles/x11/xresources.nix
        ./services/collect-garbage.nix
        ./services/nixpkgs-update-status.nix
        ./services/watch-dunst.nix
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

            backlightctl
            jobvpnctl
            rofi_service_journal
            sshuttlectl
            volumectl
            wifictl

            networkmanager_dmenu # using rofi, despite naming
        ];
    };
}
