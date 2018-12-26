{config, pkgs, lib, ...}:

{
    imports = [
        <home-manager/nixos>
        ../../toolbox/desktop/org_protocol.nix
        ../../toolbox/dev/common/clients.nix
        ../../toolbox/dev/common/forensics.nix
        ../../toolbox/dev/common/misc.nix
        ../../toolbox/dev/common/monitoring/misc.nix
        ../../toolbox/dev/common/monitoring/network.nix
        ../../toolbox/dev/common/monitoring/resources.nix
        ../../toolbox/dev/common/vcs.nix
        ../../toolbox/dev/hardware.nix
        ../../toolbox/dev/lang/ccpp.nix
        ../../toolbox/dev/lang/cl.nix
        ../../toolbox/dev/lang/clojure.nix
        ../../toolbox/dev/lang/go/dev.nix
        ../../toolbox/dev/lang/go/infra.nix
        ../../toolbox/dev/lang/js.nix
        ../../toolbox/dev/lang/python/dev.nix
        ../../toolbox/dev/lang/python/infra.nix
        ../../toolbox/dev/lang/rust.nix
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
        ./dotfiles/dev/lisp.nix
        ./dotfiles/dev/python.nix
        ./dotfiles/mail.nix
        ./dotfiles/shell/common.nix
        ./dotfiles/shell/term.nix
        ./dotfiles/shell/tmux.nix
        ./dotfiles/shell/zsh.nix
        ./dotfiles/x11/autorandr.nix
        ./dotfiles/x11/browser.nix
        ./dotfiles/x11/misc.nix
        ./dotfiles/x11/taffybar.nix
        ./dotfiles/x11/xresources.nix
        ./services/collect-garbage.nix
        ./services/fusuma.nix
        ./services/git-auto.nix
        ./services/imapfilter.nix
        ./services/nixpkgs-update-status.nix
        ./services/sshuttle.nix
        ./services/vpn.nix
        ./services/watch-dunst.nix
        ./services/xkeysnail.nix
    ];

    users.extraUsers = {
        alex3rd = {
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
    };

    nix.trustedUsers = [ "alex3rd" ];

    networking.extraHosts = (builtins.concatStringsSep "\n"
                                      (map (host: host.ip + "   " + (builtins.concatStringsSep " " host.hostNames))
                                      (config.job.extraHosts ++ config.misc.extraHosts)));

    home-manager.users.alex3rd = {
        home.packages = with pkgs; [
            # base
            file
            glibcLocales

            backlightctl
            jobvpnctl
            lockscreen
            rofi_insert_snippet
            rofi_service_journal
            sshuttlectl
            volumectl
            wifictl

            docker-machine-export
            docker-machine-import
            rofi_docker_container_traits
            rofi_docker_stacks_info
            rofi_remote_docker_logs
            rofi_docker_shell
            rofi_dbms
            rofi_containerized_services_discovery

            networkmanager_dmenu # using rofi, despite naming
        ];
    };
}
