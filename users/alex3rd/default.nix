{config, pkgs, lib, ...}:

{
    imports = [
        <home-manager/nixos>
        ../../toolbox/desktop/org_protocol.nix
        ../../toolbox/dev
        ../../toolbox/misc
        ../../toolbox/network
        ../../toolbox/nix.nix
        ../../toolbox/scripts/inventory.nix
        ../../toolbox/scripts/services.nix
        ../../toolbox/scripts/virt.nix
        ../../toolbox/security.nix
        ../../toolbox/shell
        ../../toolbox/text
        ../../toolbox/virt
        ./dotfiles
        ./services
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
            rofi_ctop
            rofi_jnettop
            rofi_entrypoint

            networkmanager_dmenu # using rofi, despite naming

            order_screenshots
        ];
        programs.ssh = {
            enable = true;
            forwardAgent = true;
            userKnownHostsFile = "~/.ssh/known_hosts";
            controlMaster = "auto";
            controlPath = "~/.ssh/sockets/%r@%h:%p";
            controlPersist = "4h"; # was 5m in imperative config
            serverAliveInterval = 30;
        };
        services.gpg-agent = {
            enable = true;
            defaultCacheTtl = 34560000;
            defaultCacheTtlSsh = 34560000;
            maxCacheTtl = 34560000;
            enableSshSupport = true;
            extraConfig = ''
                allow-emacs-pinentry
                allow-loopback-pinentry
            '';
        };
    };
}
