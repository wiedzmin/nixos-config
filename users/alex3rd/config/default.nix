{config, pkgs, ...}:
{
    imports =
    [
        ./dev
        ./gui
        ./network
        ./shell
    ];

    home-manager.users.alex3rd = {
        programs.ssh = {
            enable = true;
            forwardAgent = true;
            userKnownHostsFile = "~/.ssh/known_hosts";
            controlMaster = "auto";
            controlPath = "~/.ssh/sockets/%r@%h:%p";
            controlPersist = "4h";
            serverAliveInterval = 30;
        };
    };
}
