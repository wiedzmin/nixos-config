{config, pkgs, lib, ...}:

{
    imports = [
        ../../private/traits/common.nix
        ../../private/traits/job.nix
    ];

    system.activationScripts.refreshShellBookmarks = "echo '" +
        (builtins.concatStringsSep "\n"
             (lib.mapAttrsToList (bmk: path: bmk + " : " + path)
             (config.common.shell_bookmarks // config.job.shell_bookmarks))) +
        "' > ${config.users.extraUsers.alex3rd.home}/.bookmarks";

    home-manager.users.alex3rd = {
        home.packages = with pkgs; [
            optimize-nix
        ];

        programs.htop.enable = true;
        programs.command-not-found.enable = true;
        programs.lesspipe.enable = true;
        programs.man.enable = true;
        programs.info.enable = true;
        programs.fzf = {
            enable = true;
            enableZshIntegration = true;
        };
        programs.direnv = {
            # NOTE: fails on 'use nix;' on call to virtualenv
            enable = true;
            enableZshIntegration = true;
        };
    };
}
