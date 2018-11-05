{config, pkgs, lib, ...}:

{
    imports = [
        ../../private/traits.nix
    ];

    system.activationScripts.refreshNASVolumes = "echo '" +
        (builtins.concatStringsSep "\n" config.misc.nas_volumes) +
        "' > ${config.users.extraUsers.alex3rd.home}/nas_volumes";
    system.activationScripts.refreshShellBookmarks = "echo '" +
        (builtins.concatStringsSep "\n"
             (lib.mapAttrsToList (bmk: path: bmk + " : " + path) config.misc.shell_bookmarks)) +
        "' > ${config.users.extraUsers.alex3rd.home}/.bookmarks";

    home-manager.users.alex3rd = {
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
            enable = true;
            enableZshIntegration = true;
        };
    };
}
