{config, pkgs, ...}:

{
    config = {
        nixpkgs.config.packageOverrides = super: {
            optimize-nix = pkgs.writeShellScriptBin "optimize-nix" ''
                set -eu

                # Delete everything from this profile that isn't currently needed
                ${pkgs.nix}/bin/nix-env --delete-generations old

                # Delete generations older than a week
                ${pkgs.nix}/bin/nix-collect-garbage
                ${pkgs.nix}/bin/nix-collect-garbage --delete-older-than 7d

                # Optimize
                ${pkgs.nix}/bin/nix-store --gc --print-dead
                ${pkgs.nix}/bin/nix-store --optimise
            '';
            watch_nixpkgs_updates = pkgs.writeShellScriptBin "watch_nixpkgs_updates" ''
                if [ ! -z "$(${pkgs.watch_git_remote_status}/bin/watch_git_remote_status /etc/nixos/pkgs/nixpkgs-channels | grep available)" ]; then
                    ${pkgs.dunst}/bin/dunstify -t 30000 "Nixpkgs updated, consider install!"
                fi
            '';
            watch_home_manager_updates = pkgs.writeShellScriptBin "watch_home_manager_updates" ''
                if [ ! -z "$(${pkgs.watch_git_remote_status}/bin/watch_git_remote_status /etc/nixos/pkgs/home-manager | grep available)" ]; then
                    ${pkgs.dunst}/bin/dunstify -t 30000 "Home-manager updated, consider install!"
                fi
            '';
            show_nixpkgs_updates = pkgs.writeShellScriptBin "show_nixpkgs_updates" ''
                echo "$(${pkgs.watch_git_remote_status}/bin/watch_git_remote_status /etc/nixos/pkgs/nixpkgs-channels)" > /tmp/nixpkgs-channels-git-status
                ${pkgs.yad}/bin/yad --filename /tmp/nixpkgs-channels-git-status --text-info
                rm /tmp/nixpkgs-channels-git-status

            '';
            show_home_manager_updates = pkgs.writeShellScriptBin "show_home_manager_updates" ''
                echo "$(${pkgs.watch_git_remote_status}/bin/watch_git_remote_status /etc/nixos/pkgs/home-manager)" > /tmp/home-manager-git-status
                ${pkgs.yad}/bin/yad --filename /tmp/home-manager-git-status --text-info
                rm /tmp/home-manager-git-status
            '';
       };
    };
}
