{config, pkgs, lib, options, modulesPath}:

let
    path = ./overlay;
    content = builtins.readDir path;
in {
    nix = {
        # For interactive usage
        nixPath = [
            "nixpkgs=/etc/nixos/pkgs/nixpkgs-channels"
            "nixpkgs-overlays=/etc/nixos/pkgs/overlay"
            "nixos-config=/etc/nixos/configuration.nix"
            "home-manager=/etc/nixos/pkgs/home-manager-proposed"
        ];
        useSandbox = true;
        readOnlyStore = true;
        requireSignedBinaryCaches = true;
        binaryCachePublicKeys = [
            "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        ];
        binaryCaches = [
            "https://hydra.nixos.org"
            "https://cache.nixos.org"
        ];
        extraOptions = ''
            auto-optimise-store = true
            keep-outputs = true
            keep-derivations = true
            http-connections = 10
        '';
    };
    # For nixos-rebuild
    nixpkgs.overlays = map (n: import (path + ("/" + n)))
        (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
          (lib.attrNames content));
}
