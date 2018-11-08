{config, lib, options, modulesPath}:

let
    path = ../overlay;
    content = builtins.readDir path;
in {
  nix.nixPath = [
      "nixpkgs=/etc/nixos/pkgs/nixpkgs-channels"
      "nixpkgs-overlays=/etc/nixos/overlay"
      "nixos-config=/etc/nixos/configuration.nix"
      "home-manager=/etc/nixos/pkgs/home-manager"
  ];
  nixpkgs.overlays = map (n: import (path + ("/" + n)))
      (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
        (lib.attrNames content));
  services.journald.extraConfig = ''
      MaxRetentionSec=7day
  '';
  nix.extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
  '';
  nix.trustedBinaryCaches = [
      "http://hydra.nixos.org"
      "http://cache.nixos.org"
  ];
}
