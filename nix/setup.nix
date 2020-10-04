{ config, lib, options, modulesPath }:
let
  deps = import ../nix/sources.nix;
  nixpkgs = import deps.nixpkgs { config.allowUnfree = true; };

in {
  environment.etc = {
    nixpkgs.source = deps.nixpkgs;
    "nixos/.gitattributes".text = ''
      *.gpg filter=gpg diff=gpg
      **/secrets/** filter=git-crypt diff=git-crypt
    '';
  };

  nix = {
    package = nixpkgs.nixUnstable;
    nixPath = lib.mkForce [ "nixpkgs=/etc/nixpkgs" "nixos-config=/etc/nixos/configuration.nix" ];
    useSandbox = true;
    readOnlyStore = true;
    requireSignedBinaryCaches = true;
    binaryCachePublicKeys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
    binaryCaches = [ "https://cache.nixos.org" ];
    extraOptions = ''
      auto-optimise-store = true
      keep-outputs = true
      keep-derivations = true
      http-connections = 10
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs = {
    config.permittedInsecurePackages = [ "openssl-1.0.2u" ];
    overlays = [
      (import deps.emacs-overlay)
      (_: old: rec {
        inherit deps;

        dunst = old.dunst.override { dunstify = true; };

        i3lock-color = old.i3lock-color.overrideAttrs
          (_: rec { patches = [ ./patches/i3lock-color/forcefully-reset-keyboard-layout-group-to-0.patch ]; });

        vaapiIntel = old.vaapiIntel.override { enableHybridCodec = true; };
      })
    ] ++ map (n: import (./private + ("/" + n))) (builtins.filter
      (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (./private + ("/" + n + "/default.nix")))
      (lib.attrNames (builtins.readDir ./private))) ++ map (n: import (./public + ("/" + n))) (builtins.filter
        (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (./public + ("/" + n + "/default.nix")))
        (lib.attrNames (builtins.readDir ./public)));
  };

  systemd.services.nix-daemon = {
    environment.TMPDIR = "/tmp/buildroot";
    preStart = ''
      mkdir -p /tmp/buildroot
    '';
  };
}
