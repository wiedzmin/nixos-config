{ config, pkgs, lib, options, modulesPath }:
let
  deps = import ../nix/sources.nix;
in
{
  environment.etc.nixpkgs.source = deps.nixpkgs;
  nix = {
    nixPath = lib.mkForce [
      "nixpkgs=/etc/nixpkgs"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
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
    '';
  };

  nixpkgs = {
    overlays = [
      (import deps.emacs-overlay)
      (self: old:
        rec {
          inherit deps;

          dunst = old.dunst.override { dunstify = true; };

          i3lock-color = old.i3lock-color.overrideAttrs (oa: rec {
            patches = [ ./patches/i3lock-color/forcefully-reset-keyboard-layout-group-to-0.patch ];
          });

          vaapiIntel = old.vaapiIntel.override { enableHybridCodec = true; };

          tmuxPlugins = old.tmuxPlugins // old.tmuxPlugins.fzf-tmux-url.overrideAttrs (attrs: {
            postPatch = ''
              substituteInPlace fzf-url.sh --replace "capture-pane -J -p" "capture-pane -S -${
                  builtins.toString config.attributes.tmux.paneHistoryDepthLines
              } -J -p"
              substituteInPlace fzf-url.sh --replace "fzf-tmux" "${old.skim}/bin/sk-tmux"
              substituteInPlace fzf-url.sh --replace "--no-preview" ""
            '';
          });
      })
    ] ++ map (n: import (./private + ("/" + n)))
      (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (./private + ("/" + n + "/default.nix")))
        (lib.attrNames (builtins.readDir ./private)));
  };

  systemd.services.nix-daemon = {
    environment.TMPDIR = "/tmp/buildroot";
    preStart = ''
      mkdir -p /tmp/buildroot
    '';
  };
}
