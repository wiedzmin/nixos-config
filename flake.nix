{
  description = "Reproducible localhost configurations";

  # TODO: elaborate and add templates
  # TODO: try cachix/pre-commit-hooks.nix

  inputs = rec {
    stable.url = "github:NixOS/nixpkgs/nixos-20.03";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-mm.url = "github:nixos/nixpkgs/a2cdd1464b7";
    nixpkgs-02_06_20.url = "github:nixos/nixpkgs/467ce5a9f45aaf96110b41eb863a56866e1c2c3c";
    nixpkgs-08_02_20 = {
      url = "github:nixos/nixpkgs/8130f3c1c2bb0e533b5e150c39911d6e61dcecc2";
      flake = false;
    };
    nixpkgs-09_07_20.url = "github:nixos/nixpkgs/8d05772134f17180fb2711d0660702dae2a67313";
    nixpkgs-16_04_20.url = "github:nixos/nixpkgs/b61999e4ad60c351b4da63ae3ff43aae3c0bbdfb";

    home-manager.url = "github:rycee/home-manager"; # /bqv-flakes
    emacs.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    git-extra-commands = {
      url = "github:unixorn/git-extra-commands";
      flake = false;
    };
    liquidprompt = {
      url = "github:nojhan/liquidprompt";
      flake = false;
    };
    nixos-artwork = {
      url = "github:NixOS/nixos-artwork";
      flake = false;
    };
    pass-zsh-completion = {
      url = "github:ninrod/pass-zsh-completion";
      flake = false;
    };
    yasnippet-snippets = {
      url = "github:wiedzmin/yasnippet-snippets";
      flake = false;
    };
    zsh-async = {
      url = "github:mafredri/zsh-async";
      flake = false;
    };
    zsh-command-time = {
      url = "github:popstas/zsh-command-time";
      flake = false;
    };
    zsh-fuzzy-search-and-edit = {
      url = "github:seletskiy/zsh-fuzzy-search-and-edit";
      flake = false;
    };
    zsh-nix-shell = {
      url = "github:chisui/zsh-nix-shell";
      flake = false;
    };
    zsh-notify = {
      url = "github:marzocchi/zsh-notify";
      flake = false;
    };
    zsh-reentry-hook = {
      url = "github:RobSis/zsh-reentry-hook";
      flake = false;
    };
    zsh-syntax-highlighting = {
      url = "github:zsh-users/zsh-syntax-highlighting";
      flake = false;
    };
    zsh-you-should-use = {
      url = "github:MichaelAquilina/zsh-you-should-use";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      lib = nixpkgs.lib;
      system = "x86_64-linux";
      overlays = {
        unstable = final: prev: {
          unstable = (import inputs.nixpkgs {
            overlays = [ inputs.emacs.overlay ];
            inherit system;
          });
        };
      };
    in {
      nixosConfigurations = {
        laptoptop = lib.nixosSystem {
          inherit system;
          modules = [
            {
              nixpkgs.overlays = [
                overlays.unstable
                (_: old: rec {
                  dunst = old.dunst.override { dunstify = true; };

                  i3lock-color = old.i3lock-color.overrideAttrs (_: rec {
                    patches = [ ./nix/patches/i3lock-color/forcefully-reset-keyboard-layout-group-to-0.patch ];
                  });

                  vaapiIntel = old.vaapiIntel.override { enableHybridCodec = true; };
                })
              ] ++ map (n: import (./nix/overlays + ("/" + n))) (builtins.filter
                (n: builtins.match ".*\\.nix" n != null || builtins.pathExists ("/" + n + "/default.nix"))
                (lib.attrNames (builtins.readDir ./nix/overlays)));
            }
            (import ./machines/laptoptop)
            inputs.home-manager.nixosModules.home-manager
            inputs.nixpkgs.nixosModules.notDetected
          ];
          specialArgs = { inherit inputs; };
        };
      };

      laptoptop = inputs.self.nixosConfigurations.laptoptop.config.system.build.toplevel;

      # legacyPackages.x86_64-linux = (builtins.head (builtins.attrValues self.nixosConfigurations)).pkgs;
    };
}
