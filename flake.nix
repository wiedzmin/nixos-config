{
  description = "Reproducible localhost configurations";

  # TODO: try cachix/pre-commit-hooks.nix
  inputs = rec {
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = "github:wiedzmin/NUR";

    home-manager.url = "github:nix-community/home-manager";
    emacs.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    i3blocks-contrib = {
      url = "github:vivien/i3blocks-contrib";
      flake = false;
    };
    i3status-rust-stable.url = "github:nixos/nixpkgs/8dcbc6dff3e095743552a79912ae13698e8396e0";
    scantailor-stable.url = "github:nixos/nixpkgs/ff9efb0724de5ae0f9db9df2debefced7eb1571d";
    nixpkgs-index-fm.url = "github:nixos/nixpkgs/a4bf44345706231f9dd56f85757499af1e940847";
    nixpkgs-mspyls.url = "github:nixos/nixpkgs/e494a908e8895b9cba18e21d5fc83362f64b3f6a";
    nixpkgs-iocomfy.url = "github:nixos/nixpkgs/5857574d45925585baffde730369414319228a84";

    # NOTE: using these instead of quelpa
    emacs-org-bars = {
      url = "github:tonyaldon/org-bars";
      flake = false;
    };

    # NOTE: Kitty's kittens
    kitty-grab = {
      url = "github:yurikhan/kitty_grab";
      flake = false;
    };
    kitty-search = {
      url = "github:trygveaa/kitty-kitten-search";
      flake = false;
    };

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
    nix-zsh-completions = {
      url = "github:spwhitt/nix-zsh-completions";
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
    zsh-autocomplete = {
      # TODO: keep for future reevaluation (not completely usasble yet)
      url = "github:marlonrichert/zsh-autocomplete";
      flake = false;
    };
  };

  # TODO: consider thinking of https://github.com/lokegustafsson/nixfiles/blob/7e3ecc70c3ce7e29c857e0f8fc96cf002d3a1b4d/flake.nix
  # with context of role-based configuration

  # TODO: review https://github.com/leo60228/dotfiles/blob/5d32894eec01aad2303b7eecb3ca6fc7813c3c56/flake.nix deeper

  # TODO: review https://github.com/nix-community/nixt

  outputs = { self, unstable, ... }@inputs:
    let
      lib = unstable.lib;
      system = "x86_64-linux";
      overlays = {
        unstable = _: _: {
          unstable = import inputs.unstable {
            overlays = with inputs; [ emacs.overlay nur.overlay self.overlay ];
            inherit system;
          };
        };
      };
    in
    {
      overlay = import ./overlay.nix inputs;

      devShell."${system}" = import ./shell.nix { pkgs = inputs.unstable.legacyPackages.${system}; };

      nixosConfigurations = {
        laptoptop = lib.nixosSystem {
          inherit system;
          modules = [
            {
              nixpkgs.overlays = [
                overlays.unstable
                (_: old: rec {
                  i3lock-color = old.i3lock-color.overrideAttrs (_: rec {
                    patches = [ ./modules/workstation/lockscreen/patches/i3lock-color-pass-layout-switching.patch ];
                  });
                  mps-youtube = old.mps-youtube.overrideAttrs
                    (_: rec { patches = [ ./modules/content/media/patches/0001-fix-1134.patch ]; });
                  tabnine = old.tabnine.overrideAttrs (_: rec {
                    installPhase = ''
                      mkdir -p $out/bin
                      cp $src $out/bin/TabNine
                      chmod a+x $out/bin/TabNine
                    '';
                  });
                  vaapiIntel = old.vaapiIntel.override { enableHybridCodec = true; };
                  nyxt =
                    let
                      nyxt' = old.lispPackages.nyxt.overrideAttrs (_: rec {
                        patches = [
                          (old.fetchpatch {
                            url = "https://github.com/atlas-engineer/nyxt/commit/0786ee9fd80bf47827b6a9858895663db8498d12.patch";
                            sha256 = "sha256-lF5Ue6XhjfcqfXKpelUlVWlvl2Dq8IMNmCYZGYTdA38=";
                          })
                        ];
                      });
                    in
                    old.nyxt.overrideAttrs
                      (_: rec { src = nyxt'; });
                  kitty_grab = unstable.legacyPackages.${system}.applyPatches {
                    patches = [ ./modules/shell/kitty/patches/fixed-ui-module-accessibility.patch ];
                    name = "kitty_grab";
                    src = inputs.kitty-grab;
                  };
                  kitty_search = unstable.legacyPackages.${system}.applyPatches {
                    patches = [ ];
                    name = "kitty_search";
                    src = inputs.kitty-search;
                  };
                })
              ];
            }
            (import ./machines/laptoptop)
            inputs.home-manager.nixosModules.home-manager
            inputs.unstable.nixosModules.notDetected
          ];
          specialArgs = { inherit inputs; };
        };
        momcat = lib.nixosSystem {
          inherit system;
          modules = [
            { nixpkgs.overlays = [ overlays.unstable ]; }
            (import ./machines/momcat)
            inputs.home-manager.nixosModules.home-manager
            inputs.unstable.nixosModules.notDetected
          ];
          specialArgs = { inherit inputs; };
        };
      };

      laptoptop = inputs.self.nixosConfigurations.laptoptop.config.system.build.toplevel;
      momcat = inputs.self.nixosConfigurations.momcat.config.system.build.toplevel;
    };
}
