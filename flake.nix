{
  description = "Reproducible localhost configurations";

  # TODO: try cachix/pre-commit-hooks.nix
  inputs = rec {
    stable.url = "github:NixOS/nixpkgs/nixos-20.03";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = "github:wiedzmin/NUR";

    home-manager.url = "github:nix-community/home-manager";
    emacs.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    i3blocks-contrib = {
      url = "github:vivien/i3blocks-contrib";
      flake = false;
    };
    base16-rofi = {
      url = "github:0xdec/base16-rofi";
      flake = false;
    };

    # NOTE: using these instead of quelpa
    emacs-ob-go = {
      url = "github:pope/ob-go";
      flake = false;
    };
    emacs-org-appear = {
      url = "github:awth13/org-appear";
      flake = false;
    };
    emacs-bruh = {
      url = "github:a13/bruh";
      flake = false;
    };
    emacs-bookmark-view = {
      url = "github:minad/bookmark-view";
      flake = false;
    };
    emacs-consult-projectile = {
      url = "gitlab:OlMon/consult-projectile";
      flake = false;
    };
    emacs-consult-lsp = {
      url = "github:gagbo/consult-lsp";
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
    zsh-go-task-completions = {
      url = "github:sawadashota/go-task-completions";
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
        unstable = final: prev: {
          unstable = (import inputs.unstable {
            overlays = with inputs; [ emacs.overlay nur.overlay ];
            inherit system;
          });
        };
      };
    in
    {
      devShell."${system}" = import ./shell.nix { pkgs = unstable.legacyPackages.${system}; };

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
