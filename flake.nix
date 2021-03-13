{
  description = "Reproducible localhost configurations";

  # TODO: elaborate and add templates
  # TODO: try cachix/pre-commit-hooks.nix
  # TODO: review flake organization reference links:
  # https://github.com/ysndr/blog/blob/e4588f821ce6aee9ec3688ee9af3d2e61e143530/flake.nix
  # https://github.com/Stupremee/nix/blob/092725ceaa8f726ee2d91bf88583e60659031110/flake.nix
  # https://github.com/ccao001/nixrc/blob/5b2e874c0987d01337e08539500fde7dad4ba2f6/flake.nix
  # https://github.com/poscat0x04/nix-repo/blob/b0242f363981a3aad0b8779406cf1e53d6d761c6/flake.nix
  # https://github.com/RaitoBezarius/machines/blob/a29744af496f45f7312418988e718e297e527a3d/flake.nix
  # https://github.com/leo60228/dotfiles/blob/5d32894eec01aad2303b7eecb3ca6fc7813c3c56/flake.nix
  # https://github.com/danderson/homelab/blob/25a7a5ee7252049313dc9968ed522c28ae9201c0/flake.nix
  # https://github.com/lokegustafsson/nixfiles/blob/7e3ecc70c3ce7e29c857e0f8fc96cf002d3a1b4d/flake.nix
  # https://github.com/balsoft/nixos-config/blob/58eee3651e0a1c57b5f654d28d6db2cdf44741fb/flake.nix !
  # https://github.com/qjcg/nix/blob/71d6ae1b88fb7184ea46bb8ead2168378ea4ae70/flake.nix !!!!
  # https://github.com/rissson/dotshabka/blob/f13a2f79ff9cb486d71a5dd46a3ce7ed1cafa6a0/flake.nix !!
  # https://github.com/christoph00/nixcfg/blob/135bb5c91c60f6647b3f1e2e166af943e7c820f9/flake.nix !!
  # https://github.com/berbiche/dotfiles/blob/873889a3316f679d79afc303c79754898794d832/flake.nix !!!!!!!

  inputs = rec {
    stable.url = "github:NixOS/nixpkgs/nixos-20.03";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    future.url = "github:NixOS/nixpkgs/257cbbcd3ab7bd96f5d24d50adc807de7c82e06d"; # non-systemwide unstable
    nixpkgs-arbtt = {
      url = "github:nixos/nixpkgs/224804d0f68bd2bb083b8491c0c6cf3f34f2de10";
      flake = false;
    };
    nixpkgs-25_01_21.url = "github:nixos/nixpkgs/891f607d5301d6730cb1f9dcf3618bcb1ab7f10e";
    nixpkgs-16_04_20.url = "github:nixos/nixpkgs/b61999e4ad60c351b4da63ae3ff43aae3c0bbdfb";
    nixpkgs-03_12_20.url = "github:nixos/nixpkgs/296793637b22bdb4d23b479879eba0a71c132a66";
    nixpkgs-proposed = {
      url = "github:wiedzmin/nixpkgs";
      flake = false;
    };

    # FIXME: decouple from user
    nur.url = "/home/alex3rd/workspace/repos/github.com/wiedzmin/NUR";

    home-manager.url = "github:rycee/home-manager";
    emacs.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    i3blocks-contrib = {
      url = "github:vivien/i3blocks-contrib";
      flake = false;
    };

    # NOTE: using these instead of quelpa
    emacs-ob-go = {
      url = "github:pope/ob-go";
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
    in {
      templates = {
        # TODO: debug and add python
        ansible = {
          path = ./modules/dev/misc/templates/ansible;
          description = "Ansible environment.";
        };
        media.encode = {
          path = ./modules/content/media/templates/encode;
          description = "Various multimedia conversions.";
        };
        staging = {
          path = ./modules/nix/core/templates/staging;
          description = "Staging packages sandbox environment.";
        };
      };

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
