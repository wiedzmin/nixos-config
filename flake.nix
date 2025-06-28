{
  description = "Reproducible localhost configurations";

  inputs = {
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    nur.url = "github:wiedzmin/NUR";

    home-manager.url = "github:nix-community/home-manager";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixos-artwork = {
      url = "github:NixOS/nixos-artwork";
      flake = false;
    };

    devenv-src.url = "github:cachix/devenv";

    emacs.url = "github:nix-community/emacs-overlay";
    # NOTE: unpackaged emacs extensions
    yasnippet-snippets = {
      url = "github:wiedzmin/yasnippet-snippets";
      flake = false;
    };
    emacs-combobulate = {
      url = "github:mickeynp/combobulate";
      flake = false;
    };
    emacs-consult-org-clock = {
      url = "github:overideal/consult-org-clock";
      flake = false;
    };
    emacs-epithet = {
      url = "github:oantolin/epithet";
      flake = false;
    };
    emacs-git-msg-prefix = {
      # NOTE: fixes deprecated/removed function in upstream (hardly ever be merged)
      url = "github:acastanedam/git-msg-prefix.el/update-cl";
      flake = false;
    };
    emacs-highlight-sexp = {
      url = "github:daimrod/highlight-sexp";
      flake = false;
    };
    emacs-just-ts-mode = {
      url = "github:leon-barrett/just-ts-mode.el";
      flake = false;
    };
    emacs-org-bars = {
      url = "github:tonyaldon/org-bars";
      flake = false;
    };
    emacs-password-menu = {
      url = "github:rnadler/password-menu";
      flake = false;
    };
    emacs-password-store-menu = {
      url = "github:rjekker/password-store-menu";
      flake = false;
    };
    emacs-project-headerline = {
      url = "github:gavv/project-headerline";
      flake = false;
    };
    emacs-treesit-jump = {
      # FIXME: switch to upstream after recent PR merging
      url = "github:abougouffa/treesit-jump/enhancements";
      flake = false;
    };
    emacs-vue-ts-mode = {
      url = "github:8uff3r/vue-ts-mode";
      flake = false;
    };
    emacs-jira-el = {
      url = "github:unmonoqueteclea/jira.el";
      flake = false;
    };
    emacs-float-narrow-indirect = {
      url = "github:yibie/float-narrow-indirect";
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
    pass-zsh-completion = {
      url = "github:ninrod/pass-zsh-completion";
      flake = false;
    };
    zsh-async = {
      url = "github:mafredri/zsh-async";
      flake = false;
    };
    zsh-fuzzy-search-and-edit = {
      url = "github:seletskiy/zsh-fuzzy-search-and-edit";
      flake = false;
    };
    zsh-reentry-hook = {
      url = "github:RobSis/zsh-reentry-hook";
      flake = false;
    };

    # NOTE: pinned
    unstable-future.url = "github:nixos/nixpkgs/a84ebe20c6bc2ecbcfb000a50776219f48d134cc";
    nixpkgs-libreoffice.url = "github:nixos/nixpkgs/0f213d0fee84280d8c3a97f7469b988d6fe5fcdf";
    nixpkgs-last-unbroken.url = "github:nixos/nixpkgs/0196c0175e9191c474c26ab5548db27ef5d34b05";
    nixpkgs-idafree-pinned.url = "github:nixos/nixpkgs/0196c0175e9191c474c26ab5548db27ef5d34b05";
    nixpkgs-qutebrowser-pinned.url = "github:nixos/nixpkgs/ee930f9755f58096ac6e8ca94a1887e0534e2d81";
  };

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
    rec {
      overlay = import ./overlay.nix inputs;

      nixosConfigurations = {
        laptoptop = lib.nixosSystem {
          inherit system;
          modules = [
            {
              nixpkgs.overlays = [
                overlays.unstable
                (_: old: {
                  i3lock-color = old.i3lock-color.overrideAttrs (_: rec {
                    version = "2.13.c.4";
                    src = old.fetchFromGitHub {
                      owner = "PandorasFox";
                      repo = "i3lock-color";
                      rev = version;
                      sha256 = "sha256-bbjkvgSKD57sdOtPYGLAKpQoIsJnF6s6ySq4dTWC3tI=";
                    };
                    patches = [ ./modules/workstation/lockscreen/patches/i3lock-color-pass-layout-switching.patch ];
                  });
                  mps-youtube = old.mps-youtube.overrideAttrs
                    (_: { patches = [ ./modules/content/media/patches/0001-fix-1134.patch ]; });
                  tabnine = old.tabnine.overrideAttrs (_: {
                    installPhase = ''
                      mkdir -p $out/bin
                      cp $src $out/bin/TabNine
                      chmod a+x $out/bin/TabNine
                    '';
                  });
                  moar = old.moar.overrideAttrs (_: {
                    postPatch = ''
                      sed -i "s,^\trussiaNotSupported(),,g" moar.go
                      cat moar.go
                    '';
                  });
                  vaapiIntel = old.vaapiIntel.override { enableHybridCodec = true; };
                  nyxt =
                    let
                      nyxt' = old.lispPackages.nyxt.overrideAttrs (_: {
                        patches = [
                          (old.fetchpatch {
                            url = "https://github.com/atlas-engineer/nyxt/commit/0786ee9fd80bf47827b6a9858895663db8498d12.patch";
                            sha256 = "sha256-lF5Ue6XhjfcqfXKpelUlVWlvl2Dq8IMNmCYZGYTdA38=";
                          })
                        ];
                      });
                    in
                    old.nyxt.overrideAttrs
                      (_: { src = nyxt'; });
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

      defaultPackage.x86_64-linux = nixosConfigurations.laptoptop.config.system.build.toplevel;
    };
}
