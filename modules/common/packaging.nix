{ config, lib, pkgs, ... }:
with import ../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.packaging;
  get-pr-override = pkgs.writeShellScriptBin "get-pr-override" ''
    PR_NO=$1
    HASH=$(curl -sL https://github.com/NixOS/nixpkgs/pull/''${PR_NO}.patch \
           | head -n 1 | grep -o -E -e "[0-9a-f]{40}")
    echo pr''${PR_NO} = "import (fetchTarball"
    echo "  \"\''${config.attributes.paths.nixpkgs.archive}''${HASH}.tar.gz\")"
    echo "    { config = config.nixpkgs.config; };"
  '';
  format-config = pkgs.writeShellScriptBin "format-config" ''
    sources=$(${pkgs.fd}/bin/fd -t file nix -E forges -E "*.gpg*" /etc/nixos)
    for file in "$sources"; do
      ${pkgs.nixfmt}/bin/nixfmt -w 120 $file
    done
  '';
  make-package-diff = pkgs.writeShellScriptBin "make-package-diff" ''
    PACKAGE=$1
    TMP=`${pkgs.coreutils}/bin/mktemp -d`
    cd $TMP
    {
        ${pkgs.nix}/bin/nix-shell "<nixpkgs>" -A $PACKAGE --run "unpackPhase"
        ${pkgs.coreutils}/bin/mv * a
        ${pkgs.coreutils}/bin/cp -r a b
        $EDITOR b
    } 2>&1 > /dev/null
    ${pkgs.diffutils}/bin/diff -u --suppress-common-lines a b
  '';
  emacsPackagingSetup = ''
    (use-package nix-mode
      :ensure t
      :mode (("\\.nix$" . nix-mode)
             ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-mode)))

    (use-package company-nixos-options
      :ensure t
      :disabled
      :config
      (add-to-list 'company-backends 'company-nixos-options))
  '';
in {
  options = {
    custom.packaging = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable packaging infra.";
      };
      nix.helpers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix helper tools.";
      };
      nix.srcfmt.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix source formatting tools.";
      };
      nix.importers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable tools to convert package definitions to Nix ones.";
      };
      nix.search.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix searching helper tools.";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable misc packaging tools.";
      };
      scripts.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom scripts.";
      };
      homeManagerBackups.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to try backing up existing files in the way of HM symlinks.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable packaging-related Emacs setup.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.homeManagerBackups.enable) {
      environment.variables.HOME_MANAGER_BACKUP_EXT = "hm_backup";
    })
    (mkIf (cfg.enable && cfg.nix.helpers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nix-prefetch
          nix-prefetch-github
          nix-prefetch-scripts
          nixos-generators
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.srcfmt.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nixfmt
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.importers.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nodePackages.node2nix
          pypi2nix
        ];
      };
    })
    (mkIf (cfg.enable && cfg.nix.search.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nix-index # TODO: maybe make easier shell alias
        ];
      };
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          cachix
          dotnet-sdk # for building some binary releases
          nix-zsh-completions
          nix-review # https://github.com/Mic92/nix-review
          make-package-diff
        ] ++ lib.optionals (config.attributes.staging.enable) [
          niv
        ];
      };
    })
    (mkIf (cfg.enable && cfg.scripts.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          get-pr-override
        ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          cachix
          dotnet-sdk # for building some binary releases
          nix-zsh-completions
          nix-review # https://github.com/Mic92/nix-review
        ];
        programs.emacs.extraPackages = epkgs: [
          epkgs.company-nixos-options
          epkgs.nix-mode
        ];
      };
      ide.emacs.config = ''${emacsPackagingSetup}'';
    })
  ];
}
