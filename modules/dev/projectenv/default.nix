{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.projectenv;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    dev.projectenv = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable automation for project development environments population";
      };
      configName = mkOption {
        type = types.str;
        default = ".devenv";
        description = "File name for dev-env files list";
      };
      stashName = mkOption {
        type = types.str;
        default = "devenv";
        description = "git stash name for hidden devenv";
      };
      backupRoot = mkOption {
        type = types.str;
        default = "${homePrefix config.navigation.bookmarks.workspaces.globalRoot}/.devenv-backup";
        description = "File name for dev-env files list.";
      };
      markerFiles = mkOption {
        type = types.listOf types.str;
        default = [
          ".devenv"
          ".envrc"
          "flake.nix"
          "shell.nix"
        ];
        description = "Minimal list of files which presence at project root denotes existing devenv";
      };
      templates.entries = mkOption {
        type = types.attrs;
        default = { };
        description = "Collection of project templates paths";
      };
      templates.settings.common = mkOption {
        type = types.attrs;
        default = { };
        description = "Common part of settings collection to be used in template instantiation";
      };
      templates.settings.full = mkOption {
        type = types.attrs;
        default = { };
        description = "Collection of settings to be used in template instantiation";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        devenv = mkPythonScriptWithDeps "devenv"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.redis python3Packages.pyyaml renderizer stgit ])
          (readSubstituted ../../subst.nix ./scripts/devenv.py);
        git-hideenv = mkShellScriptWithDeps "git-hideenv" (with pkgs; [ devenv ]) ''
          devenv --hide
        '';
        git-unhideenv = mkShellScriptWithDeps "git-unhideenv" (with pkgs; [ devenv ]) ''
          devenv --unhide
        '';
        git-exportenv = mkShellScriptWithDeps "git-exportenv" (with pkgs; [ devenv ]) ''
          devenv --export
        '';
        git-removeenv = mkShellScriptWithDeps "git-removeenv" (with pkgs; [ devenv ]) ''
          devenv --remove
        '';
      };

      home-manager.users.${user} = {
        home.packages = with pkgs; [ devenv git-exportenv git-hideenv git-removeenv git-unhideenv ];

        home.activation.ensureDevEnvBackupRoot = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${cfg.backupRoot}";
        };
      };

      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set projectenv/templates ${
          strings.escapeNixString (builtins.toJSON cfg.templates.entries)
        }
        ${pkgs.redis}/bin/redis-cli set projectenv/settings ${
          strings.escapeNixString
          (builtins.toJSON (cfg.templates.settings.full // { "common" = cfg.templates.settings.common; }))
        }
      '';
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ devenv git-exportenv git-hideenv git-removeenv git-unhideenv ];
      };
    })
  ];
}
