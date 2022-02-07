{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
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
        default = "${homePrefix user config.navigation.bookmarks.workspaces.globalRoot}/.devenv-backup";
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
        devenv = mkPythonScriptWithDeps pkgs "devenv"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.redis python3Packages.pyyaml renderizer stgit ])
          (builtins.readFile ./scripts/devenv.py);
        git-hideenv = pkgs.writeShellApplication {
          name = "git-hideenv";
          runtimeInputs = with pkgs; [ devenv ];
          text = "devenv --hide";
        };
        git-unhideenv = pkgs.writeShellApplication {
          name = "git-unhideenv";
          runtimeInputs = with pkgs; [ devenv ];
          text = "devenv --unhide";
        };
        git-exportenv = pkgs.writeShellApplication {
          name = "git-exportenv";
          runtimeInputs = with pkgs; [ devenv ];
          text = "devenv --export";
        };
        git-removeenv = pkgs.writeShellApplication {
          name = "git-removeenv";
          runtimeInputs = with pkgs; [ devenv ];
          text = "devenv --remove";
        };
      };

      home-manager.users."${user}" = {
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
        ${pkgs.redis}/bin/redis-cli set projectenv/root_markers ${
          strings.escapeNixString (builtins.toJSON config.dev.navigation.projects.rootMarkers)
        }
      '';

      dev.batchvcs.commands = { exportenv = [ "${pkgs.git-exportenv}/bin/git-exportenv" ]; };
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ devenv git-exportenv git-hideenv git-removeenv git-unhideenv ];
      };
    })
  ];
}
