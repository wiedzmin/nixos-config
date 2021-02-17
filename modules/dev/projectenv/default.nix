{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.projectenv;
  user = config.attributes.mainUser.name;
in {
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
        description = "File name for dev-env files list.";
      };
      backupRoot = mkOption {
        type = types.str;
        default = "${homePrefix config.navigation.bookmarks.workspaces.globalRoot}/.devenv-backup";
        description = "File name for dev-env files list.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        git-hideenv = mkShellScriptWithDeps "git-hideenv" (with pkgs; [ gitAndTools.git ])
          (readSubstituted ../../subst.nix ./scripts/git-hideenv.sh);
        git-unhideenv = mkShellScriptWithDeps "git-unhideenv" (with pkgs; [ gitAndTools.git ]) ''
          git stash pop $(git stash list --max-count=1 --grep="dev-env" | cut -f1 -d":")
        '';
        git-dumpenv = mkShellScriptWithDeps "git-dumpenv" (with pkgs; [ gitAndTools.git coreutils ])
          (readSubstituted ../../subst.nix ./scripts/git-dumpenv.sh);
        git-restoreenv = mkShellScriptWithDeps "git-restoreenv" (with pkgs; [ gitAndTools.git coreutils git-hideenv ])
          (readSubstituted ../../subst.nix ./scripts/git-restoreenv.sh);
      };

      home-manager.users.${user} = {
        home.packages = with pkgs; [ git-dumpenv git-hideenv git-restoreenv git-unhideenv renderizer stgit ];

        home.activation.ensureDevEnvBackupRoot = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${cfg.backupRoot}";
        };
      };
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ git-dumpenv git-hideenv git-restoreenv git-unhideenv ];
      };
    })
  ];
}
