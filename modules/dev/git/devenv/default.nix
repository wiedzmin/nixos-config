{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.git.devenv;
  user = config.attributes.mainUser.name;
in {
  options = {
    dev.git.devenv = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git VCS infrastructure.";
      };
      devEnv.configName = mkOption {
        type = types.str;
        default = ".devenv";
        description = "File name for dev-env files list.";
      };
      devEnv.backupRoot = mkOption {
        type = types.str;
        default = "${homePrefix config.navigation.bookmarks.workspaces.globalRoot}/.devenv-backup";
        description = "File name for dev-env files list.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs git-related setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        git-hideenv = mkShellScriptWithDeps "git-hideenv" (with pkgs; [ gitAndTools.git ]) ''
          set -e

          devenv_data=$(<${cfg.devEnv.configName})
          devenv_data_filtered=$(echo "$devenv_data" | xargs -d '\n' find 2>/dev/null)
          devenv_filelist=$(echo "$devenv_data_filtered" | tr '\n' ' ')

          git reset # clean up index from unrelated staged changes
          git add -- $devenv_filelist
          git stash -m "dev-env" -- $devenv_filelist
        '';
        git-unhideenv = mkShellScriptWithDeps "git-unhideenv" (with pkgs; [ gitAndTools.git ]) ''
          git stash pop $(git stash list --max-count=1 --grep="dev-env" | cut -f1 -d":")
        '';
        git-dumpenv = mkShellScriptWithDeps "git-dumpenv" (with pkgs; [ gitAndTools.git coreutils ]) ''
          set -e

          devenv_data=$(<${cfg.devEnv.configName})
          devenv_data_filtered=$(echo "$devenv_data" | xargs -d '\n' find 2>/dev/null)
          devenv_filelist=$(echo "$devenv_data_filtered" | tr '\n' ' ')
          git_root=$(git rev-parse --show-toplevel)
          ts_suffix=$(date ${config.attributes.dateFormats.commonShellNoColons})
          dump_dir=$(basename $(dirname "$git_root"))_$(basename "$git_root")_$ts_suffix

          mkdir -p ${cfg.devEnv.backupRoot}/$dump_dir
          cp -t ${cfg.devEnv.backupRoot}/$dump_dir $devenv_filelist
        '';
        git-restoreenv = mkShellScriptWithDeps "git-restoreenv" (with pkgs; [ gitAndTools.git coreutils git-hideenv ]) ''
          git_root=$(git rev-parse --show-toplevel)
          envs_list=$(ls -a ${cfg.devEnv.backupRoot} | grep $(basename $(dirname "$git_root"))_$(basename "$git_root") | sort -n -r)
          envs_count=$(echo "$envs_list" | wc -l)
          tip_env=$(echo "$envs_list" | head -n 1)
          remainder_envs=$(echo "$envs_list" | tail -n +2)

          if [[ "$@" =~ "--tidy" ]]; then
            for dev_env in $remainder_envs
            do
              rm -rf "${cfg.devEnv.backupRoot}/$dev_env"
            done
          fi

          if [[ ! "$@" =~ "--keep-devenv" ]]; then
            if [ "$envs_count" -gt 1 ]; then
              rm -rf ${cfg.devEnv.backupRoot}/$tip_env
            fi
          fi

          git-hideenv
          git stash drop $(git stash list --max-count=1 --grep="dev-env" | cut -f1 -d":")
          cp -a ${cfg.devEnv.backupRoot}/$tip_env/. .

          devenv_data=$(<${cfg.devEnv.configName})
          devenv_data_filtered=$(echo "$devenv_data" | xargs -d '\n' find 2>/dev/null)
          devenv_filelist=$(echo "$devenv_data_filtered" | tr '\n' ' ')
          git reset # clean up index from unrelated staged changes
          git add -- $devenv_filelist
        '';
      };
      home-manager.users.${user} = {
        home.packages = with pkgs; [ git-dumpenv git-hideenv git-restoreenv git-unhideenv ];
        home.activation.ensureDevEnvBackupRoot = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${cfg.devEnv.backupRoot}";
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.config = readSubstituted ../../../subst.nix ./emacs/git.el;
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ git-dumpenv git-hideenv git-restoreenv git-unhideenv ]; };
    })
  ];
}
